import { mkdir, writeFile } from "node:fs/promises";
import { dirname, extname, resolve } from "node:path";
import { builtinImagesModels } from "@earendil-works/pi-ai/providers/all";
import { StringEnum } from "@earendil-works/pi-ai";
import { type ExtensionAPI, withFileMutationQueue } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import {
	assertSafeImagePath,
	extensionForMimeType,
	selectImageModel,
	type ImageDifficulty,
	type ImageModelPreference,
} from "./helpers.ts";

const imageModels = builtinImagesModels();

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "generate_image",
		label: "Generate Image",
		description:
			"Generate and save an image through OpenRouter. The harness routes simple photorealistic/art prompts to Seedream 4.5, simple typography/design prompts to Qwen Image 3, and difficult precision/composition/editing prompts to Nano Banana Pro. Set difficulty or model only when the automatic choice is unsuitable.",
		promptSnippet: "Generate images with automatic cost/quality model routing",
		promptGuidelines: [
			"Use generate_image when the user asks to create or edit an image; prefer automatic routing unless the user requests a specific image model or quality tier.",
		],
		parameters: Type.Object({
			prompt: Type.String({ description: "Detailed image-generation prompt" }),
			difficulty: Type.Optional(
				StringEnum(["auto", "simple", "difficult"] as const, {
					description: "Quality tier. Auto infers from the prompt; default: auto.",
				}),
			),
			model: Type.Optional(
				StringEnum(["auto", "seedream", "qwen", "nano-banana-pro"] as const, {
					description: "Explicit model preference; default: auto.",
				}),
			),
			outputPath: Type.Optional(
				Type.String({ description: "Output path, relative to the working directory unless absolute" }),
			),
		}),
		async execute(_toolCallId, params, signal, onUpdate, ctx) {
			const difficulty = (params.difficulty ?? "auto") as ImageDifficulty;
			const preference = (params.model ?? "auto") as ImageModelPreference;
			const modelId = selectImageModel(params.prompt, difficulty, preference);
			const catalogModel = imageModels.getModel("openrouter", modelId);
			if (!catalogModel) throw new Error(`OpenRouter image model not found: ${modelId}`);

			const resolvedAuth = await ctx.modelRegistry.getProviderAuth("openrouter");
			if (!resolvedAuth?.auth.apiKey) {
				throw new Error('OpenRouter authentication is required. Run "/login" and select OpenRouter.');
			}

			onUpdate?.({
				content: [{ type: "text", text: `Generating with ${catalogModel.name}...` }],
				details: { model: modelId },
			});

			const requestModel = resolvedAuth.auth.baseUrl
				? { ...catalogModel, baseUrl: resolvedAuth.auth.baseUrl }
				: catalogModel;
			const result = await imageModels.generateImages(
				requestModel,
				{ input: [{ type: "text", text: params.prompt }] },
				{
					apiKey: resolvedAuth.auth.apiKey,
					headers: resolvedAuth.auth.headers,
					env: resolvedAuth.env,
					signal,
				},
			);
			if (result.stopReason !== "stop") {
				throw new Error(result.errorMessage ?? `Image generation ${result.stopReason}`);
			}

			const image = result.output.find((block) => block.type === "image");
			if (!image || image.type !== "image") throw new Error("The image model returned no image.");

			const requestedPath = (params.outputPath ?? `generated-images/image-${Date.now()}`).replace(/^@/, "");
			const pathWithExtension = extname(requestedPath) ? requestedPath : requestedPath + extensionForMimeType(image.mimeType);
			assertSafeImagePath(pathWithExtension);
			const absolutePath = resolve(ctx.cwd, pathWithExtension);
			await withFileMutationQueue(absolutePath, async () => {
				await mkdir(dirname(absolutePath), { recursive: true });
				await writeFile(absolutePath, Buffer.from(image.data, "base64"));
			});

			return {
				content: [
					{ type: "text", text: `Generated with ${catalogModel.name} and saved to ${absolutePath}` },
					image,
				],
				details: { model: modelId, path: absolutePath, mimeType: image.mimeType },
				usage: result.usage,
			};
		},
	});
}
