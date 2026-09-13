export type ImageDifficulty = "auto" | "simple" | "difficult";
export type ImageModelPreference = "auto" | "seedream" | "qwen" | "nano-banana-pro";

export const IMAGE_MODELS = {
	seedream: "bytedance-seed/seedream-4.5",
	qwen: "qwen/qwen-image-3",
	"nano-banana-pro": "google/gemini-3-pro-image",
} as const;

const DIFFICULT_IMAGE_PATTERN =
	/\b(complex|difficult|precise|exact|consistent|multi[- ]?(?:character|panel|scene)|infographic|diagram|edit|inpaint|reference image)\b/i;
const TYPOGRAPHY_PATTERN = /\b(text|typography|headline|caption|label|logo|poster|sign|menu|flyer|banner|ui|interface)\b/i;

export function selectImageModel(
	prompt: string,
	difficulty: ImageDifficulty,
	preference: ImageModelPreference,
): string {
	if (preference !== "auto") return IMAGE_MODELS[preference];

	const resolvedDifficulty =
		difficulty === "auto" ? (DIFFICULT_IMAGE_PATTERN.test(prompt) ? "difficult" : "simple") : difficulty;
	if (resolvedDifficulty === "difficult") return IMAGE_MODELS["nano-banana-pro"];
	return TYPOGRAPHY_PATTERN.test(prompt) ? IMAGE_MODELS.qwen : IMAGE_MODELS.seedream;
}

export function extensionForMimeType(mimeType: string): string {
	switch (mimeType.toLowerCase()) {
		case "image/jpeg":
			return ".jpg";
		case "image/webp":
			return ".webp";
		case "image/gif":
			return ".gif";
		default:
			return ".png";
	}
}

export function assertSafeImagePath(path: string): void {
	const normalized = path.replaceAll("\\", "/");
	const extension = normalized.match(/(\.[^./]+)$/)?.[1]?.toLowerCase();
	if (!extension || ![".png", ".jpg", ".jpeg", ".webp", ".gif"].includes(extension)) {
		throw new Error("Output path must use an image file extension (.png, .jpg, .jpeg, .webp, or .gif).");
	}
	if (/(?:^|\/)\.env(?:\.|\/|$)/i.test(normalized)) {
		throw new Error("Refusing to write an image to a protected path.");
	}
}
