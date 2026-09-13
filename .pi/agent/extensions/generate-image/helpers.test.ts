import assert from "node:assert/strict";
import { test } from "node:test";

const helpers = await import("./helpers.ts").catch(() => ({} as Record<string, unknown>));

test("routes difficult images to Nano Banana Pro", () => {
	assert.equal(typeof helpers.selectImageModel, "function");
	assert.equal(
		(helpers.selectImageModel as (prompt: string, difficulty: string, preference: string) => string)(
			"A cinematic scene with several characters and exact typography",
			"difficult",
			"auto",
		),
		"google/gemini-3-pro-image",
	);
});

test("routes cheap typography work to Qwen Image 3", () => {
	assert.equal(typeof helpers.selectImageModel, "function");
	assert.equal(
		(helpers.selectImageModel as (prompt: string, difficulty: string, preference: string) => string)(
			"Create a poster with the headline Launch Day",
			"simple",
			"auto",
		),
		"qwen/qwen-image-3",
	);
});

test("routes other cheap work to Seedream 4.5", () => {
	assert.equal(typeof helpers.selectImageModel, "function");
	assert.equal(
		(helpers.selectImageModel as (prompt: string, difficulty: string, preference: string) => string)(
			"A watercolor landscape at sunrise",
			"simple",
			"auto",
		),
		"bytedance-seed/seedream-4.5",
	);
});

test("maps image MIME types to file extensions", () => {
	assert.equal(typeof helpers.extensionForMimeType, "function");
	const extensionForMimeType = helpers.extensionForMimeType as (mimeType: string) => string;
	assert.equal(extensionForMimeType("image/png"), ".png");
	assert.equal(extensionForMimeType("image/jpeg"), ".jpg");
	assert.equal(extensionForMimeType("image/webp"), ".webp");
});

test("rejects non-image output extensions", () => {
	assert.equal(typeof helpers.assertSafeImagePath, "function");
	const assertSafeImagePath = helpers.assertSafeImagePath as (path: string) => void;
	assert.throws(() => assertSafeImagePath(".env"), /image file extension/i);
	assert.throws(() => assertSafeImagePath(".env.png"), /protected path/i);
	assert.throws(() => assertSafeImagePath("config.json"), /image file extension/i);
	assert.doesNotThrow(() => assertSafeImagePath("art/output.png"));
});
