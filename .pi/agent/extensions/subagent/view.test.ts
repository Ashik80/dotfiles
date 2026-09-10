import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { after, before, test } from "node:test";

const cacheRoot = fs.mkdtempSync(path.join(os.tmpdir(), "pi-subagent-view-test-"));
process.env.XDG_CACHE_HOME = cacheRoot;

const { SubagentView, cleanupViewSession } = await import("./view.ts");

before(() => {
	process.env.XDG_CACHE_HOME = cacheRoot;
});

after(() => {
	cleanupViewSession();
	fs.rmSync(cacheRoot, { recursive: true, force: true });
});

test("mirrors cumulative tool updates without duplicating prior output", () => {
	const view = new SubagentView({ agent: "worker", task: "inspect output", mode: "single" });
	view.onToolStart("bash", { command: "printf output" });
	view.onToolUpdate({ content: [{ type: "text", text: "output" }] });
	view.onToolUpdate({ content: [{ type: "text", text: "output line" }] });
	view.onToolEnd("bash", false);
	view.finalize("done");

	const transcript = fs.readFileSync(view.transcriptPath, "utf-8");
	const lines = transcript.split("\n");
	assert.equal(lines.filter((line) => line === "output").length, 0);
	assert.equal(lines.filter((line) => line === "output line").length, 1);
	assert.match(transcript, /→ bash: printf output/);
	assert.match(transcript, /✓ bash: printf output \([0-9.]+s\)/);
	assert.match(transcript, /\[done — C-c to close\]/);

	const registry = JSON.parse(fs.readFileSync(path.join(view.workDir, "registry.json"), "utf-8"));
	assert.equal(registry[view.key].status, "done");
	assert.equal(registry[view.key].transcript, view.transcriptPath);
});
