import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { after, before, beforeEach, afterEach, test } from "node:test";
import { spawnSync } from "node:child_process";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const cacheRoot = fs.mkdtempSync(path.join(os.tmpdir(), "pi-subagent-view-test-"));
process.env.XDG_CACHE_HOME = cacheRoot;

const viewModule = await import("./view.ts");
const { SubagentView, cleanupViewSession } = viewModule;
const openRunningSubagentView = (viewModule as {
	openRunningSubagentView?: (
		pi: Pick<ExtensionAPI, "exec">,
		ctx: Pick<ExtensionContext, "mode"> & { ui: Pick<ExtensionContext["ui"], "select" | "notify"> },
	) => Promise<void>;
}).openRunningSubagentView;
const savedEnv = { TMUX: process.env.TMUX, TMUX_PANE: process.env.TMUX_PANE, PI_SUBAGENT_VIEW: process.env.PI_SUBAGENT_VIEW };

beforeEach(() => {
	cleanupViewSession();
	process.env.TMUX = "/tmp/tmux-test,1,0";
	process.env.TMUX_PANE = "%17";
	delete process.env.PI_SUBAGENT_VIEW;
});

afterEach(() => {
	cleanupViewSession();
	for (const [key, value] of Object.entries(savedEnv)) {
		if (value === undefined) delete process.env[key];
		else process.env[key] = value;
	}
});

function picker() {
	const selections: string[][] = [];
	const notifications: string[] = [];
	const commands: { command: string; args: string[] }[] = [];
	const ctx = {
		mode: "tui" as ExtensionContext["mode"],
		ui: {
			select: async (_title: string, options: string[]): Promise<string | undefined> => {
				selections.push(options);
				return options[0];
			},
			notify: (message: string) => { notifications.push(message); },
		},
	};
	const pi = {
		exec: async (command: string, args: string[]) => {
			commands.push({ command, args });
			return { code: 0, stdout: "", stderr: "", killed: false };
		},
	};
	const open = async () => {
		assert.equal(typeof openRunningSubagentView, "function");
		await openRunningSubagentView!(pi, ctx);
	};
	return { ctx, pi, selections, notifications, commands, open };
}

function running(agent = "worker") {
	const view = new SubagentView({ agent, task: "inspect output", mode: "single" });
	// The picker must also work before the first batched transcript flush.
	return view;
}

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

test("picks only running agents from this parent and targets its original pane", async () => {
	const done = running("scout");
	done.finalize("done");
	const worker = running();
	const secondWorker = running();
	const otherSession = path.join(path.dirname(worker.workDir), "session-other");
	fs.mkdirSync(otherSession);
	fs.writeFileSync(path.join(otherSession, "registry.json"), JSON.stringify({ foreign: { status: "running" } }));
	const p = picker();
	p.ctx.ui.select = async (_title, options) => {
		p.selections.push(options);
		process.env.TMUX_PANE = "%99";
		return options[1];
	};
	await p.open();
	assert.deepEqual(p.selections, [[worker.key, secondWorker.key]]);
	assert.equal(p.commands.length, 1);
	assert.equal(p.commands[0].command, "tmux");
	assert.deepEqual(p.commands[0].args.slice(0, -1), ["split-window", "-h", "-t", "%17"]);
	assert.match(p.commands[0].args.at(-1)!, /^exec tail -n \+1 -F -- /);
	assert.ok(p.commands[0].args.at(-1)!.includes(secondWorker.transcriptPath));
	assert.equal(worker.status, "running");
	assert.equal(secondWorker.status, "running");
});

test("quotes transcript paths without letting the pane shell execute their contents", async () => {
	const view = running("worker'$(touch injected); space");
	const p = picker();
	await p.open();
	const shellCommand = p.commands[0].args.at(-1)!;
	// Replace tail with argv printing to check the actual shell parse, not a quoting pattern.
	const parsed = spawnSync("sh", ["-c", shellCommand.replace(/^exec tail /, "printf '%s\\n' ")], { encoding: "utf-8", cwd: cacheRoot });
	assert.equal(parsed.status, 0);
	assert.deepEqual(parsed.stdout.trimEnd().split("\n"), ["-n", "+1", "-F", "--", view.transcriptPath]);
	assert.equal(fs.existsSync(path.join(cacheRoot, "injected")), false);
});

for (const variable of ["TMUX", "TMUX_PANE"]) {
	test(`does not open a picker outside tmux (${variable} missing)`, async () => {
		delete process.env[variable];
		const p = picker();
		await p.open();
		assert.deepEqual(p.selections, []);
		assert.deepEqual(p.commands, []);
		assert.match(p.notifications[0], /tmux/i);
	});
}

test("disabled mirroring does not create files or open a picker", async () => {
	process.env.PI_SUBAGENT_VIEW = "0";
	const p = picker();
	await p.open();
	assert.deepEqual(p.selections, []);
	assert.deepEqual(p.commands, []);
	assert.match(p.notifications[0], /disabled/i);
	assert.equal(fs.existsSync(path.join(cacheRoot, "pi", "subagent-views", `session-${process.pid}`)), false);
});

test("no running agents does not create files or open a picker", async () => {
	const p = picker();
	await p.open();
	assert.deepEqual(p.selections, []);
	assert.deepEqual(p.commands, []);
	assert.match(p.notifications[0], /no running/i);
	assert.equal(fs.existsSync(path.join(cacheRoot, "pi", "subagent-views", `session-${process.pid}`)), false);
	const done = running();
	done.finalize("done");
	await p.open();
	assert.deepEqual(p.selections, []);
});

test("canceling the picker does nothing", async () => {
	const view = running();
	const p = picker();
	p.ctx.ui.select = async () => undefined;
	await p.open();
	assert.deepEqual(p.commands, []);
	assert.deepEqual(p.notifications, []);
	assert.equal(view.status, "running");
});

test("ignores non-terminal modes", async () => {
	for (const mode of ["rpc", "json", "print"] as const) {
		const p = picker();
		p.ctx.mode = mode;
		await p.open();
		assert.deepEqual(p.selections, []);
		assert.deepEqual(p.commands, []);
		assert.deepEqual(p.notifications, []);
	}
});

test("missing, malformed and out-of-session registry entries are safe", async () => {
	const view = running();
	const registryPath = path.join(view.workDir, "registry.json");
	for (const content of ["{", "null", "[]", JSON.stringify({ bad: null, foreign: { status: "running", transcript: "/tmp/foreign.log" } })]) {
		fs.writeFileSync(registryPath, content);
		const p = picker();
		await p.open();
		assert.deepEqual(p.selections, []);
		assert.deepEqual(p.commands, []);
	}
	fs.unlinkSync(registryPath);
	await picker().open();
});

test("reports tmux nonzero exits and spawn failures without changing the agent", async () => {
	const view = running();
	for (const throws of [false, true]) {
		const p = picker();
		p.pi.exec = async () => {
			if (throws) throw new Error("tmux unavailable");
			return { code: 1, stdout: "", stderr: "pane too small", killed: false };
		};
		await p.open();
		assert.match(p.notifications[0], /could not open/i);
		assert.equal(view.status, "running");
	}
});
