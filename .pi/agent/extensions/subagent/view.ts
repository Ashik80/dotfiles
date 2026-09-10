/**
 * Subagent View — live per-agent activity mirroring.
 *
 * Forked from the pi-coding-agent subagent extension. Adds a per-subagent
 * append-only transcript file plus a JSON registry so the user can open a
 * live view of each spawned subagent (typically from Emacs, e.g. a ghostel
 * buffer tailing the transcript) while the agent is running.
 *
 * Environment controls:
 *   PI_SUBAGENT_VIEW=0   disable mirroring entirely (no files written)
 *   XDG_CACHE_HOME       override root (defaults to ~/.cache)
 *
 * Files live under <cache>/pi/subagent-views/session-<pid>/ and are removed
 * on session shutdown (see `cleanupViewSession`).
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export type ViewStatus = "running" | "done" | "aborted" | "error";

export interface SubagentViewSpec {
	agent: string;
	task: string;
	mode: "single" | "parallel" | "chain";
	step?: number;
	chainTotal?: number;
}

const FLUSH_MS = 300; // batch append writes
const MAX_TOOL_OUTPUT_BYTES = 64 * 1024; // cap per-tool partial mirror
const MAX_TOOL_OUTPUT_CHUNK = 2000; // cap per-update chunk

let _sessionDir: string | undefined;
let _seq = 0;

export function viewsEnabled(): boolean {
	return process.env.PI_SUBAGENT_VIEW !== "0";
}

export function sessionDir(): string {
	if (_sessionDir) return _sessionDir;
	const base = path.join(
		process.env.XDG_CACHE_HOME || path.join(os.homedir(), ".cache"),
		"pi",
		"subagent-views",
	);
	_sessionDir = path.join(base, `session-${process.pid}`);
	fs.mkdirSync(_sessionDir, { recursive: true, mode: 0o700 });
	return _sessionDir;
}

export function cleanupViewSession(): void {
	if (!_sessionDir) return;
	try {
		fs.rmSync(_sessionDir, { recursive: true, force: true });
	} catch {
		/* best-effort */
	}
	_sessionDir = undefined;
	_seq = 0;
}

/** Read-time helpers used by index.ts */
export function viewKey(agent: string): string {
	_seq++;
	return `${agent}-${_seq}`;
}

function esc(s: string): string {
	return s.replace(/\x1b\[[0-9;]*[A-Za-z]/g, "").replace(/\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)/g, "");
}

function truncate(s: string, max: number): string {
	return s.length <= max ? s : s.slice(0, max) + "…";
}

/** Compact plain-text rendering of a tool call (theme-free; for file transcripts). */
export function formatCallPlain(toolName: string, args: Record<string, unknown>): string {
	const shorten = (p: string) => p.replace(/^\/home\/[^/]+/, "~");
	switch (toolName) {
		case "bash": {
			const command = (args.command as string) || "";
			return truncate(esc(`bash: ${command}`), 160);
		}
		case "read":
		case "write":
		case "edit":
		case "ls":
		case "find":
		case "grep": {
			const raw = (args.file_path || args.path || args.pattern || "") as string;
			const target = shorten(String(raw));
			const pattern = (args.pattern as string) || "";
			if (toolName === "grep") return truncate(esc(`grep /${pattern}/ in ${target}`), 160);
			if (toolName === "find") return truncate(esc(`find ${pattern} in ${target}`), 160);
			return truncate(esc(`${toolName} ${target}`), 160);
		}
		default: {
			const preview = truncate(JSON.stringify(args), 160);
			return `${toolName} ${preview}`;
		}
	}
}

export class SubagentView {
	readonly key: string;
	readonly transcriptPath: string;
	readonly workDir: string;
	status: ViewStatus = "running";
	startedAt = Date.now();

	private toolOutputBytes = 0;
	private pendingBuf = "";
	private flushTimer: ReturnType<typeof setTimeout> | undefined;
	private currentTool: string | undefined;
	private currentToolArgs: Record<string, unknown> = {};
	private currentToolStartedAt: number | undefined;
	private currentToolOutput = "";

	constructor(spec: SubagentViewSpec) {
		this.key = viewKey(spec.agent);
		this.workDir = sessionDir();
		this.transcriptPath = path.join(this.workDir, `${this.key}.log`);

		const now = new Date().toISOString();
		const modeInfo =
			spec.mode === "chain"
				? `mode=chain step=${spec.step ?? "?"}/${spec.chainTotal ?? "?"}`
				: `mode=${spec.mode}`;
		this.append(
			`=== subagent ${this.key} ===\n` +
				`agent: ${spec.agent}\n` +
				`task: ${truncate(esc(spec.task), 400)}\n` +
				`${modeInfo}\n` +
				`started: ${now}\n`,
		);
		this.writeRegistry();
	}

	onToolStart(toolName: string, args: Record<string, unknown>): void {
		this.currentTool = toolName;
		this.currentToolArgs = args;
		this.currentToolStartedAt = Date.now();
		this.currentToolOutput = "";
		this.toolOutputBytes = 0;
		this.append(`→ ${formatCallPlain(toolName, args)}\n`);
	}

	onToolUpdate(partialResult: unknown): void {
		if (this.toolOutputBytes >= MAX_TOOL_OUTPUT_BYTES) return;
		let s = "";
		if (typeof partialResult === "string") s = partialResult;
		else if (partialResult && typeof partialResult === "object") {
			const content = (partialResult as { content?: string | unknown[] }).content;
			if (typeof content === "string") s = content;
			else if (Array.isArray(content)) {
				s = content
					.map((c) => (typeof c === "object" && c && (c as { text?: string }).text) || "")
					.join("");
			}
		}
		s = esc(s.replace(/\r/g, ""));
		if (!s) return;

		// Pi's tool_execution_update payload is cumulative. Mirror only the
		// suffix added since the previous update to avoid repeated output.
		const delta = s.startsWith(this.currentToolOutput) ? s.slice(this.currentToolOutput.length) : s;
		this.currentToolOutput = s;
		if (!delta) return;
		const remaining = MAX_TOOL_OUTPUT_BYTES - this.toolOutputBytes;
		const chunk = truncate(delta, Math.min(MAX_TOOL_OUTPUT_CHUNK, remaining));
		this.toolOutputBytes += chunk.length;
		this.append(chunk);
	}

	onToolEnd(toolName: string, isError: boolean): void {
		const elapsed = this.currentToolStartedAt
			? `${((Date.now() - this.currentToolStartedAt) / 1000).toFixed(1)}s`
			: "";
		if (this.currentToolOutput && !this.currentToolOutput.endsWith("\n")) this.append("\n");
		this.append(`${isError ? "✗" : "✓"} ${formatCallPlain(toolName, this.currentToolArgs)}${elapsed ? ` (${elapsed})` : ""}\n`);
		this.currentTool = undefined;
		this.currentToolArgs = {};
		this.currentToolStartedAt = undefined;
		this.currentToolOutput = "";
	}

	onTextDelta(delta: string): void {
		if (!delta) return;
		this.append(esc(delta));
	}

	finalize(status: ViewStatus): void {
		if (this.status !== "running") return;
		this.status = status;
		this.append(`[${status} — C-c to close]\n`);
		this.writeRegistry();
		this.flush(true);
	}

	private append(text: string): void {
		this.pendingBuf += text;
		if (this.flushTimer === undefined) {
			this.flushTimer = setTimeout(() => this.flush(false), FLUSH_MS);
		}
		// Flush promptly if a final marker was written.
		if (text.startsWith("[")) this.flush(true);
	}

	private flush(sync: boolean): void {
		if (this.flushTimer !== undefined) {
			clearTimeout(this.flushTimer);
			this.flushTimer = undefined;
		}
		if (!this.pendingBuf) return;
		const data = this.pendingBuf;
		this.pendingBuf = "";
		if (sync) {
			const fd = fs.openSync(this.transcriptPath, "a", 0o600);
			try {
				fs.writeSync(fd, data);
			} finally {
				fs.closeSync(fd);
			}
		} else {
			try {
				fs.appendFileSync(this.transcriptPath, data, { encoding: "utf-8", mode: 0o600 });
			} catch {
				/* best-effort */
			}
		}
	}

	private writeRegistry(): void {
		let registry: Record<string, Record<string, unknown>> = {};
		try {
			const raw = fs.readFileSync(path.join(this.workDir, "registry.json"), "utf-8");
			if (raw) registry = JSON.parse(raw) as Record<string, Record<string, unknown>>;
		} catch {
			/* fresh */
		}
		registry[this.key] = {
			agent: this.key.slice(0, this.key.lastIndexOf("-")),
			transcript: this.transcriptPath,
			status: this.status,
			startedAt: this.startedAt,
		};
		try {
			fs.writeFileSync(path.join(this.workDir, "registry.json"), JSON.stringify(registry, null, 2), {
				encoding: "utf-8",
				mode: 0o600,
			});
		} catch {
			/* best-effort */
		}
	}
}