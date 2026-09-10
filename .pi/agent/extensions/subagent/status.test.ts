import assert from "node:assert/strict";
import { test } from "node:test";

const statusModule = await import("./status.ts").catch(() => ({}));
const ActiveSubagentStatus = (statusModule as {
	ActiveSubagentStatus?: new () => {
		increment(callId: string): string;
		decrement(callId: string): string | undefined;
		clear(callId: string): string | undefined;
		reset(): undefined;
	};
}).ActiveSubagentStatus;

test("reports the aggregate number of active subagents", () => {
	assert.equal(typeof ActiveSubagentStatus, "function");
	const status = new ActiveSubagentStatus!();

	assert.equal(status.increment("call-a"), "subagents: 1 running");
	assert.equal(status.increment("call-a"), "subagents: 2 running");
	assert.equal(status.increment("call-b"), "subagents: 3 running");
	assert.equal(status.decrement("call-a"), "subagents: 2 running");
	assert.equal(status.decrement("call-b"), "subagents: 1 running");
	assert.equal(status.decrement("call-a"), undefined);
});

test("clears one invocation without affecting concurrent invocations", () => {
	assert.equal(typeof ActiveSubagentStatus, "function");
	const status = new ActiveSubagentStatus!();

	status.increment("call-a");
	status.increment("call-a");
	status.increment("call-b");
	assert.equal(status.clear("call-a"), "subagents: 1 running");
	assert.equal(status.clear("call-b"), undefined);
});

test("reset clears all active subagents", () => {
	assert.equal(typeof ActiveSubagentStatus, "function");
	const status = new ActiveSubagentStatus!();

	status.increment("call-a");
	status.increment("call-b");
	assert.equal(status.reset(), undefined);
});
