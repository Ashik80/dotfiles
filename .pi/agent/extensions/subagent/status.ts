export const SUBAGENT_STATUS_KEY = "subagents";

export class ActiveSubagentStatus {
	private readonly counts = new Map<string, number>();

	increment(callId: string): string {
		this.counts.set(callId, (this.counts.get(callId) ?? 0) + 1);
		return this.label()!;
	}

	decrement(callId: string): string | undefined {
		const count = this.counts.get(callId) ?? 0;
		if (count <= 1) this.counts.delete(callId);
		else this.counts.set(callId, count - 1);
		return this.label();
	}

	clear(callId: string): string | undefined {
		this.counts.delete(callId);
		return this.label();
	}

	reset(): undefined {
		this.counts.clear();
		return undefined;
	}

	private label(): string | undefined {
		let total = 0;
		for (const count of this.counts.values()) total += count;
		return total > 0 ? `subagents: ${total} running` : undefined;
	}
}
