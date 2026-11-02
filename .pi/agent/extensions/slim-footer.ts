/**
 * Slim Footer Extension
 *
 * Replaces the default footer, hiding the total input tokens (↑), total output
 * tokens (↓), total cache reads (R), and the "(auto)" auto-compaction marker.
 * Everything else (cache write, cache-hit rate, cost, context %, model, session
 * path, extension statuses) is kept identical to the built-in footer.
 */

import { isAbsolute, relative, resolve, sep } from "node:path";
import type { AssistantMessage, ToolResultMessage } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

function formatTokens(count: number): string {
	if (count < 1000) return count.toString();
	if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
	if (count < 1000000) return `${Math.round(count / 1000)}k`;
	if (count < 10000000) return `${(count / 1000000).toFixed(1)}M`;
	return `${Math.round(count / 1000000)}M`;
}

function formatCwdForFooter(cwd: string, home: string | undefined): string {
	if (!home) return cwd;
	const rel = relative(resolve(home), resolve(cwd));
	const isInsideHome = rel === "" || (rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel));
	if (!isInsideHome) return cwd;
	return rel === "" ? "~" : `~${sep}${rel}`;
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		if (!ctx.hasUI) return;

		ctx.ui.setFooter((_tui, theme, footerData) => {
			const unsub = footerData.onBranchChange(() => _tui.requestRender());
			return {
				dispose: unsub,
				invalidate() {},
				render(width: number): string[] {
					// Totals over the whole session; cacheRead tracked for the CH% condition but never displayed
					let cacheRead = 0,
						cacheWrite = 0,
						cost = 0,
						hitRate: number | undefined;
					for (const entry of ctx.sessionManager.getEntries()) {
						if (entry.type === "usage") {
							cacheRead += entry.usage.cacheRead;
							cacheWrite += entry.usage.cacheWrite;
							cost += entry.usage.cost?.total ?? 0;
						} else if (entry.type === "message" && entry.message.role === "assistant") {
							const u = (entry.message as AssistantMessage).usage;
							cacheRead += u.cacheRead;
							cacheWrite += u.cacheWrite;
							cost += u.cost?.total ?? 0;
							const prompt = u.input + u.cacheRead + u.cacheWrite;
							if (prompt > 0) hitRate = (u.cacheRead / prompt) * 100;
						} else if (entry.type === "message" && entry.message.role === "toolResult" && (entry.message as ToolResultMessage).usage) {
							const u = (entry.message as ToolResultMessage).usage!;
							cacheRead += u.cacheRead;
							cacheWrite += u.cacheWrite;
							cost += u.cost?.total ?? 0;
						} else if ((entry.type === "branch_summary" || entry.type === "compaction") && entry.usage) {
							cacheRead += entry.usage.cacheRead;
							cacheWrite += entry.usage.cacheWrite;
							cost += entry.usage.cost?.total ?? 0;
						}
					}

					// Context usage — "(auto)" marker intentionally omitted
					const usage = ctx.getContextUsage();
					const contextWindow = usage?.contextWindow ?? ctx.model?.contextWindow ?? 0;
					const pctValue = usage?.percent ?? 0;
					const pct = usage && usage.percent !== null ? pctValue.toFixed(1) : "?";
					const contextStr =
						pct === "?" ? `?/${formatTokens(contextWindow)}` : `${pct}%/${formatTokens(contextWindow)}`;
					const contextColored =
						pctValue > 90 ? theme.fg("error", contextStr) : pctValue > 70 ? theme.fg("warning", contextStr) : contextStr;

					const parts: string[] = [];
					if (cacheWrite) parts.push(`W${formatTokens(cacheWrite)}`);
					if ((cacheRead > 0 || cacheWrite > 0) && hitRate !== undefined) parts.push(`CH${hitRate.toFixed(1)}%`);
					if (cost) parts.push(`$${cost.toFixed(3)}`);
					parts.push(contextColored);
					let statsLeft = parts.join(" ");

					// Model on the right, plus thinking level if the model supports reasoning
					const modelName = ctx.model?.id || "no-model";
					let right = modelName;
					if (ctx.model?.reasoning) {
						const level = ctx.thinkingLevel || "off";
						right = level === "off" ? `${modelName} • thinking off` : `${modelName} • ${level}`;
					}
					const statsLeftWidth = visibleWidth(statsLeft);
					const minPadding = 2;
					if (footerData.getAvailableProviderCount() > 1 && ctx.model) {
						const withProvider = `(${ctx.model.provider}) ${right}`;
						if (statsLeftWidth + minPadding + visibleWidth(withProvider) <= width) right = withProvider;
					}

					const rightWidth = visibleWidth(right);
					const totalNeeded = statsLeftWidth + minPadding + rightWidth;
					let statsLine: string;
					if (totalNeeded <= width) {
						statsLine = statsLeft + " ".repeat(width - statsLeftWidth - rightWidth) + right;
					} else {
						const availableForRight = width - statsLeftWidth - minPadding;
						if (availableForRight > 0) {
							const truncatedRight = truncateToWidth(right, availableForRight, "");
							const truncatedRightWidth = visibleWidth(truncatedRight);
							statsLine = statsLeft + " ".repeat(Math.max(0, width - statsLeftWidth - truncatedRightWidth)) + truncatedRight;
						} else {
							statsLine = statsLeft;
						}
					}

					// Dim each side separately; statsLeft may contain its own color codes
					const dimStatsLeft = theme.fg("dim", statsLeft);
					const dimRemainder = theme.fg("dim", statsLine.slice(statsLeft.length));

					let pwd = formatCwdForFooter(ctx.sessionManager.getCwd(), process.env.HOME || process.env.USERPROFILE);
					const branch = footerData.getGitBranch();
					if (branch) pwd += ` (${branch})`;
					const sessionName = ctx.sessionManager.getSessionName();
					if (sessionName) pwd += ` • ${sessionName}`;
					const pwdLine = truncateToWidth(theme.fg("dim", pwd), width, theme.fg("dim", "..."));

					const lines = [pwdLine, dimStatsLeft + dimRemainder];

					const extensionStatuses = footerData.getExtensionStatuses();
					if (extensionStatuses.size > 0) {
						const sorted = Array.from(extensionStatuses.entries())
							.sort(([a], [b]) => a.localeCompare(b))
							.map(([, text]) => text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim());
						lines.push(truncateToWidth(sorted.join(" "), width, theme.fg("dim", "...")));
					}
					return lines;
				},
			};
		});
	});
}
