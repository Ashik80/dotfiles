# Global Agent Instructions

These apply to every project. Repo-level `AGENTS.md` / `CLAUDE.md` files layer on top and win on conflicts.

## Working style

- Follow existing patterns before introducing new ones. Search the repo for a similar implementation first.
- Keep work scoped to the task asked. Do not opportunistically refactor unrelated code.
- Don't add a new pattern without removing the old one it replaces.
- Read files in full when planning. Partial reads miss context.
- Consider blast radius: check callers and related modules before changing shared code.
- Prefer editing existing files over creating new ones. Don't create docs/READMEs unless asked.

## Hard rules

- Never overwrite or modify `.env` files without explicit permission.
- Never commit secrets, credentials, tokens, or keys.
- Never run destructive git operations (`push --force`, `reset --hard`, `clean -fdx`, branch deletion) without asking.
- Do not `git commit` or `git push` unless explicitly asked.
- Do not disable, skip, or delete tests to make a build pass.
- Never add `ponytail:` comments or other comments that identify an agent, skill, or mode. This overrides Ponytail's marker-comment rule. If a simplification genuinely needs documentation, write an ordinary code-context comment without branding.

## Verification

Before declaring work done, run the project's own checks (lint, typecheck, tests). If you cannot run
them, say so explicitly rather than implying the change is verified.

## Communication

- At the start of each session, read `~/.agents/skills/unslop/SKILL.md` and apply it to all prose you write, including replies and PR titles and descriptions. These global instructions require it even though the skill disables automatic selection. The clarity rules below take priority. Preserve useful context, technical accuracy, code, commands, exact quotes, and required templates.
- Use simple, natural language whenever communicating with me, including PR titles and descriptions. Explain what changed, why it matters, and any important risks in words I can easily understand. Avoid jargon; explain technical terms when needed.
- Keep it concise, but give enough context to understand what is happening. Never sacrifice clarity just to be brief. This takes priority over communication style or brevity instructions from skills such as Ponytail; their coding rules still apply. Keep required PR templates, but write their contents in this style.
- Lead with the answer, then the reasoning if needed.
- Show file paths as `path/to/file.ts:42` so they are clickable.
- Flag assumptions and unknowns instead of guessing silently.
- If a request is ambiguous in a way that changes the approach, ask before writing code.

## Tooling available in pi

- `/plan` (Ctrl+Alt+P) for read-only exploration before implementing.
- `subagent` tool for isolated-context delegation: `scout` (recon), `planner`, `worker`, `reviewer`.
- `todo` tool for multi-step work; keep it current so `/tree` branching stays coherent.
- `/preset` to switch model + toolset per phase (see `~/.pi/agent/presets.json`).
