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

## Verification

Before declaring work done, run the project's own checks (lint, typecheck, tests). If you cannot run
them, say so explicitly rather than implying the change is verified.

## Communication

- Be concise. Lead with the answer, then the reasoning if needed.
- Show file paths as `path/to/file.ts:42` so they are clickable.
- Flag assumptions and unknowns instead of guessing silently.
- If a request is ambiguous in a way that changes the approach, ask before writing code.

## Tooling available in pi

- `/plan` (Ctrl+Alt+P) for read-only exploration before implementing.
- `subagent` tool for isolated-context delegation: `scout` (recon), `planner`, `worker`, `reviewer`.
- `todo` tool for multi-step work; keep it current so `/tree` branching stays coherent.
- `/preset` to switch model + toolset per phase (see `~/.pi/agent/presets.json`).
