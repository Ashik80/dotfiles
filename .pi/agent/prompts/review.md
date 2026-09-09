---
description: Review uncommitted or staged changes for bugs and risk
argument-hint: "[staged|branch|<path>]"
---
Review the changes in this repo. Scope: `${1:-working tree}`.

Pick the right diff command for the scope:
- working tree -> `git diff` plus `git status --short` for untracked files
- `staged` -> `git diff --cached`
- `branch` -> `git diff $(git merge-base HEAD origin/HEAD 2>/dev/null || git merge-base HEAD main)...HEAD`
- a path -> `git diff -- <path>`

Read the surrounding code, not just the diff hunks, before judging.

Report findings ranked by severity, each as `path/to/file.ts:LINE — problem — fix`:
1. Correctness bugs and logic errors
2. Security issues (injection, authz gaps, leaked secrets, unsafe deserialization)
3. Missing error handling and unhandled failure paths
4. Race conditions, N+1 queries, and other performance traps
5. Missing or wrong tests

Do not modify files. If the change looks clean, say so instead of manufacturing nitpicks.
