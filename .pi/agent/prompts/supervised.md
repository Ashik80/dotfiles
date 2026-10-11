---
description: Work in bounded steps with selectable GLM or GPT workers and GLM, GPT, or Opus supervisors
argument-hint: "[--worker glm-flash|glm|gpt|gpt-luna|gpt-terra] [--supervisor glm-flash|glm|gpt|gpt-luna|gpt-terra|opus] <task>"
---
Run a checkpoint-supervised workflow for these arguments:

$@

You are the coordinator. Use the subagent and todo tools. Interpret the following workflow options yourself; they are not pi CLI flags.

Parse leading --worker <value> and --supervisor <value> options in either order. Both default to glm-flash. Remove those options from the task. For --worker, accept only glm-flash, glm, gpt, gpt-luna, or gpt-terra. For --supervisor, accept those values plus opus; if an option is missing its value, repeated, unknown, or has an unsupported value, explain the valid syntax and ask the user before delegating. An optional -- ends option parsing; do not interpret flags inside the remaining task. If no task remains, ask for it.

Select the agents from this mapping:
- Worker glm-flash: "zcode-glm-flash-worker" using zcode/glm-5.3-flash.
- Worker glm: "zcode-glm-worker" using zcode/glm-5.3.
- Worker gpt: "worker" using openai-codex/gpt-6.1-sol.
- Worker gpt-luna: "gpt-luna-worker" using openai-codex/gpt-5.6-luna.
- Worker gpt-terra: "gpt-terra-worker" using openai-codex/gpt-5.6-terra.
- Supervisor glm-flash: "zcode-glm-flash-supervisor" using zcode/glm-5.3-flash.
- Supervisor glm: "zcode-glm-supervisor" using zcode/glm-5.3.
- Supervisor gpt: "gpt-sol-supervisor" using openai-codex/gpt-6.1-sol.
- Supervisor gpt-luna: "gpt-luna-supervisor" using openai-codex/gpt-5.6-luna.
- Supervisor gpt-terra: "gpt-terra-supervisor" using openai-codex/gpt-5.6-terra.
- Supervisor opus: "opus-supervisor" using anthropic/claude-opus-5-5.

Use the selected pair throughout this workflow. If a selected agent or model is unavailable, report the failure rather than substituting another model or provider. These options replace the old --default-worker option.

1. Preserve the user's original goal and constraints. Do not change the selected agents' configured models. Use agentScope: "user" so project agents cannot override the selected pair.
2. Ask the selected supervisor to check a short proposed plan before implementation. Include the original request, constraints, exact working directory, relevant paths, and expected verification. Proceed only with an approved plan; pass requested corrections back for approval, and ask the user about blockers.
3. Delegate only the next approved bounded step to the selected worker, not the whole task. Include the original request, constraints, current plan, relevant paths, supervisor guidance, and verification instructions. Require the worker to stop after that step and report changes, verification results, blockers, and remaining work.
4. Once the worker stops, call the selected supervisor with the original request, constraints, plan, cumulative progress, and the latest worker report. Give it the working directory and changed paths so it can inspect the evidence. Do not run it in parallel with a worker editing those files.
5. For CONTINUE or REDIRECT, pass the supervisor's guidance into the worker's next bounded step, then repeat the checkpoint. Each invocation has fresh context: include all necessary accumulated context explicitly. For BLOCKED, ask the user the supervisor's question. If two consecutive cycles make no progress, stop and report the blocker instead of looping.
6. Finish only after the supervisor returns DONE with evidence of the requested verification. Preserve any final review explicitly requested by the user. Report completed work, actual checks, and any unresolved limits.

This is checkpoint supervision, not live interruption. Keep the OpenRouter agent definitions and presets unchanged.
