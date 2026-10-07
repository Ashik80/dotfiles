---
description: Work in bounded steps with ZCode GLM Flash supervision at every checkpoint
argument-hint: <task>
---
Run a checkpoint-supervised workflow for this request:

$@

You are the coordinator. Use the subagent and todo tools. If no task was supplied, ask for it before delegating.

1. Preserve the user's original goal and constraints. Choose the worker the user explicitly requested; otherwise use "zcode-glm-flash-worker". Do not change the worker's configured model. Use agentScope: "user" so project agents cannot override the ZCode supervisor.
2. Ask "zcode-glm-flash-supervisor" to check a short proposed plan before implementation. Include the original request, constraints, exact working directory, relevant paths, and expected verification. It must use zcode/glm-5.3-flash; if unavailable, report the failure rather than substituting OpenRouter.
3. Delegate only the next approved bounded step to the worker, not the whole task. Include the original request, constraints, current plan, relevant paths, supervisor guidance, and verification instructions. Require the worker to stop after that step and report changes, verification results, blockers, and remaining work.
4. Once the worker stops, call "zcode-glm-flash-supervisor" with the original request, constraints, plan, cumulative progress, and the latest worker report. Give it the working directory and changed paths so it can inspect the evidence. Do not run it in parallel with a worker editing those files.
5. For CONTINUE or REDIRECT, pass the supervisor's guidance into the worker's next bounded step, then repeat the checkpoint. Each invocation has fresh context: include all necessary accumulated context explicitly. For BLOCKED, ask the user the supervisor's question. If two consecutive cycles make no progress, stop and report the blocker instead of looping.
6. Finish only after the supervisor returns DONE with evidence of the requested verification. Preserve any final review explicitly requested by the user. Report completed work, actual checks, and any unresolved limits.

This is checkpoint supervision, not live interruption. Keep the OpenRouter agent definitions and presets unchanged.
