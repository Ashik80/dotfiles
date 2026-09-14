#!/usr/bin/env bash
set -euo pipefail

SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
REPO_ROOT="$(cd "$(dirname "$SCRIPT_PATH")/.." && pwd)"
SANDBOX="$(mktemp -d)"
trap 'rm -rf "$SANDBOX"' EXIT

mkdir -p \
    "$SANDBOX/projscripts" \
    "$SANDBOX/.agents/skills/asana" \
    "$SANDBOX/.claude" \
    "$SANDBOX/.codex" \
    "$SANDBOX/.pi/agent/agents" \
    "$SANDBOX/.pi/agent/prompts" \
    "$SANDBOX/.pi/agent/extensions/generate-image" \
    "$SANDBOX/.pi/agent/extensions/subagent/prompts"
cp "$REPO_ROOT/projscripts/install-ai-config.sh" "$SANDBOX/projscripts/"
printf '%s\n' test > "$SANDBOX/.agents/skills/asana/SKILL.md"
printf '%s\n' '#!/usr/bin/env bash' > "$SANDBOX/.agents/skills/asana/asana"
printf '%s\n' '{}' > "$SANDBOX/.agents/.skill-lock.json"
printf '%s\n' claude-global > "$SANDBOX/.claude/CLAUDE.md"
printf '%s\n' codex-global > "$SANDBOX/.codex/AGENTS.md"
printf '%s\n' test > "$SANDBOX/.pi/agent/AGENTS.md"
printf '%s\n' '{}' > "$SANDBOX/.pi/agent/settings.json"
printf '%s\n' '{}' > "$SANDBOX/.pi/agent/presets.json"
printf '%s\n' test > "$SANDBOX/.pi/agent/prompts/commit.md"
printf '%s\n' test > "$SANDBOX/.pi/agent/prompts/review.md"
printf '%s\n' test > "$SANDBOX/.pi/agent/agents/test.md"
printf '%s\n' test > "$SANDBOX/.pi/agent/extensions/generate-image/index.ts"
printf '%s\n' test > "$SANDBOX/.pi/agent/extensions/subagent/index.ts"
printf '%s\n' test > "$SANDBOX/.pi/agent/extensions/subagent/prompts/test.md"
for extension in confirm-destructive.ts git-checkpoint.ts notify.ts preset.ts protected-paths.ts todo.ts; do
    printf '%s\n' test > "$SANDBOX/.pi/agent/extensions/$extension"
done

HOME="$SANDBOX" "$SANDBOX/projscripts/install-ai-config.sh" >/dev/null

[[ -f "$SANDBOX/.agents/skills/asana/SKILL.md" && ! -L "$SANDBOX/.agents/skills/asana/SKILL.md" ]]
[[ -f "$SANDBOX/.agents/.skill-lock.json" && ! -L "$SANDBOX/.agents/.skill-lock.json" ]]
[[ -f "$SANDBOX/.claude/CLAUDE.md" && ! -L "$SANDBOX/.claude/CLAUDE.md" ]]
[[ -f "$SANDBOX/.codex/AGENTS.md" && ! -L "$SANDBOX/.codex/AGENTS.md" ]]
[[ -f "$SANDBOX/.pi/agent/AGENTS.md" && ! -L "$SANDBOX/.pi/agent/AGENTS.md" ]]
[[ -f "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" && ! -L "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" ]]

GLOBAL_HOME="$SANDBOX/home"
mkdir -p "$GLOBAL_HOME"
HOME="$GLOBAL_HOME" "$REPO_ROOT/projscripts/install-ai-config.sh" >/dev/null
[[ "$(readlink -f "$GLOBAL_HOME/.claude/CLAUDE.md")" == "$REPO_ROOT/.claude/CLAUDE.md" ]]
[[ "$(readlink -f "$GLOBAL_HOME/.codex/AGENTS.md")" == "$REPO_ROOT/.codex/AGENTS.md" ]]

printf 'install-ai-config tests passed\n'
