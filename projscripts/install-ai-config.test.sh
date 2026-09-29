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
    "$SANDBOX/.config/ponytail" \
    "$SANDBOX/.pi/agent/agents" \
    "$SANDBOX/bin" \
    "$SANDBOX/.pi/agent/prompts" \
    "$SANDBOX/.pi/agent/extensions/generate-image" \
    "$SANDBOX/.pi/agent/extensions/subagent/prompts"
cp "$REPO_ROOT/projscripts/install-ai-config.sh" "$SANDBOX/projscripts/"
printf '%s\n' test > "$SANDBOX/.agents/skills/asana/SKILL.md"
printf '%s\n' '#!/usr/bin/env bash' > "$SANDBOX/.agents/skills/asana/asana"
printf '%s\n' '{}' > "$SANDBOX/.agents/.skill-lock.json"
printf '%s\n' claude-global > "$SANDBOX/.claude/CLAUDE.md"
printf '%s\n' codex-global > "$SANDBOX/.codex/AGENTS.md"
printf '%s\n' '{"defaultMode":"full"}' > "$SANDBOX/.config/ponytail/config.json"
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

COMMAND_LOG="$SANDBOX/plugin-commands.log"
for command in claude codex; do
    cat > "$SANDBOX/bin/$command" <<'EOF'
#!/usr/bin/env bash
printf '%s %s\n' "$(basename "$0")" "$*" >> "$AI_CONFIG_COMMAND_LOG"
EOF
    chmod +x "$SANDBOX/bin/$command"
done

AI_CONFIG_COMMAND_LOG="$COMMAND_LOG" PATH="$SANDBOX/bin:$PATH" HOME="$SANDBOX" "$SANDBOX/projscripts/install-ai-config.sh" >/dev/null

[[ -f "$SANDBOX/.agents/skills/asana/SKILL.md" && ! -L "$SANDBOX/.agents/skills/asana/SKILL.md" ]]
[[ -f "$SANDBOX/.agents/.skill-lock.json" && ! -L "$SANDBOX/.agents/.skill-lock.json" ]]
[[ -f "$SANDBOX/.claude/CLAUDE.md" && ! -L "$SANDBOX/.claude/CLAUDE.md" ]]
[[ -f "$SANDBOX/.codex/AGENTS.md" && ! -L "$SANDBOX/.codex/AGENTS.md" ]]
[[ -f "$SANDBOX/.config/ponytail/config.json" && ! -L "$SANDBOX/.config/ponytail/config.json" ]]
[[ -f "$SANDBOX/.pi/agent/AGENTS.md" && ! -L "$SANDBOX/.pi/agent/AGENTS.md" ]]
[[ -f "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" && ! -L "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" ]]

GLOBAL_HOME="$SANDBOX/home"
mkdir -p "$GLOBAL_HOME"
AI_CONFIG_COMMAND_LOG="$COMMAND_LOG" PATH="$SANDBOX/bin:$PATH" HOME="$GLOBAL_HOME" "$REPO_ROOT/projscripts/install-ai-config.sh" >/dev/null
[[ "$(readlink -f "$GLOBAL_HOME/.claude/CLAUDE.md")" == "$REPO_ROOT/.claude/CLAUDE.md" ]]
[[ "$(readlink -f "$GLOBAL_HOME/.codex/AGENTS.md")" == "$REPO_ROOT/.codex/AGENTS.md" ]]
[[ "$(readlink -f "$GLOBAL_HOME/.config/ponytail/config.json")" == "$REPO_ROOT/.config/ponytail/config.json" ]]
grep -Fxq 'claude plugin marketplace add DietrichGebert/ponytail --scope user' "$COMMAND_LOG"
grep -Fxq 'claude plugin install ponytail@ponytail --scope user' "$COMMAND_LOG"
grep -Fxq 'codex plugin marketplace add DietrichGebert/ponytail' "$COMMAND_LOG"
grep -Fxq 'codex plugin add ponytail@ponytail' "$COMMAND_LOG"

printf 'install-ai-config tests passed\n'
