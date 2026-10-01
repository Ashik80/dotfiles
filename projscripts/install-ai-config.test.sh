#!/usr/bin/env bash
set -euo pipefail

SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
REPO_ROOT="$(cd "$(dirname "$SCRIPT_PATH")/.." && pwd)"
SANDBOX="$(mktemp -d)"
trap 'rm -rf "$SANDBOX"' EXIT

mkdir -p \
    "$SANDBOX/projscripts" \
    "$SANDBOX/.agents/skills/asana" \
    "$SANDBOX/.agents/skills/browser-tools" \
    "$SANDBOX/.agents/skills/youtube-transcript" \
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
printf '%s\n' '{}' > "$SANDBOX/.agents/skills/browser-tools/package.json"
printf '%s\n' '{}' > "$SANDBOX/.agents/skills/browser-tools/package-lock.json"
printf '%s\n' '{}' > "$SANDBOX/.agents/skills/youtube-transcript/package.json"
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
for command in claude codex npm pi; do
    cat > "$SANDBOX/bin/$command" <<'EOF'
#!/usr/bin/env bash
printf '%s %s\n' "$(basename "$0")" "$*" >> "$AI_CONFIG_COMMAND_LOG"
if [[ "$(basename "$0")" == npm && "$*" == 'install -g --ignore-scripts @earendil-works/pi-coding-agent' ]]; then
    cp "$AI_CONFIG_PI_STUB" "$(dirname "$0")/pi"
fi
if [[ "${AI_CONFIG_FAIL_PI:-}" == 1 && "$(basename "$0")" == pi ]]; then
    exit 1
fi
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
grep -Fxq 'pi update --extensions --no-approve' "$COMMAND_LOG"
grep -Fxq 'npm install -g --ignore-scripts @mariozechner/gccli @mariozechner/gdcli @mariozechner/gmcli' "$COMMAND_LOG"
grep -Fxq "npm ci --prefix $REPO_ROOT/.agents/skills/browser-tools" "$COMMAND_LOG"
grep -Fxq "npm install --prefix $REPO_ROOT/.agents/skills/youtube-transcript --package-lock=false" "$COMMAND_LOG"
[[ -L "$GLOBAL_HOME/.pi/agent/agents/openrouter-vision.md" ]]
[[ "$(readlink -f "$GLOBAL_HOME/.agents/skills")" == "$REPO_ROOT/.agents/skills" ]]
for source in "$REPO_ROOT"/.pi/agent/agents/*.md "$REPO_ROOT"/.pi/agent/extensions/* "$REPO_ROOT"/.pi/agent/prompts/*.md; do
    relative="${source#"$REPO_ROOT"/}"
    [[ "$(readlink -f "$GLOBAL_HOME/$relative")" == "$source" ]]
done
for source in "$REPO_ROOT"/.pi/agent/extensions/subagent/prompts/*.md; do
    [[ "$(readlink -f "$GLOBAL_HOME/.pi/agent/prompts/$(basename "$source")")" == "$source" ]]
done

# Reinstalling preserves correct links, local credentials, and existing backups.
printf '%s\n' local-auth > "$GLOBAL_HOME/.pi/agent/auth.json"
rm "$GLOBAL_HOME/.pi/agent/prompts/commit.md"
printf '%s\n' local-prompt > "$GLOBAL_HOME/.pi/agent/prompts/commit.md"
AI_CONFIG_COMMAND_LOG="$COMMAND_LOG" PATH="$SANDBOX/bin:$PATH" HOME="$GLOBAL_HOME" "$REPO_ROOT/projscripts/install-ai-config.sh" >/dev/null
[[ "$(< "$GLOBAL_HOME/.pi/agent/auth.json")" == local-auth ]]
[[ -L "$GLOBAL_HOME/.pi/agent/prompts/commit.md" ]]
grep -Fxq local-prompt "$GLOBAL_HOME"/.pi/agent/prompts/commit.md.before-dotfiles-*

# A missing Pi CLI is installed before packages are reconciled.
cp "$SANDBOX/bin/pi" "$SANDBOX/pi-stub"
rm "$SANDBOX/bin/pi"
ln -s "$(command -v node)" "$SANDBOX/bin/node"
AI_CONFIG_PI_STUB="$SANDBOX/pi-stub" AI_CONFIG_COMMAND_LOG="$COMMAND_LOG" PATH="$SANDBOX/bin:/usr/bin:/bin" HOME="$GLOBAL_HOME" /bin/bash "$REPO_ROOT/projscripts/install-ai-config.sh" >/dev/null
grep -Fxq 'npm install -g --ignore-scripts @earendil-works/pi-coding-agent' "$COMMAND_LOG"

# Package installation failures must not be reported as success.
if AI_CONFIG_FAIL_PI=1 AI_CONFIG_COMMAND_LOG="$COMMAND_LOG" PATH="$SANDBOX/bin:$PATH" HOME="$GLOBAL_HOME" "$REPO_ROOT/projscripts/install-ai-config.sh" >/dev/null; then
    printf 'error: failed Pi package installation was ignored\n' >&2
    exit 1
fi

printf 'install-ai-config tests passed\n'
