#!/usr/bin/env bash
# Link shared Agent Skills and portable pi configuration from this dotfiles checkout.
# Existing paths are preserved as timestamped backups.
set -euo pipefail

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"

link_path() {
    local source="$1" destination="$2"
    mkdir -p "$(dirname "$destination")"

    if [[ -L "$destination" ]] && [[ "$(readlink -f "$destination")" == "$(readlink -f "$source")" ]]; then
        printf 'already linked: %s\n' "$destination"
        return
    fi

    if [[ -e "$destination" || -L "$destination" ]]; then
        local backup="${destination}.before-dotfiles-${STAMP}"
        mv "$destination" "$backup"
        printf 'backed up:      %s -> %s\n' "$destination" "$backup"
    fi

    ln -s "$source" "$destination"
    printf 'linked:          %s -> %s\n' "$destination" "$source"
}

# Shared skills: pi discovers ~/.agents/skills; Claude uses ~/.claude/skills.
link_path "$DOTFILES/.agents/skills" "$HOME/.agents/skills"
link_path "$DOTFILES/.agents/skills" "$HOME/.claude/skills"
link_path "$DOTFILES/.agents/.skill-lock.json" "$HOME/.agents/.skill-lock.json"

# Portable pi configuration. Authentication and runtime files remain local.
link_path "$DOTFILES/.pi/agent/AGENTS.md" "$HOME/.pi/agent/AGENTS.md"
link_path "$DOTFILES/.pi/agent/settings.json" "$HOME/.pi/agent/settings.json"
link_path "$DOTFILES/.pi/agent/presets.json" "$HOME/.pi/agent/presets.json"
link_path "$DOTFILES/.pi/agent/prompts/commit.md" "$HOME/.pi/agent/prompts/commit.md"
link_path "$DOTFILES/.pi/agent/prompts/review.md" "$HOME/.pi/agent/prompts/review.md"

# Agent definitions are configuration, so keep them in dotfiles.
for agent in "$DOTFILES"/.pi/agent/agents/*.md; do
    link_path "$agent" "$HOME/.pi/agent/agents/$(basename "$agent")"
done

# Link selected extensions from the active pi installation so upgrades are followed.
PI_BIN="$(command -v pi || true)"
if [[ -z "$PI_BIN" ]]; then
    printf 'error: pi is not installed or not in PATH\n' >&2
    exit 1
fi
PI_ROOT="$(cd "$(dirname "$(readlink -f "$PI_BIN")")/../.." && pwd)"
EXTENSION_EXAMPLES="$PI_ROOT/examples/extensions"
if [[ ! -d "$EXTENSION_EXAMPLES" ]]; then
    printf 'error: pi extension examples not found at %s\n' "$EXTENSION_EXAMPLES" >&2
    exit 1
fi

for extension in confirm-destructive.ts git-checkpoint.ts notify.ts preset.ts protected-paths.ts todo.ts; do
    link_path "$EXTENSION_EXAMPLES/$extension" "$HOME/.pi/agent/extensions/$extension"
done
link_path "$DOTFILES/.pi/agent/extensions/generate-image" "$HOME/.pi/agent/extensions/generate-image"
link_path "$EXTENSION_EXAMPLES/subagent/index.ts" "$HOME/.pi/agent/extensions/subagent/index.ts"
link_path "$EXTENSION_EXAMPLES/subagent/agents.ts" "$HOME/.pi/agent/extensions/subagent/agents.ts"

# Subagent workflow prompts come from pi; custom GPT agent definitions come from dotfiles.
for prompt in "$EXTENSION_EXAMPLES"/subagent/prompts/*.md; do
    link_path "$prompt" "$HOME/.pi/agent/prompts/$(basename "$prompt")"
done

# Convenience CLI installed by the custom Asana skill.
link_path "$DOTFILES/.agents/skills/asana/asana" "$HOME/.local/bin/asana"

printf '\nDone. Restart pi and Claude to reload skills and settings.\n'
