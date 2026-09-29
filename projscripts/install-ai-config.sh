#!/usr/bin/env bash
# Link shared Agent Skills and portable pi configuration from this dotfiles checkout.
# Existing paths are preserved as timestamped backups.
set -euo pipefail

SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
DOTFILES="$(cd "$(dirname "$SCRIPT_PATH")/.." && pwd)"
STAMP="$(date +%Y%m%d-%H%M%S)"

link_path() {
    local source="$1" destination="$2"
    local resolved_source resolved_destination

    if [[ ! -e "$source" && ! -L "$source" ]]; then
        printf 'error: link source does not exist: %s\n' "$source" >&2
        return 1
    fi

    resolved_source="$(readlink -f "$source" 2>/dev/null || true)"
    resolved_destination="$(readlink -f "$destination" 2>/dev/null || true)"
    if [[ "$source" == "$destination" ]] ||
        [[ -n "$resolved_source" && "$resolved_source" == "$resolved_destination" ]]; then
        printf 'already in place: %s\n' "$destination"
        return
    fi

    mkdir -p "$(dirname "$destination")"

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

# Global instructions for each coding agent.
link_path "$DOTFILES/.claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
link_path "$DOTFILES/.codex/AGENTS.md" "$HOME/.codex/AGENTS.md"

# Ponytail uses this shared default across pi, Claude Code, and Codex.
link_path "$DOTFILES/.config/ponytail/config.json" "$HOME/.config/ponytail/config.json"

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

# Extensions are kept in dotfiles so every machine installs the same versions.
for extension in confirm-destructive.ts generate-image git-checkpoint.ts notify.ts preset.ts protected-paths.ts subagent todo.ts; do
    link_path "$DOTFILES/.pi/agent/extensions/$extension" "$HOME/.pi/agent/extensions/$extension"
done

# Install the workflow prompts bundled with the subagent extension.
for prompt in "$DOTFILES"/.pi/agent/extensions/subagent/prompts/*.md; do
    link_path "$prompt" "$HOME/.pi/agent/prompts/$(basename "$prompt")"
done

# Convenience CLI installed by the custom Asana skill.
link_path "$DOTFILES/.agents/skills/asana/asana" "$HOME/.local/bin/asana"

# Keep Ponytail installed for coding agents whose plugin registries are local state.
if command -v claude >/dev/null 2>&1; then
    claude plugin marketplace add DietrichGebert/ponytail --scope user
    claude plugin install ponytail@ponytail --scope user
else
    printf 'skipped:         Claude Code is not installed\n'
fi

if command -v codex >/dev/null 2>&1; then
    codex plugin marketplace add DietrichGebert/ponytail
    codex plugin add ponytail@ponytail
else
    printf 'skipped:         Codex is not installed\n'
fi

printf '\nDone. Restart pi, Claude Code, and Codex to reload skills and settings.\n'
