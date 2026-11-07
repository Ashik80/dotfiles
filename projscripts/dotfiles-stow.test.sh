#!/usr/bin/env bash
set -euo pipefail

SCRIPT_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
REPO_ROOT="$(cd "$(dirname "$SCRIPT_PATH")/.." && pwd)"
SANDBOX="$(mktemp -d)"
trap 'rm -rf "$SANDBOX"' EXIT

fail() {
    printf 'error: %s\n' "$1" >&2
    exit 1
}

# A miniature dotfiles repository mirroring the real layout.
make_repo() {
    local repo="$1"
    mkdir -p "$repo/projscripts" \
        "$repo/.pi/agent/agents" \
        "$repo/.config/nvim" \
        "$repo/.claude" \
        "$repo/.codex" \
        "$repo/.agents/skills/asana" \
        "$repo/.local/share/applications" \
        "$repo/.git"
    cp "$REPO_ROOT/projscripts/dotfiles-stow.sh" "$repo/projscripts/"
    printf '%s\n' '{}' > "$repo/.pi/agent/settings.json"
    printf '%s\n' agent > "$repo/.pi/agent/agents/test.md"
    printf '%s\n' nvim > "$repo/.config/nvim/init.lua"
    printf '%s\n' claude > "$repo/.claude/CLAUDE.md"
    printf '%s\n' codex > "$repo/.codex/AGENTS.md"
    printf '%s\n' skill > "$repo/.agents/skills/asana/SKILL.md"
    printf '%s\n' '#!/usr/bin/env bash' > "$repo/.agents/skills/asana/asana"
    printf '%s\n' desktop > "$repo/.local/share/applications/neovide.desktop"
    printf '%s\n' vimrc > "$repo/.vimrc"
    printf '%s\n' doc > "$repo/foostan-qmk-build-guide.org"
    printf '%s\n' '{}' > "$repo/.git/config"
}

is_link_to() {
    [[ -L "$1" ]] && [[ "$(readlink -f "$1")" == "$(readlink -f "$2")" ]]
}

# Fresh machine: config is linked, runtime directories stay real, .git and
# non-config files are not linked.
SANDBOX="$SANDBOX/fresh"
mkdir -p "$SANDBOX/home"
make_repo "$SANDBOX/dotfiles"
HOME="$SANDBOX/home" "$SANDBOX/dotfiles/projscripts/dotfiles-stow.sh" >/dev/null

is_link_to "$SANDBOX/home/.pi/agent/settings.json" "$SANDBOX/dotfiles/.pi/agent/settings.json" ||
    fail 'settings.json was not linked'
is_link_to "$SANDBOX/home/.pi/agent/agents" "$SANDBOX/dotfiles/.pi/agent/agents" ||
    fail 'agents directory was not linked'
is_link_to "$SANDBOX/home/.config/nvim" "$SANDBOX/dotfiles/.config/nvim" ||
    fail 'config entry was not linked'
is_link_to "$SANDBOX/home/.vimrc" "$SANDBOX/dotfiles/.vimrc" || fail 'vimrc was not linked'
is_link_to "$SANDBOX/home/.claude/CLAUDE.md" "$SANDBOX/dotfiles/.claude/CLAUDE.md" ||
    fail 'CLAUDE.md was not linked'
is_link_to "$SANDBOX/home/.claude/skills" "$SANDBOX/dotfiles/.agents/skills" ||
    fail 'cross-source skills link was not created'
is_link_to "$SANDBOX/home/.local/bin/asana" "$SANDBOX/dotfiles/.agents/skills/asana/asana" ||
    fail 'cross-source asana link was not created'
is_link_to "$SANDBOX/dotfiles/.claude/skills" "$SANDBOX/dotfiles/.agents/skills" ||
    fail 'in-repo cross link is missing'

for directory in .pi .pi/agent .config .claude .codex .agents .local/bin; do
    [[ -d "$SANDBOX/home/$directory" && ! -L "$SANDBOX/home/$directory" ]] ||
        fail "runtime directory was replaced by a link: $directory"
done
[[ ! -e "$SANDBOX/home/.git" && ! -L "$SANDBOX/home/.git" ]] || fail '.git was linked'

# pi writes auth into the real home directory, never into the repository.
printf '%s\n' token > "$SANDBOX/home/.pi/agent/auth.json"
[[ -f "$SANDBOX/home/.pi/agent/auth.json" && ! -L "$SANDBOX/home/.pi/agent/auth.json" ]] ||
    fail 'auth.json is not a real file'
[[ ! -e "$SANDBOX/dotfiles/.pi/agent/auth.json" ]] || fail 'auth.json leaked into dotfiles'

# Re-running reconciles without taking over or backing up anything.
HOME="$SANDBOX/home" "$SANDBOX/dotfiles/projscripts/dotfiles-stow.sh" > "$SANDBOX/second-run.log"
grep -qE "^(relinked|backed up|created):" "$SANDBOX/second-run.log" && fail 'second run took over paths'
is_link_to "$SANDBOX/home/.pi/agent/settings.json" "$SANDBOX/dotfiles/.pi/agent/settings.json" ||
    fail 'second run broke an existing link'

# Adding a file links it; deleting one drops the stale link.
printf '%s\n' extra > "$SANDBOX/dotfiles/.extrarc"
rm "$SANDBOX/dotfiles/.vimrc"
HOME="$SANDBOX/home" "$SANDBOX/dotfiles/projscripts/dotfiles-stow.sh" >/dev/null
is_link_to "$SANDBOX/home/.extrarc" "$SANDBOX/dotfiles/.extrarc" || fail 'added file was not linked'
[[ ! -e "$SANDBOX/home/.vimrc" && ! -L "$SANDBOX/home/.vimrc" ]] || fail 'stale link was not removed'

# Links created by other tools are taken over; local files are backed up.
SANDBOX="$SANDBOX/takeover"
mkdir -p "$SANDBOX/home/.pi/agent" "$SANDBOX/home/.claude" "$SANDBOX/home/.local/share/applications" "$SANDBOX/home/.local/bin" "$SANDBOX/home/.config" "$SANDBOX/home/.agents" "$SANDBOX/home/.codex"
make_repo "$SANDBOX/dotfiles"
ln -s "$SANDBOX/dotfiles/.pi/agent/settings.json" "$SANDBOX/home/.pi/agent/settings.json"
ln -s "$SANDBOX/dotfiles/.claude/CLAUDE.md" "$SANDBOX/home/.claude/CLAUDE.md"
printf '%s\n' local-vimrc > "$SANDBOX/home/.vimrc"

HOME="$SANDBOX/home" "$SANDBOX/dotfiles/projscripts/dotfiles-stow.sh" >/dev/null

is_link_to "$SANDBOX/home/.pi/agent/settings.json" "$SANDBOX/dotfiles/.pi/agent/settings.json" ||
    fail 'existing link was not taken over'
is_link_to "$SANDBOX/home/.claude/CLAUDE.md" "$SANDBOX/dotfiles/.claude/CLAUDE.md" ||
    fail 'existing claude link was not taken over'
is_link_to "$SANDBOX/home/.vimrc" "$SANDBOX/dotfiles/.vimrc" || fail 'local vimrc was not replaced'
grep -Fxq local-vimrc "$SANDBOX/home"/.vimrc.before-dotfiles-* || fail 'local vimrc was not backed up'

# A dry run changes nothing.
SANDBOX="$SANDBOX/dry"
mkdir -p "$SANDBOX/home"
make_repo "$SANDBOX/dotfiles"
HOME="$SANDBOX/home" "$SANDBOX/dotfiles/projscripts/dotfiles-stow.sh" --dry-run >/dev/null
[[ -z "$(ls -A "$SANDBOX/home")" ]] || fail 'dry run modified the home directory'
[[ ! -L "$SANDBOX/dotfiles/.claude/skills" ]] || fail 'dry run created a link in the repo'

printf 'dotfiles-stow tests passed\n'
