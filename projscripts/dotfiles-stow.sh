#!/usr/bin/env bash
# Link this dotfiles repository into $HOME with GNU stow.
# Runtime directories are created first so stow links config files one by one
# instead of folding whole directories (with tokens, caches, and history) into
# the repository.
set -euo pipefail

usage() {
    printf 'usage: %s [--dry-run]\n' "$(basename "$0")"
}

DRY_RUN=0
case "${1:-}" in
    -n | --dry-run) DRY_RUN=1 ;;
    "") ;;
    *)
        usage >&2
        exit 2
        ;;
esac

if ! command -v stow >/dev/null 2>&1; then
    printf 'error: GNU stow is not installed\n' >&2
    exit 1
fi

STOW_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/.." && pwd)"
TARGET="$HOME"
STAMP="$(date +%Y%m%d-%H%M%S)"

# Machine-local state lives here. These must exist before stowing, otherwise
# stow folds the directory itself into dotfiles and pi/claude write auth tokens
# and session history into the repository.
RUNTIME_DIRS=(.agents .claude .codex .config .local/bin .local/share/applications .pi/agent)

STOW_FLAGS=()
if [[ "$DRY_RUN" == 1 ]]; then
    STOW_FLAGS+=(-n)
fi

# stow can only link paths that exist inside the package tree, so cross-source
# links are stored in the repository itself.
link_in_repo() {
    local link="$1" source="$2"
    local destination="$STOW_DIR/$link"

    [[ -e "$STOW_DIR/$source" ]] || return 0

    if [[ -L "$destination" ]]; then
        if [[ "$(readlink -f "$destination")" == "$(readlink -f "$STOW_DIR/$source")" ]]; then
            return 0
        fi
        printf 'error: %s points somewhere else\n' "$destination" >&2
        return 1
    fi

    printf 'created:         %s -> %s\n' "$destination" "$source"
    [[ "$DRY_RUN" == 1 ]] && return 0
    mkdir -p "$(dirname "$destination")"
    ln -s "$(realpath --relative-to="$(dirname "$destination")" "$STOW_DIR/$source")" "$destination"
}

for directory in "${RUNTIME_DIRS[@]}"; do
    [[ -d "$TARGET/$directory" ]] && continue
    printf 'created:         %s\n' "$TARGET/$directory"
    [[ "$DRY_RUN" == 1 ]] || mkdir -p "$TARGET/$directory"
done

link_in_repo .claude/skills .agents/skills
link_in_repo .local/bin/asana .agents/skills/asana/asana

# stow refuses to overwrite anything it does not own. Links that already point
# into dotfiles are replaced in place; anything genuinely local is moved aside
# first, never deleted.
resolve_conflict() {
    local destination="$TARGET/$1"

    if [[ -L "$destination" ]] && [[ "$(readlink -f "$destination")" == "$STOW_DIR"/* ]]; then
        printf 'relinked:        %s\n' "$destination"
        [[ "$DRY_RUN" == 1 ]] || rm "$destination"
        return 0
    fi

    if [[ -e "$destination" ]]; then
        printf 'backed up:       %s -> %s.before-dotfiles-%s\n' "$destination" "$destination" "$STAMP"
        [[ "$DRY_RUN" == 1 ]] || mv "$destination" "$destination.before-dotfiles-$STAMP"
    fi
}

linked=0
for attempt in 1 2 3 4 5; do
    status=0
    # Restow so links for files you deleted or renamed in the repo are dropped.
    output="$(cd "$STOW_DIR" && stow "${STOW_FLAGS[@]}" -R -v -t "$TARGET" . 2>&1)" || status=$?
    printf '%s\n' "$output" | grep -v '^$' || true

    if [[ "$status" == 0 ]]; then
        linked=1
        break
    fi

    conflicts="$(printf '%s\n' "$output" | sed -n \
        -e 's/^  \* existing target is not owned by stow: //p' \
        -e 's/^  \* cannot stow .* over existing target \(.*\) since neither a link nor a directory.*/\1/p')"
    if [[ -z "$conflicts" ]]; then
        printf 'error: stow failed without reporting resolvable conflicts\n' >&2
        exit 1
    fi

    while IFS= read -r conflict; do
        [[ -n "$conflict" ]] && resolve_conflict "$conflict"
    done <<<"$conflicts"

    # A dry run cannot clear conflicts, so report the first pass and stop.
    if [[ "$DRY_RUN" == 1 ]]; then
        linked=1
        break
    fi
done

if [[ "$linked" == 0 ]]; then
    printf 'error: stow could not finish after %s attempts\n' "$attempt" >&2
    exit 1
fi

printf '\nlinked:          %s -> %s\n' "$STOW_DIR" "$TARGET"
