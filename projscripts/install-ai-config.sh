#!/usr/bin/env bash
# Install shared Agent Skills, their npm dependencies, and pi/claude/codex plugins.
# Symlinks are owned by dotfiles-stow.sh; this script only installs what they need.
set -euo pipefail

for command in node npm git; do
    if ! command -v "$command" >/dev/null 2>&1; then
        printf 'error: install %s before running this script\n' "$command" >&2
        exit 1
    fi
done
node -e 'const [major, minor] = process.versions.node.split(".").map(Number); if (major < 22 || (major === 22 && minor < 19)) { console.error("error: Node.js 22.19 or newer is required"); process.exit(1); }'

SCRIPT_PATH="$(node -e 'console.log(require("node:fs").realpathSync(process.argv[1]))' "${BASH_SOURCE[0]}")"
DOTFILES="$(cd "$(dirname "$SCRIPT_PATH")/.." && pwd)"
# Symlinks come from dotfiles-stow.sh; warn when that has not run yet.
for relative in .pi/agent/settings.json .pi/agent/agents .agents/skills .claude/CLAUDE.md; do
    if [[ ! -L "$HOME/$relative" ]]; then
        printf 'manual setup:    %s is not linked, run projscripts/dotfiles-stow.sh\n' "$relative"
        break
    fi
done

# Install Pi if needed, then reconcile packages declared in the linked settings.
if ! command -v pi >/dev/null 2>&1; then
    npm install -g --ignore-scripts @earendil-works/pi-coding-agent
fi
pi update --extensions --no-approve

# Shared skills contain instructions, not their npm dependencies or global CLIs.
# browser-start uses the system Chrome/Chromium, so do not download another browser.
PUPPETEER_SKIP_DOWNLOAD=true npm ci --prefix "$DOTFILES/.agents/skills/browser-tools"
npm install --prefix "$DOTFILES/.agents/skills/youtube-transcript" --package-lock=false
npm install -g --ignore-scripts @mariozechner/gccli @mariozechner/gdcli @mariozechner/gmcli

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

# OS applications and credentials are deliberately not installed or copied.
for command in python3 gh code rsync ffmpeg; do
    if ! command -v "$command" >/dev/null 2>&1; then
        printf 'manual setup:    %s (needed by optional skills)\n' "$command"
    fi
done
if ! python3 -c 'import yaml' >/dev/null 2>&1; then
    printf 'manual setup:    PyYAML for review-pr posting (install for your Python interpreter)\n'
fi
printf 'manual setup:    Chrome/Chromium for browser-tools; transcribe needs Apple Silicon macOS\n'
printf 'manual setup:    pi /login, OpenRouter key, Google CLI accounts, Asana token, gh auth login\n'
printf 'unchanged:       machine-local credentials/runtime state and skill sync metadata\n'
printf '\nDone. Restart pi, Claude Code, and Codex to reload skills and settings.\n'
