#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SANDBOX="$(mktemp -d)"
trap 'rm -rf "$SANDBOX"' EXIT

mkdir -p \
    "$SANDBOX/projscripts" \
    "$SANDBOX/.agents/skills/asana" \
    "$SANDBOX/.pi/agent/agents" \
    "$SANDBOX/.pi/agent/prompts" \
    "$SANDBOX/.pi/agent/extensions/generate-image" \
    "$SANDBOX/.pi/agent/extensions/subagent/prompts"
cp "$REPO_ROOT/projscripts/install-ai-config.sh" "$SANDBOX/projscripts/"
printf '%s\n' test > "$SANDBOX/.agents/skills/asana/SKILL.md"
printf '%s\n' '#!/usr/bin/env bash' > "$SANDBOX/.agents/skills/asana/asana"
printf '%s\n' '{}' > "$SANDBOX/.agents/.skill-lock.json"
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
[[ -f "$SANDBOX/.pi/agent/AGENTS.md" && ! -L "$SANDBOX/.pi/agent/AGENTS.md" ]]
[[ -f "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" && ! -L "$SANDBOX/.pi/agent/extensions/generate-image/index.ts" ]]

printf 'install-ai-config self-link test passed\n'
