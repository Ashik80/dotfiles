---
name: asana
description: Asana CLI for reading, creating, updating, commenting on and acting on tasks (assign, complete, move between sections/columns, tags, subtasks, dependencies, attachments, custom fields). Use whenever the user mentions Asana tickets, boards, epics, or gives an app.asana.com URL.
compatibility: Requires python3 (stdlib only) and an Asana personal access token.
---

# Asana CLI

`./asana` is a dependency-free Python wrapper over the Asana REST API v1.0. Pi has no MCP
support, so this replaces the Asana MCP server.

It is symlinked to `~/.local/bin/asana` (on `$PATH`), so call it as plain `asana`.
If the symlink is missing, run:

```bash
ln -sfn ~/.pi/agent/skills/asana/asana ~/.local/bin/asana
```

## Setup (one-time)

1. User creates a personal access token: <https://app.asana.com/0/my-apps> → "Create new token".
2. Store it (written to `~/.asana/config.json`, chmod 600):
   ```bash
   asana config set-token <TOKEN>
   ```
   `$ASANA_TOKEN` / `$ASANA_ACCESS_TOKEN` override the file if set.
3. Pin the default workspace so you can omit `--workspace`:
   ```bash
   asana whoami                       # lists workspace gids
   asana config set-workspace <GID>
   ```
4. Optional aliases for boards you use often — any `--project`/task argument accepts them:
   ```bash
   asana config alias platform https://app.asana.com/1/<ws>/project/<gid>/board/<view>
   asana tasks --project platform
   asana config show          # token is masked
   asana config unalias platform
   ```

Verify with `asana whoami`. Never print the token back to the user or commit it.

## Identifiers

Every task/project argument accepts **a gid, a full Asana URL, or a config alias**.
`--project` additionally accepts an exact/substring project name. `--assignee` accepts
`me`, a gid, an email, or a user's display name.

## Reading

```bash
asana whoami                                  # me + workspaces
asana projects [--grep kyc] [--all]           # --all includes archived
asana project <project>                       # description, owner, status
asana sections --project <project>            # board columns
asana tasks --project <project>               # open tasks (add --include-done)
asana tasks --section <section>
asana tasks --assignee me                     # my tasks in the workspace
asana search "credit score" [--project p] [--assignee me]
asana task <task|url>                         # full detail incl. custom fields + notes
asana subtasks <task>
asana comments <task> [--all]                 # --all = full activity log, not just comments
asana attachments <task>
asana users --grep ali                        # gid lookup for assignment
asana tags
asana custom-fields --project <project>       # field gids + enum option gids
```

Add `--json` to any command for raw API JSON (use when you need fields the text view omits).

## Writing

```bash
asana create --project <project> --name "Fix X" \
  [--notes "…"] [--notes-file notes.md] [--section <section>] \
  [--assignee me] [--due 2025-06-01] [--tag <tag>] [--followers <user>] \
  [--field <field-gid>=<value-or-option-name>]

asana subtask <parent-task> --name "Write migration" [--assignee me]
asana update <task> [--name …] [--notes …] [--assignee …] [--unassign] \
                    [--due 2025-06-01] [--start …] [--field gid=value] [--complete|--reopen]
asana comment <task> "Deployed to staging"     # or: … - (reads stdin), or --file notes.md
```

Long descriptions/comments: pipe them in to avoid shell-quoting pain.

```bash
printf '%s\n' "line one" "line two" | asana comment <task> -
asana create --project platform --name "Bug: X" --notes - <<'EOF'
Steps to reproduce
...
EOF
```

Rich text: pass `--html` and wrap the value in `<body>…</body>` (Asana supports a
restricted HTML subset: `<b> <i> <code> <a> <ul> <ol> <li> <h1> <h2>`).

## Actions

```bash
asana complete <task>              asana reopen <task>
asana assign <task> me             asana assign <task> none
asana due <task> 2025-06-01        asana due <task>              # clears
asana move <task> --section <section> [--before <task>|--after <task>]
asana add-project <task> --project <p> [--section <s>]
asana remove-project <task> --project <p>
asana tag <task> <tag> [--remove]
asana follow <task> <user…> [--remove]
asana depend <task> <blocker…>            # task is blocked by blocker(s)
asana depend <task> <blocked…> --blocks   # task blocks the others
asana attach <task> ./screenshot.png
asana delete <task> --yes          # destructive; always confirm with the user first
```

## Escape hatch

Anything not covered above (portfolios, goals, project status updates, webhooks,
task templates, batch API):

```bash
asana raw GET /projects/<gid>/project_statuses --param 'opt_fields=title,text'
asana raw POST /tasks/<gid>/addFollowers --data '{"followers":["123"]}'
asana raw PUT /projects/<gid> --data '{"name":"New name"}'
```

API reference: <https://developers.asana.com/reference/rest-api-reference>

## Behaviour notes

- Pagination is followed automatically; `--limit` caps results.
- 429/5xx are retried with backoff (respects `Retry-After`).
- `asana search` needs a paid plan; it auto-falls back to typeahead (name match only) and
  says so on stderr.
- Custom fields: pass the field **gid**; enum values may be given as the option's name and
  are resolved to option gids. Multi-enum takes `|`-separated values. Empty value clears.
- Writes are real and immediate. Confirm the target task with the user before destructive
  actions (`delete`, `remove-project`, clearing fields).

## Files

- `~/.asana/config.json` — token (0600), default workspace, aliases. Never commit.
- `tests/run.sh` — offline smoke test of every subcommand against `tests/mock_api.py`
  (no token/network). Run it after editing the CLI.
