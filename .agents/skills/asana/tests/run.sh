#!/usr/bin/env bash
# Offline smoke test: runs every asana subcommand against tests/mock_api.py.
# No real token or network needed. Usage: tests/run.sh
set -uo pipefail
cd "$(dirname "$0")"
CLI="$(cd .. && pwd)/asana"
TMP=$(mktemp -d)
trap 'pkill -f "$TMP/mock_api.py" 2>/dev/null; rm -rf "$TMP"' EXIT

cp mock_api.py "$TMP/"
(cd "$TMP" && nohup python3 mock_api.py >/dev/null 2>&1 &)
for _ in $(seq 20); do
  curl -sf -o /dev/null "http://127.0.0.1:8731/api/1.0/workspaces" && break
  sleep 0.2
done

export ASANA_API_BASE=http://127.0.0.1:8731/api/1.0
export ASANA_TOKEN=TESTTOKEN
export ASANA_CONFIG_DIR="$TMP/cfg"
export ASANA_WORKSPACE=100

fail=0
check() {
  if ! out=$("$CLI" "$@" 2>&1); then
    printf 'FAIL  asana %s\n%s\n' "$*" "$(echo "$out" | head -3)"
    fail=1
  else
    printf 'ok    asana %s\n' "$*"
  fi
}

check whoami
check workspaces
check projects
check project 222
check sections --project 222
check users
check tags
check custom-fields --project 222
check tasks --project 222
check tasks --project 222 --include-done
check tasks --section 333
check tasks --assignee me
check search kyc
check task 1111
check task https://app.asana.com/0/222/1111
check subtasks 1111
check comments 1111
check comments 1111 --all
check comment 1111 "hello"
check comment 1111 "<body><b>b</b></body>" --html
check create --project 222 --name "New" --notes d --assignee Ali --due 2025-09-09 --section 333 --field 555=High
check create --name Standalone
check subtask 1111 --name Sub
check update 1111 --name Z --unassign --field 555=Low
check complete 1111
check reopen 1111
check assign 1111 me
check assign 1111 none
check due 1111 2025-01-01
check due 1111
check move 1111 --section 334
check add-project 1111 --project 223 --section 333
check remove-project 1111 --project 223
check tag 1111 77
check tag 1111 77 --remove
check follow 1111 Ali
check depend 1111 1112
check depend 1111 1112 --blocks
check attachments 1111
check delete 1111 --yes
check raw GET /tasks/1111
check url https://app.asana.com/1/100/project/222/task/1111
check --json projects
check config show

# negative cases must fail
for bad in "delete 1111" "tasks" "config set-token"; do
  if "$CLI" $bad >/dev/null 2>&1; then
    echo "FAIL  expected failure: asana $bad"
    fail=1
  else
    printf 'ok    rejected: asana %s\n' "$bad"
  fi
done

[ $fail -eq 0 ] && echo "PASS" || echo "FAILURES"
exit $fail
