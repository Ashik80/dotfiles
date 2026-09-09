#!/usr/bin/env python3
"""Mock Asana API to exercise the CLI offline."""
import json
import re
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlsplit, parse_qs

LOG = []

TASK = {
    "gid": "1111", "name": "Fix KYC validation", "completed": False,
    "due_on": "2025-07-01", "assignee": {"gid": "9", "name": "Ashik"},
    "permalink_url": "https://app.asana.com/0/222/1111",
    "memberships": [{"section": {"name": "In Progress"}, "project": {"name": "Platform"}}],
    "projects": [{"gid": "222", "name": "Platform"}], "num_subtasks": 2,
    "notes": "Steps:\n1. open form\n2. boom", "tags": [{"gid": "77", "name": "bug"}],
    "followers": [{"gid": "9", "name": "Ashik"}],
    "custom_fields": [{"gid": "555", "name": "Priority", "display_value": "High"},
                      {"gid": "556", "name": "Empty", "display_value": None}],
    "dependencies": [{"gid": "1112"}], "created_at": "2025-01-01T00:00:00Z",
}


class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def _send(self, code, payload):
        body = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _read(self):
        n = int(self.headers.get("Content-Length") or 0)
        return self.rfile.read(n) if n else b""

    def handle_any(self, method):
        u = urlsplit(self.path)
        p, q = u.path, parse_qs(u.query)
        raw = self._read()
        ctype = self.headers.get("Content-Type", "")
        if "multipart" in ctype:
            body = {"_multipart_bytes": len(raw), "_has_file": b'name="file"' in raw,
                    "_parent": b'name="parent"' in raw}
        else:
            try:
                body = json.loads(raw)["data"] if raw else None
            except Exception:
                body = raw.decode(errors="replace")
        LOG.append({"m": method, "p": p, "q": {k: v[0] for k, v in q.items()}, "body": body})

        if self.headers.get("Authorization") != "Bearer TESTTOKEN":
            return self._send(401, {"errors": [{"message": "Not Authorized"}]})

        # --- reads
        if p == "/api/1.0/users/me":
            return self._send(200, {"data": {"gid": "9", "name": "Ashik", "email": "a@b.c",
                                             "workspaces": [{"gid": "100", "name": "Manzil"}]}})
        if p == "/api/1.0/workspaces":
            return self._send(200, {"data": [{"gid": "100", "name": "Manzil"}]})
        if p == "/api/1.0/projects":
            return self._send(200, {"data": [{"gid": "222", "name": "Platform"},
                                             {"gid": "223", "name": "Mobile KYC"}]})
        if re.fullmatch(r"/api/1\.0/projects/\d+", p):
            return self._send(200, {"data": {"gid": "222", "name": "Platform",
                                             "owner": {"name": "Ashik"}, "notes": "board"}})
        if p.endswith("/sections") :
            return self._send(200, {"data": [{"gid": "333", "name": "In Progress"},
                                             {"gid": "334", "name": "Done"}]})
        if p.endswith("/custom_field_settings"):
            return self._send(200, {"data": [{"custom_field": {
                "gid": "555", "name": "Priority", "resource_subtype": "enum",
                "enum_options": [{"gid": "5551", "name": "High", "enabled": True},
                                 {"gid": "5552", "name": "Low", "enabled": True}]}}]})
        if re.fullmatch(r"/api/1\.0/custom_fields/\d+", p):
            return self._send(200, {"data": {
                "gid": "555", "resource_subtype": "enum",
                "enum_options": [{"gid": "5551", "name": "High"}, {"gid": "5552", "name": "Low"}]}})
        if p == "/api/1.0/tasks" and method == "GET":
            # exercise pagination: first page returns next_page
            if "offset" not in q:
                return self._send(200, {"data": [TASK], "next_page": {"offset": "pg2"}})
            return self._send(200, {"data": [dict(TASK, gid="1112", name="Done thing",
                                                  completed=True)]})
        if re.fullmatch(r"/api/1\.0/sections/\d+/tasks", p):
            return self._send(200, {"data": [TASK]})
        if re.fullmatch(r"/api/1\.0/tasks/\d+", p) and method == "GET":
            return self._send(200, {"data": TASK})
        if p.endswith("/subtasks") and method == "GET":
            return self._send(200, {"data": [dict(TASK, gid="1113", name="Sub A")]})
        if p.endswith("/stories") and method == "GET":
            return self._send(200, {"data": [
                {"gid": "s1", "text": "first comment\nsecond line", "created_at": "2025-01-02T10:00:00Z",
                 "created_by": {"name": "Ashik"}, "resource_subtype": "comment_added"},
                {"gid": "s2", "text": "changed the due date", "created_at": "2025-01-03T10:00:00Z",
                 "created_by": {"name": "Bot"}, "resource_subtype": "due_date_changed"}]})
        if p.endswith("/tasks/search"):
            return self._send(402, {"errors": [{"message": "upgrade required"}]})
        if p.endswith("/typeahead"):
            return self._send(200, {"data": [TASK]})
        if p.endswith("/users") and method == "GET":
            return self._send(200, {"data": [{"gid": "9", "name": "Ashik", "email": "a@b.c"},
                                             {"gid": "10", "name": "Ali", "email": "ali@b.c"}]})
        if p.endswith("/tags") and method == "GET":
            return self._send(200, {"data": [{"gid": "77", "name": "bug"}]})
        if p == "/api/1.0/attachments" and method == "GET":
            return self._send(200, {"data": [{"gid": "a1", "name": "shot.png",
                                              "created_at": "2025-01-01"}]})

        # --- writes
        if p == "/api/1.0/tasks" and method == "POST":
            return self._send(201, {"data": dict(TASK, gid="2222", name=(body or {}).get("name"))})
        if re.fullmatch(r"/api/1\.0/tasks/\d+", p) and method == "PUT":
            return self._send(200, {"data": dict(TASK, **{k: v for k, v in (body or {}).items()
                                                          if k in ("name", "completed")})})
        if re.fullmatch(r"/api/1\.0/tasks/\d+", p) and method == "DELETE":
            return self._send(200, {"data": {}})
        if p.endswith("/stories") and method == "POST":
            return self._send(201, {"data": {"gid": "s9"}})
        if p == "/api/1.0/attachments" and method == "POST":
            return self._send(200, {"data": {"gid": "a9", "name": "shot.png"}})
        if method == "POST":  # addTask/addProject/addTag/addFollowers/addDependencies/subtasks
            return self._send(200, {"data": dict(TASK, gid="3333")})
        return self._send(404, {"errors": [{"message": f"no mock for {method} {p}"}]})

    def do_GET(self):
        self.handle_any("GET")

    def do_POST(self):
        self.handle_any("POST")

    def do_PUT(self):
        self.handle_any("PUT")

    def do_DELETE(self):
        self.handle_any("DELETE")


if __name__ == "__main__":
    import atexit
    srv = HTTPServer(("127.0.0.1", 8731), H)
    atexit.register(lambda: open("/tmp/asana-mock/requests.json", "w").write(json.dumps(LOG, indent=1)))
    print("ready", flush=True)
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        pass
