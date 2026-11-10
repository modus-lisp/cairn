#!/usr/bin/env python3
"""git-http-server.py ROOT -- serve the bare repositories under ROOT over git's smart HTTP
protocol (`git http-backend' as a CGI) on 127.0.0.1, an ephemeral port.  Prints the port on
the first line of stdout, then serves until killed.  For hermetic gates: nothing listens
beyond loopback, and the port is the kernel's choice, so no gate picks a number."""
import http.server, os, subprocess, sys

ROOT = os.path.abspath(sys.argv[1])


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def _cgi(self):
        path, _, query = self.path.partition("?")
        n = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(n) if n else b""
        env = dict(os.environ, GIT_PROJECT_ROOT=ROOT, GIT_HTTP_EXPORT_ALL="1",
                   PATH_INFO=path, QUERY_STRING=query, REQUEST_METHOD=self.command,
                   CONTENT_TYPE=self.headers.get("Content-Type", ""),
                   CONTENT_LENGTH=str(len(body)), REMOTE_ADDR="127.0.0.1")
        out = subprocess.run(["git", "http-backend"], input=body, env=env,
                             capture_output=True).stdout
        head, _, payload = out.partition(b"\r\n\r\n")
        if not _:
            head, _, payload = out.partition(b"\n\n")
        status, headers = 200, []
        for line in head.decode("latin-1").splitlines():
            k, _, v = line.partition(":")
            if k.lower() == "status":
                status = int(v.strip().split()[0])
            elif k:
                headers.append((k, v.strip()))
        self.send_response(status)
        for k, v in headers:
            if k.lower() != "content-length":
                self.send_header(k, v)
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    do_GET = do_POST = _cgi


server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
