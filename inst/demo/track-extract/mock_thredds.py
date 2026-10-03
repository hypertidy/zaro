# mock_thredds.py -- stand-in for the THREDDS fileServer, for dry runs only.
# Answers Range GETs with a synthetic temp chunk (shuffle + zlib, like the
# real NetCDF chunks), padded to the requested length. Values encode the
# global index: v = (x + 3*y + 5*z + 7*t) % 30000, so the extracted track
# can be checked exactly. Adds a fixed latency and records the peak number
# of concurrent requests. GET /stats returns JSON counters.
import json, sys, threading, time, zlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs
import numpy as np

LATENCY = float(sys.argv[2]) if len(sys.argv) > 2 else 0.25
lock = threading.Lock()
st = {"requests": 0, "active": 0, "peak": 0, "bytes": 0}

def chunk_bytes(t, z, cy, cx):
    y = cy * 300 + np.arange(300)[:, None]
    x = cx * 300 + np.arange(300)[None, :]
    v = ((x + 3 * y + 5 * z + 7 * t) % 30000).astype("<i2")
    b = np.frombuffer(v.tobytes(), dtype=np.uint8).reshape(-1, 2)
    return zlib.compress(b.T.tobytes(), 1)  # shuffle, then zlib

class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass
    def do_GET(self):
        u = urlparse(self.path)
        if u.path == "/stats":
            body = json.dumps(st).encode()
            self.send_response(200); self.send_header("Content-Length", str(len(body)))
            self.end_headers(); self.wfile.write(body); return
        with lock:
            st["requests"] += 1; st["active"] += 1
            st["peak"] = max(st["peak"], st["active"])
        try:
            time.sleep(LATENCY)
            q = parse_qs(u.query)
            t, z, cy, cx = (int(s) for s in q["c"][0].split("."))
            a, b = self.headers["Range"].split("=")[1].split("-")
            n = int(b) - int(a) + 1
            body = chunk_bytes(t, z, cy, cx)
            body = body + b"\0" * max(0, n - len(body))
            body = body[:n] if len(body) > n else body
            with lock:
                st["bytes"] += len(body)
            self.send_response(206)
            self.send_header("Content-Range", "bytes %s-%s/*" % (a, b))
            self.send_header("Content-Length", str(len(body)))
            self.end_headers(); self.wfile.write(body)
        finally:
            with lock:
                st["active"] -= 1

ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), H).serve_forever()
