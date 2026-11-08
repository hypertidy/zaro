# A small local HTTP server for the fetcher tests: serves files under `dir`
# with single byte-range support, 404 for missing files, a 503 the first
# time any path containing "flaky" is requested, an optional delay per
# request, and GET /_stats -> {"requests": n, "max_active": k}. Needs
# python3; tests skip without it. ASCII only.

range_server_py <- '
import http.server, json, os, re, sys, threading, time
root, port, delay = sys.argv[1], int(sys.argv[2]), float(sys.argv[3])
lock = threading.Lock()
st = {"requests": 0, "active": 0, "max_active": 0, "flaky": set()}
class H(http.server.BaseHTTPRequestHandler):
    def log_message(self, *a): pass
    def do_GET(self):
        if self.path == "/_stats":
            b = json.dumps({"requests": st["requests"], "max_active": st["max_active"]}).encode()
            self.send_response(200); self.send_header("Content-Length", str(len(b)))
            self.end_headers(); self.wfile.write(b); return
        with lock:
            st["requests"] += 1; st["active"] += 1
            st["max_active"] = max(st["max_active"], st["active"])
        try:
            time.sleep(delay)
            p = os.path.join(root, self.path.lstrip("/").split("?")[0])
            if "flaky" in self.path and self.path not in st["flaky"]:
                st["flaky"].add(self.path)
                self.send_response(503); self.send_header("Content-Length", "0"); self.end_headers(); return
            if not os.path.isfile(p):
                self.send_response(404); self.send_header("Content-Length", "0"); self.end_headers(); return
            data = open(p, "rb").read()
            m = re.match(r"bytes=(\\d+)-(\\d+)", self.headers.get("Range", ""))
            sfx = re.match(r"bytes=-(\\d+)$", self.headers.get("Range", ""))
            if sfx and "nosuffix" not in self.path:
                n = min(int(sfx.group(1)), len(data)); s, e = len(data) - n, len(data) - 1
                body = data[s:]; self.send_response(206)
                self.send_header("Content-Range", "bytes %d-%d/%d" % (s, e, len(data)))
            elif m:
                s, e = int(m.group(1)), min(int(m.group(2)), len(data) - 1)
                body = data[s:e + 1]; self.send_response(206)
                self.send_header("Content-Range", "bytes %d-%d/%d" % (s, e, len(data)))
            else:
                body = data; self.send_response(200)
            self.send_header("Content-Length", str(len(body))); self.end_headers(); self.wfile.write(body)
        finally:
            with lock: st["active"] -= 1
http.server.ThreadingHTTPServer(("127.0.0.1", port), H).serve_forever()
'

#' Start the server on a free port; returns list(url, stats(), stop()).
#' Callers stop it: on.exit(srv$stop(), add = TRUE)
local_range_server <- function(dir, delay = 0) {
  py <- Sys.which("python3")
  testthat::skip_if(!nzchar(py), "python3 not available")
  script <- tempfile(fileext = ".py")
  writeLines(range_server_py, script)
  port <- NA
  for (try in 1:20) {
    cand <- sample(20000:40000, 1L)
    ok <- tryCatch({
      con <- suppressWarnings(socketConnection("127.0.0.1", cand, open = "r",
                                               timeout = 0.2))
      close(con)
      FALSE                          # something is listening: taken
    }, error = function(e) TRUE)
    if (ok) { port <- cand; break }
  }
  pidfile <- tempfile()
  system2("sh", c("-c", shQuote(sprintf("%s %s %s %s %s >/dev/null 2>&1 & echo $! > %s",
                                        py, shQuote(script), shQuote(dir), port,
                                        delay, shQuote(pidfile)))),
          wait = TRUE)
  pid <- as.integer(readLines(pidfile))
  url <- sprintf("http://127.0.0.1:%d", port)
  for (i in 1:50) {
    up <- tryCatch(curl::curl_fetch_memory(paste0(url, "/_stats"))$status_code == 200L,
                   error = function(e) FALSE)
    if (up) break
    Sys.sleep(0.1)
  }
  testthat::skip_if(!up, "local HTTP server did not start")
  list(
    url = url,
    stats = function() jsonlite::fromJSON(rawToChar(
      curl::curl_fetch_memory(paste0(url, "/_stats"))$content)),
    stop = function() invisible(tools::pskill(pid))
  )
}

## the local server must never go through a proxy
local({
  np <- paste(c(Sys.getenv("no_proxy"), "127.0.0.1", "localhost"), collapse = ",")
  Sys.setenv(no_proxy = sub("^,", "", np), NO_PROXY = sub("^,", "", np))
})
