# Drive the running app in headless Chrome from the command line, one action per
# call, for agents and quick manual checks. Chrome stays open between calls (its
# DevTools port and pid are kept in a state directory), so app state carries
# over from one command to the next. Not a test: tests/e2e/smoke.R is the
# scripted browser check.
#
# Usage (from the repo root): Rscript dev/ui_drive.R <command> [args]
#   app-start [port]        start the app in the background (default port 8765)
#   app-stop                stop the app started by app-start
#   open <url>              start Chrome if needed and load <url>
#   wait [text] [secs]      wait until Shiny is idle (and <text> is on the page)
#   controls [css]          list visible controls: tag, id, label, value
#   text [css]              print the visible text of <css> (default main)
#   click <css>             click the first element matching <css>
#   click-text <text>       click a tab, button, link or label by its text
#   set <css> <value>       set an input (selectize, select, text, number, radio)
#   js <expr>               evaluate a JavaScript expression and print the result
#   shot <file.png> [css]   screenshot the viewport, or one element
#   size <width> <height>   set the viewport (default 1440 x 900)
#   errors                  print JS errors, Shiny output errors, notifications
#   close                   close Chrome and clear the state directory
#
# Environment: WISEAPP_UI_STATE (state directory, default <tmp>/wiseapp-ui),
# WISEAPP_UI_PORT (Chrome DevTools port, default 9333).

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: Rscript dev/ui_drive.R <command> [args]; see the header.", call. = FALSE)
}
cmd <- args[[1L]]
arg <- function(i, default = NULL) if (length(args) > i) args[[i + 1L]] else default

state_dir <- Sys.getenv("WISEAPP_UI_STATE", file.path(dirname(tempdir()), "wiseapp-ui"))
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
state_file <- function(name) file.path(state_dir, name)
read_state <- function(name) {
  f <- state_file(name)
  if (file.exists(f)) readLines(f, warn = FALSE)[1L] else NA_character_
}
cdp_port <- as.integer(Sys.getenv("WISEAPP_UI_PORT", "9333"))

pid_alive <- function(pid) {
  !is.na(pid) && isTRUE(tryCatch(
    processx::process$new("kill", c("-0", pid))$wait()$get_exit_status() == 0L,
    error = function(e) FALSE
  ))
}

url_up <- function(url) {
  isTRUE(tryCatch({
    con <- url(url)
    on.exit(close(con))
    readLines(con, n = 1L, warn = FALSE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}

# App ----

app_start <- function(port) {
  pid <- read_state("app.pid")
  if (pid_alive(pid)) {
    cat("app already running: ", read_state("app.url"), " (pid ", pid, ")\n", sep = "")
    return(invisible())
  }
  log <- state_file("app.log")
  code <- sprintf(paste(
    "pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE);",
    "options(golem.app.prod = TRUE);",
    "shiny::runApp(wiseapp::run_app(), port = %dL, launch.browser = FALSE)"
  ), port)
  p <- processx::process$new(
    file.path(R.home("bin"), "Rscript"), c("-e", code),
    wd = getwd(), stdout = log, stderr = "2>&1", cleanup = FALSE
  )
  url <- sprintf("http://127.0.0.1:%d/", port)
  writeLines(as.character(p$get_pid()), state_file("app.pid"))
  writeLines(url, state_file("app.url"))
  for (i in seq_len(240L)) {
    if (url_up(url)) {
      cat("app running: ", url, "\nlog: ", log, "\n", sep = "")
      return(invisible())
    }
    if (!p$is_alive()) break
    Sys.sleep(0.5)
  }
  cat(utils::tail(readLines(log, warn = FALSE), 30L), sep = "\n")
  stop("app did not start; log: ", log, call. = FALSE)
}

app_stop <- function() {
  pid <- read_state("app.pid")
  if (pid_alive(pid)) tools::pskill(as.integer(pid))
  unlink(state_file(c("app.pid", "app.url")))
  cat("app stopped\n")
}

# Browser ----

chrome_start <- function() {
  if (pid_alive(read_state("chrome.pid")) &&
      url_up(sprintf("http://127.0.0.1:%d/json/version", cdp_port))) {
    return(invisible())
  }
  p <- processx::process$new(
    chromote::find_chrome(),
    c("--headless=new", sprintf("--remote-debugging-port=%d", cdp_port),
      paste0("--user-data-dir=", state_file("profile")),
      "--window-size=1440,900", "--no-first-run", "--no-default-browser-check",
      "--use-angle=swiftshader", "--enable-unsafe-swiftshader", "about:blank"),
    stdout = state_file("chrome.log"), stderr = "2>&1", cleanup = FALSE
  )
  writeLines(as.character(p$get_pid()), state_file("chrome.pid"))
  for (i in seq_len(40L)) {
    if (url_up(sprintf("http://127.0.0.1:%d/json/version", cdp_port))) return(invisible())
    Sys.sleep(0.25)
  }
  stop("Chrome did not start; log: ", state_file("chrome.log"), call. = FALSE)
}

# Attach to the existing page so the app session survives between calls. Do not
# call s$close(): that would close the tab.
connect <- function() {
  if (!url_up(sprintf("http://127.0.0.1:%d/json/version", cdp_port))) {
    stop("Chrome is not running; run `open <url>` first.", call. = FALSE)
  }
  b <- chromote::Chromote$new(
    browser = chromote::ChromeRemote$new(host = "127.0.0.1", port = cdp_port)
  )
  pages <- Filter(function(t) identical(t$type, "page"), b$Target$getTargets()$targetInfos)
  id <- if (length(pages)) pages[[1L]]$targetId else b$Target$createTarget("about:blank")$targetId
  s <- chromote::ChromoteSession$new(parent = b, targetId = id)
  w <- as.integer(read_state("width")); h <- as.integer(read_state("height"))
  s$Emulation$setDeviceMetricsOverride(
    width = if (is.na(w)) 1440L else w, height = if (is.na(h)) 900L else h,
    deviceScaleFactor = 1, mobile = FALSE
  )
  s
}

js <- function(s, expr, timeout = 30) {
  r <- s$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE,
                          timeout_ = timeout)
  if (!is.null(r$exceptionDetails)) {
    stop("JS error: ", r$exceptionDetails$exception$description %||%
           r$exceptionDetails$text, call. = FALSE)
  }
  r$result$value
}
`%||%` <- function(x, y) if (is.null(x)) y else x
q <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)

# Records uncaught errors from the moment the page loads.
error_hook <- paste(
  "window.__wiseErrors = [];",
  "window.addEventListener('error', e => window.__wiseErrors.push(String(e.message)));",
  "window.addEventListener('unhandledrejection', e => window.__wiseErrors.push(String(e.reason)));"
)

wait_idle <- function(s, text = NULL, secs = 60) {
  check <- sprintf(paste0(
    "(() => { const ok = !!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());",
    " const busy = document.documentElement.classList.contains('shiny-busy');",
    " const t = %s; return ok && !busy && (!t || document.body.innerText.includes(t)); })()"
  ), q(text %||% ""))
  streak <- 0L
  deadline <- Sys.time() + secs
  while (Sys.time() < deadline) {
    streak <- if (isTRUE(js(s, check))) streak + 1L else 0L
    if (streak >= 3L) return(invisible(TRUE))
    Sys.sleep(0.3)
  }
  invisible(FALSE)
}

controls_js <- "(sel => {
  const root = document.querySelector(sel) || document.body;
  const vis = e => !!(e.offsetParent || e.getClientRects().length);
  const lab = e => ((e.labels && e.labels[0] && e.labels[0].innerText) ||
    e.getAttribute('aria-label') || e.innerText || e.value || '').trim().replace(/\\s+/g, ' ').slice(0, 60);
  const out = [];
  root.querySelectorAll('select.selectized').forEach(e => {
    const w = e.nextElementSibling;
    const l = document.getElementById(e.id + '-label');
    if (w && vis(w)) out.push('selectize #' + e.id + ' \"' + (l ? lab(l) : '') + '\" = ' + JSON.stringify(e.selectize.getValue()) +
      ' options=' + JSON.stringify(Object.keys(e.selectize.options).slice(0, 25)));
  });
  root.querySelectorAll('input:not(.selectized):not([type=hidden]), select:not(.selectized), textarea, button, a.nav-link, [role=tab], .accordion-button').forEach(e => {
    if (!vis(e) || e.closest('.selectize-control')) return;
    let v = '';
    if (e.type === 'checkbox' || e.type === 'radio') v = e.checked ? ' [x]' : ' [ ]';
    else if ('value' in e && e.tagName !== 'BUTTON' && e.value !== '') v = ' = ' + JSON.stringify(e.value);
    if (e.classList.contains('active') || e.getAttribute('aria-selected') === 'true') v += ' (active)';
    if (e.getAttribute('aria-expanded') === 'false') v += ' (collapsed)';
    if (e.disabled) v += ' (disabled)';
    out.push(e.tagName.toLowerCase() + (e.type && e.tagName === 'INPUT' ? '[' + e.type + ']' : '') +
      (e.id ? ' #' + e.id : '') + (e.name && e.type === 'radio' ? ' name=' + e.name : '') + ' \"' + lab(e) + '\"' + v);
  });
  return out.join('\\n');
})"

click_text_js <- "(t => {
  const vis = e => !!(e.offsetParent || e.getClientRects().length);
  const cands = [...document.querySelectorAll('[role=tab], a, button, label, .accordion-button, summary')].filter(vis);
  const norm = e => (e.innerText || '').trim().replace(/\\s+/g, ' ');
  const hit = cands.find(e => norm(e) === t) || cands.find(e => norm(e).includes(t));
  if (!hit) return 'no visible element with text: ' + t;
  hit.scrollIntoView({block: 'center'}); hit.click();
  return 'clicked ' + hit.tagName.toLowerCase() + ' \"' + norm(hit).slice(0, 60) + '\"';
})"

set_js <- "((sel, v) => {
  const e = document.querySelector(sel);
  if (!e) return 'no element: ' + sel;
  if (e.selectize) {
    const vals = Array.isArray(v) ? v : [v];
    const opts = e.selectize.options;
    const keys = vals.map(x => x in opts ? x : (Object.keys(opts).find(k => opts[k].label === x || opts[k].text === x) || x));
    e.selectize.setValue(e.multiple ? keys : keys[0]);
    return 'set selectize #' + e.id + ' = ' + JSON.stringify(e.selectize.getValue());
  }
  if (e.type === 'radio' || e.type === 'checkbox' || e.classList.contains('shiny-input-radiogroup') || e.classList.contains('shiny-input-checkboxgroup')) {
    const box = e.type ? e.closest('.shiny-input-container') || e.parentElement : e;
    const r = [...box.querySelectorAll('input')].find(i => i.value === v ||
      (i.parentElement && i.parentElement.innerText.trim() === v));
    if (!r) return 'no option ' + v + ' in ' + sel;
    r.click(); return 'clicked option ' + r.value;
  }
  e.value = v;
  e.dispatchEvent(new Event('input', {bubbles: true}));
  e.dispatchEvent(new Event('change', {bubbles: true}));
  return 'set ' + e.tagName.toLowerCase() + ' #' + e.id + ' = ' + JSON.stringify(e.value);
})"

errors_js <- "(() => {
  const vis = e => !!(e.offsetParent || e.getClientRects().length);
  const out = (window.__wiseErrors || []).map(m => 'js: ' + m);
  document.querySelectorAll('.shiny-output-error').forEach(e => { if (vis(e)) out.push('output #' + (e.id || '?') + ': ' + e.innerText.trim().slice(0, 300)); });
  document.querySelectorAll('.shiny-notification').forEach(e => out.push('notification: ' + e.innerText.trim().slice(0, 300)));
  return out.length ? out.join('\\n') : 'no errors';
})()"

# Commands ----

switch(cmd,
  "app-start" = app_start(as.integer(arg(1L, "8765"))),
  "app-stop" = app_stop(),
  "open" = {
    url <- arg(1L, read_state("app.url"))
    if (is.na(url)) stop("open needs a URL (or app-start first).", call. = FALSE)
    chrome_start()
    s <- connect()
    s$Page$addScriptToEvaluateOnNewDocument(error_hook)
    s$Page$navigate(url, wait_ = FALSE)
    Sys.sleep(1)
    ok <- wait_idle(s, secs = 120)
    cat(if (ok) "loaded " else "loaded (Shiny not idle after 120 s) ",
        js(s, "document.title"), " ", url, "\n", sep = "")
  },
  "wait" = {
    ok <- wait_idle(connect(), arg(1L), as.numeric(arg(2L, "60")))
    cat(if (ok) "idle\n" else "timeout\n")
    if (!ok) quit(status = 1L)
  },
  "controls" = cat(js(connect(), sprintf("%s(%s)", controls_js, q(arg(1L, "body")))), "\n"),
  "text" = {
    t <- js(connect(), sprintf(
      "(document.querySelector(%s) || document.body).innerText", q(arg(1L, "main"))
    ))
    cat(substr(t %||% "", 1L, 6000L), "\n")
  },
  "click" = {
    s <- connect()
    cat(js(s, sprintf(paste0(
      "(() => { const e = document.querySelector(%s); if (!e) return 'no element';",
      " e.scrollIntoView({block: 'center'}); e.click(); return 'clicked'; })()"
    ), q(arg(1L)))), "\n")
    wait_idle(s, secs = 30)
  },
  "click-text" = {
    s <- connect()
    cat(js(s, sprintf("%s(%s)", click_text_js, q(arg(1L)))), "\n")
    wait_idle(s, secs = 30)
  },
  "set" = {
    s <- connect()
    cat(js(s, sprintf("%s(%s, %s)", set_js, q(arg(1L)), q(arg(2L)))), "\n")
    wait_idle(s, secs = 30)
  },
  "js" = print(js(connect(), arg(1L))),
  "shot" = {
    s <- connect()
    file <- normalizePath(arg(1L, state_file("shot.png")), mustWork = FALSE)
    sel <- arg(2L)
    if (is.null(sel)) {
      png <- s$Page$captureScreenshot(format = "png")$data
      writeBin(jsonlite::base64_dec(png), file)
    } else {
      s$screenshot(file, selector = sel, scale = 1, show = FALSE)
    }
    cat("saved ", file, "\n", sep = "")
  },
  "size" = {
    writeLines(arg(1L, "1440"), state_file("width"))
    writeLines(arg(2L, "900"), state_file("height"))
    invisible(connect())
    cat("viewport ", arg(1L, "1440"), " x ", arg(2L, "900"), "\n", sep = "")
  },
  "errors" = cat(js(connect(), errors_js), "\n"),
  "close" = {
    pid <- read_state("chrome.pid")
    if (pid_alive(pid)) tools::pskill(as.integer(pid))
    unlink(state_file(c("chrome.pid", "chrome.log", "width", "height")))
    unlink(state_file("profile"), recursive = TRUE)
    cat("browser closed\n")
  },
  stop("Unknown command: ", cmd, call. = FALSE)
)
