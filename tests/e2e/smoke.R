# Browser smoke test (CR-OPS-01, CR-A11Y-09): boots the real app in headless
# Chrome through shinytest2, checks one conditionalPanel and runs axe-core on
# the first screens. It lives outside tests/testthat on purpose: R CMD check and
# testthat::test_local() do not run it, and the CI job is non-blocking.
#
# Usage (from the repo root):
#   Rscript tests/e2e/smoke.R [path/to/axe.min.js]
# Without an argument axe-core is downloaded from cdnjs (pinned version).
# Set WISEAPP_AXE_FAIL=1 to fail on serious or critical violations.

stopifnot(file.exists("DESCRIPTION"), file.exists("app.R"))

axe_version <- "4.10.2"
args <- commandArgs(trailingOnly = TRUE)
axe_file <- if (length(args) >= 1L) args[[1L]] else {
  f <- tempfile(fileext = ".js")
  utils::download.file(
    sprintf("https://cdnjs.cloudflare.com/ajax/libs/axe-core/%s/axe.min.js", axe_version),
    f, quiet = TRUE
  )
  f
}
axe_src <- paste(readLines(axe_file, warn = FALSE), collapse = "\n")
out_dir <- Sys.getenv("WISEAPP_E2E_OUT", file.path("tests", "e2e", "out"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(NOT_CRAN = "true", WISEAPP_DATA_SOURCE = "", WISEAPP_ASYNC_SYNC = "1")
app <- shinytest2::AppDriver$new(
  ".", name = "smoke", load_timeout = 90 * 1000, timeout = 30 * 1000,
  options = list(shiny.testmode = FALSE)
)
on.exit(app$stop(), add = TRUE)

failures <- character()
check <- function(ok, label) {
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n", sep = "")
  if (!isTRUE(ok)) failures <<- c(failures, label)
}

# Boot ----
errors <- app$get_logs()
errors <- errors[errors$level == "error" & errors$location == "shiny", , drop = FALSE]
check(nrow(errors) == 0L, "no Shiny errors in the log after boot")
tabs <- app$get_js("[...document.querySelectorAll('.navbar .nav-link')].map(e => e.textContent.trim())")
check(any(grepl("Overview", tabs)) && any(grepl("Step 3", tabs)), "navbar shows Overview and Step 3")
check(isTRUE(app$get_js("!!document.querySelector('#main-content')")), "main landmark present")

# axe-core on one screen ----
# Known upstream issue, filtered out below: bslib gives the navbar role=tablist
# while it also holds plain links (Docs, Data, GitHub) and a dropdown, so axe
# reports aria-required-children on `.navbar-nav`. Not fixable without
# dropping the tab semantics.
run_axe <- function(label) {
  app$run_js(axe_src)
  res <- app$get_js(
    "axe.run(document, {runOnly: {type: 'tag', values: ['wcag2a','wcag2aa','wcag21aa','wcag22aa']}}).then(r => JSON.stringify(r.violations.map(v => (v.id === 'aria-required-children' ? Object.assign({}, v, {nodes: v.nodes.filter(n => n.target.join(' ') !== '.navbar-nav')}) : v)).filter(v => v.nodes.length).map(v => ({id: v.id, impact: v.impact, help: v.help, nodes: v.nodes.length, target: v.nodes[0].target.join(' ')}))))",
    timeout = 60 * 1000
  )
  v <- jsonlite::fromJSON(res)
  writeLines(res, file.path(out_dir, sprintf("axe-%s.json", label)))
  if (length(v) == 0L) {
    cat("axe ", label, ": no violations\n", sep = "")
    return(invisible(0L))
  }
  cat("axe ", label, ": ", nrow(v), " violation type(s)\n", sep = "")
  print(v[, c("id", "impact", "nodes", "help")], row.names = FALSE)
  invisible(sum(v$impact %in% c("serious", "critical")))
}
bad <- run_axe("overview")

# conditionalPanel: the coefficient-uncertainty sub-option follows its parent ----
app$run_js("document.querySelector('.navbar .nav-link[data-value=\"step2\"]').click()")
Sys.sleep(1.5)
# The settings block is itself behind a toggle (a conditionalPanel on its click count).
app$run_js("document.querySelector(\"[id$='weathersim-settings_toggle']\").click()")
Sys.sleep(1)
sel_parent <- "input[id$='include_coef_uncertainty']"
sel_child <- "input[id$='propagate_all_covariate_uncertainty']"
visible <- function(sel) app$get_js(sprintf(
  "(function(){var e = document.querySelector(\"%s\"); return !!e && e.offsetParent !== null;})()", sel
))
check(isTRUE(app$get_js(sprintf("!!document.querySelector(\"%s\")", sel_parent))), "Step 2 coefficient-uncertainty checkbox renders")
check(visible(sel_child), "sub-option visible while the parent is checked")
app$run_js(sprintf("document.querySelector(\"%s\").click()", sel_parent))
Sys.sleep(0.5)
check(!visible(sel_child), "sub-option hidden after the parent is unchecked")
bad <- bad + run_axe("step2")

if (length(failures)) {
  stop("Smoke checks failed: ", paste(failures, collapse = "; "), call. = FALSE)
}
if (identical(Sys.getenv("WISEAPP_AXE_FAIL"), "1") && bad > 0L) {
  stop(bad, " serious or critical axe violation type(s).", call. = FALSE)
}
cat("smoke ok\n")
