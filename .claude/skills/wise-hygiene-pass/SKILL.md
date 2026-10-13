---
name: wise-hygiene-pass
description: >
  Run a code hygiene pass over R/ in WISE-APP: readability and structural
  consistency with zero behavioral change, following review/code_hygiene.md.
  Covers batch order, baseline tests, protected areas, escalation and the
  per-batch report.
when_to_use: >
  When the user asks for a hygiene, cleanup, style, lint, dead-code or
  consistency pass over R/ files, or asks to bring new code in line with the
  repo conventions without changing behavior.
user-invocable: true
allowed-tools: Read Grep Glob Edit Bash
argument-hint: "[batch: utils | fct | mod_1 | mod_2 | mod_3 | app | <file>]"
---

# Hygiene pass for WISE-APP

`review/code_hygiene.md` is the source of truth. Read it in full before
starting; this skill is the operating checklist. `AGENTS.md` and the user's
global rules win over both. The first full pass is archived in
`review/archive/golem_code_hygiene_progress.md`; later passes are incremental.

A hygiene pass changes no behavior, no UI layout, no input or output IDs and
no reactive logic. If a cleanup might, it is not hygiene: flag it.

## Scope

- Edit: `R/` (`app_*.R`, `mod_*.R`, `fct_*.R`, `utils_*.R`, `run_app.R`),
  roxygen blocks, and `DESCRIPTION` `Imports:` only with approval.
- Do not edit: `batch/`, `dev/`, `inst/`, `review/`, non-app scripts. `tests/`
  is run, never edited.
- Unsure what a file is for: do not edit it, list it in the report.

## Protected areas: whitespace and comments only

- Numerics: `fct_run_simulation.R`, `fct_step2_compute.R`,
  `fct_policy_decompose.R`, `fct_policy_metric_decompose.R`,
  `fct_decomposition_summary.R`.
- Async: `fct_step2_async.R` and anything serialized to mirai workers.
  Functions, globals and arguments can be captured by name, so "unused"
  cannot be proven by reading.
- Contract surface: exported functions, `ENGINE_REGISTRY` and metric-registry
  fields, `.wiseapp_*` columns, any identifier used in `inst/` JS,
  `Shiny.setInputValue` or `tests/`, and the `year` type contract.

Anything beyond whitespace and comments in these areas is flagged, not applied.

## Conventions to apply

- Native pipe `|>` only; convert any `%>%` in files you modify. `<-` for
  assignment, `=` only for arguments.
- No `library()` or `require()` in `R/`.
- `@importFrom` for heavily used core functions of packages already in
  `Imports:`; inline `pkg::fn()` for infrequent or collision-prone calls
  (`stats`, `collapse`, `kit`, utilities).
- Section headers `# Title ----`. Modules: `# UI ----`, `# Server ----`,
  `# Reactives ----`, `# Handlers ----`, `# Helpers ----`, only those needed.
  `fct_`/`utils_`: `# <Topic> ----`. If a file's headers are already
  internally consistent, keep them and report the deviation; no churn edits.
- Comments: remove outdated ones; add short ones only for non-obvious logic.
  Preserve comments that cite `review/` plans, contract notes or issue IDs
  (for example `CR-CQ-07`, `R2-PERF-04`). ASCII quotes and hyphens only.
- Tidyverse style, 2-space indent, lines up to 120 characters (`.lintr`
  disables `object_name_linter`). Rename to `snake_case` only for locals
  inside one function; never exported names, registry fields, column names or
  IDs.
- Dead code: delete unused locals, commented-out code, stray console
  printing. Unreferenced functions and unused arguments are flag-first. Keep
  `message()` diagnostics and `cat()` inside `renderPrint`.
- Logic with no Shiny dependency belongs in `fct_`/`utils_`. Moving it is a
  refactor: list candidates, do not move them.

## Procedure

1. **Clean tree.** `git status`. Skip any target file with uncommitted changes
   or ask the user to commit or stash. Work on a dedicated branch (`git checkout
   -b ...` is allowed). Do not commit or push unless asked.
2. **Baseline.** `devtools::test()` (about 4 to 6 minutes serial; run in the
   background). Record pass/fail per file. Failures at baseline and tests that
   fail for environment reasons (credentials, network) are recorded and
   excluded from regression checks. Also run `Rscript tests/spelling.R`.
   Never accept or update snapshots; `git status` on `tests/` must be empty
   at the end.
3. **Plan.** Read the target files. Write a short plan per file so the
   conventions are applied the same way across files.
4. **Batch.** Order: `utils_*`, `fct_*`, `mod_1_*`, `mod_2_*`, `mod_3_*`, then
   `app_*` and `run_app.R`. Split a batch if the diff would exceed about 400
   changed lines.
   - Use `lintr::lint()` to find issues. Use `styler::style_file()` on one
     file first and check the diff size; skip files where it reflows
     unrelated code.
   - Preserve each file's line endings. `git diff --check` must be clean
     (CRLF or mixed endings).
5. **Dead-code search before any removal.** Search `R/`, `tests/`, `inst/app/`
   JS, `dev/`, `batch/`, `NAMESPACE`, and string references. Include dynamic
   lookups: `do.call`, `get()`, `match.fun`, registry lookups, NSE, tidy
   eval, ids referenced only as strings.
6. **Save a patch.** After editing and before testing, save `git diff <files>`
   to the scratchpad directory.
7. **Verify.** `devtools::test()` and `Rscript tests/spelling.R`. If roxygen
   changed, run `devtools::document()` with the roxygen2 version in
   `DESCRIPTION` (`RoxygenNote`, 7.3.3; 8.x rewrites the NAMESPACE layout) and
   confirm `NAMESPACE` and `man/` diffs contain only intended changes.
   - Comment-only and whitespace-only batches: also check that every changed
     file parses to an expression identical to HEAD (compare `parse()` output
     with `keep.source = FALSE` between HEAD and the working copy).
   - A previously passing test now fails: revert that change
     (`git restore <file>` or `git apply -R` on the saved patch) and flag it.
8. **Stop for user review** after each batch. The user commits each batch.

## Escalate instead of applying

- A deletion cannot be confirmed safe by static reading.
- A reactive, observer or event chain would need reordering.
- `DESCRIPTION` `Imports:` would change: list the proposed diff and reason.
- The touched file or function has no test coverage: say so, because that
  section relied on manual review.

## Report after each batch and at the end

1. **Test status:** baseline against current pass/fail, with the exact
   command and output summary.
2. **Per-file changelog:** short bullets.
3. **Flagged items:** everything paused under Escalation, misplaced-logic
   candidates, skipped files, with reasons.
4. **Coverage note:** count and list of touched files with no dedicated test
   file (`tests/testthat/test-<R file>.R` or a concept-named file).
5. **Files touched:** modified paths.

State what was not run or failed. Do not report success on checks that were
skipped.
