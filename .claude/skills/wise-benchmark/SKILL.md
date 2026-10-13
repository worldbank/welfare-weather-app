---
name: wise-benchmark
description: >
  Benchmark a performance-related change to WISE-APP against real data using the
  existing dev/bench_*.R harnesses. Produces before/after wall time and peak
  memory for the small (BFA) and large (IRN) payloads, as required by
  review/optimization_guidelines.md section 9.
when_to_use: >
  When the user asks to benchmark, profile, or compare timing or memory of a
  change, or when a change claims a speed or memory improvement and needs
  evidence before merging.
user-invocable: true
allowed-tools: Read Grep Glob Bash
argument-hint: "[area: step2 | step3 | aggregation | rif | weather | ui]"
---

# Benchmarking a WISE-APP change

Procedure for `review/optimization_guidelines.md` section 9. Read that file's
sections 0 and 9 first; they win over this summary if they differ.

## Rules

- Reuse the harnesses in `dev/`. Do not write ad-hoc timing scripts.
- Benchmark against real data, never `WISEAPP_STEP2_FIXTURE=smoke`. The smoke
  fixture is fabricated and is never production-path evidence.
- Data location comes from the `WISEAPP_DATA_PATH` environment variable (set in
  `.Renviron`). Do not hardcode a path. Check that the variable is set and the
  folder exists (for example `Rscript -e 'nzchar(Sys.getenv("WISEAPP_DATA_PATH"))'`).
  If it is unset or missing, stop and tell the user; do not substitute other
  data. Never print `.Renviron` contents.
- Never print or write credentials. Remote runs (`WISEAPP_STEP2_DATA_SOURCE=databricks`)
  read `DATABRICKS_*` from the environment only.
- Harness output is summaries only. Do not write household-level snapshots
  (`WISEAPP_STEP2_SNAPSHOT_RDS` inputs) into the repo or commit them.
- Write each run to its own directory under `dev/outputs/` (set
  `WISEAPP_STEP2_OUTPUT_DIR`). Never overwrite an existing results directory;
  `dev/outputs/step2-benchmark/` holds earlier baselines.

## Payloads

| Payload | Countries | Notes |
|---|---|---|
| Small | `BFA`, all survey years | Default weather and climate scenario |
| Large | `IRN`, all survey years | Default weather and climate scenario, plus a second run with multiple periods and multiple SSPs (realistic worst case) |

Variables: `WISEAPP_STEP2_COUNTRIES`, `WISEAPP_STEP2_SURVEY_YEARS`,
`WISEAPP_STEP2_PERIODS`, `WISEAPP_STEP2_SSPS`, `WISEAPP_STEP2_REPETITIONS`.
Read the header of `dev/bench_step2.R` for the full list and defaults before
choosing values; confirm the multi-period and multi-SSP values against what the
app offers for IRN.

## Choose the harness

| Area | Harness |
|---|---|
| Step 2 pipeline end to end | `dev/run_step2_benchmark.sh` (wraps `dev/bench_step2.R`, adds process-tree peak RSS) |
| Step 3 policy | `dev/bench_step2.R` with `WISEAPP_STEP2_INCLUDE_STEP3=1` and `WISEAPP_STEP3_POLICIES=...`; helpers in `dev/bench_step3_helpers.R` |
| Aggregation / welfare stats | `dev/bench_aggregation.R`, `dev/bench_surveystats.R`, `dev/profile_agg_hotpath.R` |
| RIF | `dev/bench_rif_kernels.R`, `dev/bench_rif_preparation.R` |
| Prediction | `dev/bench_prediction_core.R` |
| Weather | `dev/bench_w3_weather.R`, `dev/bench_weather_threads.R` |
| Overview metadata | `dev/bench_overview_metadata.R` |
| UI stages | `dev/bench_ui_step1.R`, `dev/bench_ui_step2.R`, `dev/bench_ui_step3.R` |
| Multi-process | `dev/bench_multi_par.R` |

Open the chosen script header to confirm its env vars; they differ per harness.

## Procedure

1. **Plan.** State the change, the area, the harness, and the two payloads.
   Before touching weather, RIF or aggregation, skim the matching archived
   list in `review/archive/` (`optimize_get_weather.md`, `optimize_rif.md`,
   `optimize_aggregation_welfare.md`) for parity risks and traps. They are
   history, not current guidance; verify against the current code.
2. **Clean tree check.** Run `git status`. Identify the baseline commit (usually
   `HEAD` before the change, or the branch point) and the candidate state.
3. **Baseline run.** Check out the baseline in a way that does not lose work:
   use `git stash` or a separate `git worktree`, never `reset --hard` or
   `git clean` (denied in settings). Run the harness for the small payload, then
   the large payload, then the large multi-period multi-SSP case. Use a
   distinct `WISEAPP_STEP2_OUTPUT_DIR` per run, for example
   `dev/outputs/<change>-baseline-bfa`.
4. **Candidate run.** Restore the change and repeat the same three runs with the
   same environment variables, seed (`WISEAPP_STEP2_SEED`) and repetitions.
   Keep everything else equal: same machine, nothing else heavy running, same
   `WISEAPP_THREADS` and DuckDB limits.
5. **Parity.** A faster result that changes numbers is a regression. Compare
   the harness's result summaries between baseline and candidate and report any
   difference. Run the relevant tests (`devtools::test(filter = "...")`) and
   the determinism tests if the change touches numerics.
6. **Report** (format below).

Run long jobs with `run_in_background` and wait for the completion
notification; do not poll.

## Memory

- Use `dev/run_step2_benchmark.sh` for peak RSS. It records the complete
  process tree (main process plus mirai daemons) with `/usr/bin/time -l` on
  macOS and writes `external_process_metrics.csv`. This is the production
  memory gate.
- R-side RSS values are sampled at stage boundaries only. Treat them as
  supporting detail, not the peak.
- `WISEAPP_STEP2_MEMORY_PROFILE=1` serialises every pipeline and slows the
  run. Use it for attribution only, never in a run you compare for timing.
- Related budgets: Step 2 and Step 3 retained qs2 files add roughly 0.4 to
  0.6 GB temp disk per session at a 3x3 Step 2 payload. Mention it if the
  change affects retained artifacts.

## Report format

State plainly what was run and what was not.

1. Change, baseline commit, candidate commit or state, machine, date.
2. Table per payload (BFA, IRN, IRN multi-period multi-SSP):

   | Metric | Baseline | Candidate | Change |
   |---|---|---|---|
   | Wall time, total | | | |
   | Wall time, per stage | | | |
   | Peak RSS (process tree) | | | |

3. Repetitions and spread (not a single run), and the seed.
4. Parity result: identical, or the exact differences.
5. Exact commands and env vars used, and the output directories.
6. Anything not run (missing data, skipped payload, failed run) and why.

## Traps

- The Arrow fetch path (`dbSendQueryArrow()` + `dbFetchArrow()`) was measured
  and not adopted (2026-09-18 investigation). Do not re-test it without new
  evidence such as a much wider, string-heavy frame.
- `year` type must not be unified across stages without parity runs on every
  engine (CR-CQ-07).
- Do not benchmark on a different DuckDB version than the one pinned in
  `DESCRIPTION`; bundled extensions are built for that exact version.
- Maps are out of scope for chart or library migrations (guidelines section 7);
  optimize their data preparation instead.
- A cache that is warm in the candidate run and cold in the baseline run is
  not a fair comparison. Use `WISEAPP_STEP2_CACHE_STATES` and
  `WISEAPP_STEP2_FORCE_CACHE` deliberately, and note the weather disk cache
  (`WISEAPP_WEATHER_CACHE_*`) state in the report.
