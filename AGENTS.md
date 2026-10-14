# AGENTS.md

This file provides guidance to coding agents (Kilo, Claude Code, Codex, etc.) when working with code in this repository.

**Performance/optimization work**: read and follow `review/optimization_guidelines.md` — it defines the app's optimization standards, app-specific constraints, and benchmarking requirements.

**Code hygiene work**: follow `review/code_hygiene.md`. **Open review items** (security, performance, accessibility, code quality) are tracked in `review/REVIEW-2026-10-06-tracking.md`; finished work is in `review/archive/`.

## What This Is

**WISE-APP** (Weather Impact Simulation and Evaluation for Adaptation Policy and Planning) is an R Shiny web application built by the World Bank. It estimates relationships between weather and household welfare, simulates welfare outcomes under climate scenarios, and evaluates policy/adaptation strategies.

- R package name: `wiseapp`
- Framework: [Golem](https://thinkr-open.github.io/golem/) (production-ready Shiny scaffolding)
- Version: 0.3.0 (`DESCRIPTION` and `inst/golem-config.yml` must match; `manifest.json` checksums follow)
- R version: 4.5.3 (configured by the local development environment); `DESCRIPTION` requires R >= 4.4.0
- License: MIT

## Development Commands

```r
# Install dependencies from DESCRIPTION
install.packages(read.dcf("DESCRIPTION")[1, "Imports"] |>
  gsub("\\s*\\([^)]*\\)", "", x = _) |>
  strsplit(",\\s*") |>
  unlist() |>
  trimws())
# duckdb must be exactly the version pinned in DESCRIPTION (bundled extensions)

# Run the app locally
wiseapp::run_app()
# or from the R console:
source("R/run_app.R"); run_app()

# Document (regenerate man/ and NAMESPACE)
# Use the roxygen2 version in DESCRIPTION's RoxygenNote (7.3.3); CI pins it,
# and 8.x rewrites the NAMESPACE layout.
devtools::document()

# Load all R code during development
devtools::load_all()

# Check package
devtools::check()

# Run tests
testthat::test_dir("tests")
# or
devtools::test()
```

Development scripts are in `dev/`: `run_dev.R` (run the app), `00_make_manifest.R` (Connect manifest) and the `bench_*.R` benchmark harnesses.

## Architecture

### Pipeline (tabs in the UI: Overview plus three steps)

1. **Step 0 – Overview** (`mod_0_overview`): Data source configuration (local/S3/GCS/Azure/HuggingFace/Databricks), loads survey metadata.
2. **Step 1 – Modelling** (`mod_1_modelling` + 8 sub-modules): Select sample → explore data → define outcome variable → pick weather variables → configure model → view results.
3. **Step 2 – Simulation** (`mod_2_simulation` + 3 sub-modules): Define historical/future weather scenarios (including CMIP6 climate models), generate welfare predictions.
4. **Step 3 – Policy Scenarios** (`mod_3_scenario` + 10 sub-modules): Model social protection, infrastructure, digital, labor market, and education interventions (`mod_3_01`-`mod_3_05`), then run the policy simulation, results, diagnostics, and decomposition (`mod_3_06`-`mod_3_09`), and the cash-transfer cost-effectiveness tab (`mod_3_10`).

Reactive data flows forward through the pipeline: Step 0 outputs feed Step 1, which feeds Steps 2 and 3.

### Code Organization

```
R/
├── app_ui.R / app_server.R    # Top-level app wiring
├── app_config.R               # Environment detection (dev/Posit Connect/Databricks)
├── run_app.R                  # Entry point
├── mod_*.R                    # 25 Shiny modules (each has a UI and server function)
├── fct_*.R                    # 43 business logic files (no Shiny dependencies)
└── utils_*.R                  # 5 files: shared math, UI, plot-theme, logging, and Step 1 helpers
src/welfare_stats.cpp          # Rcpp kernel: all aggregation statistics in one sort per group
```

**`fct_` files are the core engine:**
- `fct_connection.R` – builds connection params for each storage backend
- `fct_load_data.R` – data ingestion and validation via DuckDB
- `fct_fit_model.R` – **engine registry**: pluggable modeling backends (fixest, ranger, xgboost, RIF)
- `fct_predict_outcomes.R` – prediction pipeline for fitted models
- `fct_simulations.R` – orchestrates the simulation pipeline (historical + future weather)
- `fct_sim_compare.R` – visualization and comparison functions (exceedance curves, threshold tables)
- `fct_results.R` – output formatting, coefficient plots, tables
- `fct_policy_sim.R` – policy scenario variable discovery and placeholder UI
- `fct_sp_effectiveness.R` – social protection cost and targeting metrics (coverage, leakage, adequacy, realised errors) shown in the Step 3 diagnostics (for a shock program: who can be paid and the cost of a year with payments), plus the effect-per-cost engine of the cost-effectiveness tab (`sp_cost_effectiveness()`: cost per person lifted for a poverty rate, gain per $1 otherwise, average and 1-in-20 year)
- `fct_sp_shock.R` – **shock-responsive social protection**: spec validation (`.sp_shock_problem()`), trigger thresholds from the historical exposure (`sp_trigger_thresholds()`: weather level or 1-in-N return period, N-year record rule), trigger state and payout scope (`sp_trigger_state()`), the run-level `sp_shock_plan()` (plain data), the dynamic per-row transfer, run outputs (`sp_shock_pipeline_rows()`, `sp_shock_summary()`: activation, annual cost distribution, basis risk, leakage, budget and cap, anticipatory breakeven) and the Step 3 summary-card preview (`sp_shock_preview()`); it also holds the response check (`sp_response_check()`), impact triggers (`.sp_impact_column()`), layered dynamic targeting (`.sp_dyn_scope()`) and the timing design (`.sp_forecast_fire()`, `.sp_timing_effect()`)
- `fct_policy_decompose.R` – **policy effect decomposition** (main effect + resilience: repositioning + interaction)
- `fct_policy_metric_decompose.R` / `fct_decomposition_summary.R` – metric-aware Step 3 decomposition (per-metric channels) and its summaries
- `fct_metric_registry.R` – metric metadata (labels, units, direction, change kind, supported engines and uncertainty sources)
- `fct_rif_sim.R` – Recentered Influence Function (RIF) quantile regression helpers
- `fct_weatherstats.R` – weather statistics computation
- `fct_hexmap.R` – **hex-map engine bridge**: vendored MapLibre GL + h3-js (`inst/app/vendor/` — outside the `bundle_resources()` scan tree, served once via the explicit dependency; browser side `hexmap.js`), columnar payload contract (`hexmap_payload()`, senders `hexmap_update`/`hexmap_clear`/`hexmap_fit`), container `hexmap_ui()`. Used by mod_1_02, mod_1_03, mod_1_05; no Leaflet fallback — MapLibre is the only map surface. Asset pins and behavior details live in-file.
- `fct_step2_compute.R` – pure, serial Step 2 compute boundary (reference numerics remain in `fct_run_simulation()`)
- `fct_export.R` / `fct_provenance.R` – export bundles (config, tables, figures, metadata) and the immutable run-provenance record
- `fct_step2_async.R` – process-wide mirai async coordinator for Step 2 (FIFO queue, worker snapshots, secrets scrubbing) and the shared daemon pool for Overview metadata loads (credentials policy: see `review/optimization_guidelines.md` §0)

### Modeling Engine Registry

`fct_fit_model.R` defines `ENGINE_REGISTRY` with six fields per engine:
- `$requires` – package dependencies
- `$model_types` – supported model types (Linear regression, Logistic regression, Quantile regression)
- `$build_formulas` – creates nested formulae for progressive models
- `$fit_one` – fits a single model with the backend's API
- `$make_spec` – creates parsnip model spec (NULL for fixest/RIF)
- `$prepare_outcome` – coerces outcome to required type

**Engines:**
| Engine | Description | Model Types |
|--------|-------------|-------------|
| `fixest` | High-dimensional fixed effects (feols/feglm) | Linear, Logistic |
| `ranger` | Random forest via parsnip + ranger | Linear |
| `xgboost` | XGBoost via parsnip | Linear, Logistic |
| `rif` | Unconditional quantile regression via RIF (Firpo et al. 2009) | Quantile |

### Data Backends

DuckDB is the unified query engine with extension-based storage support:

| Backend | Required DuckDB Extension | Auth Method |
|---------|---------------------------|-------------|
| S3 | httpfs | AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY |
| GCS | httpfs | GCS_ACCESS_KEY_ID / GCS_SECRET_ACCESS_KEY |
| Azure | azure + delta (not bundled; installed on first use, local/dev runs only) | Account key or service principal |
| Databricks | httpfs | OAuth2 M2M (DATABRICKS_HOST/CLIENT_ID/SECRET) |
| Local | none | File paths |

See `fct_connection.R` and `fct_load_data.R` for implementation details.

## Key Patterns

### Shiny Architecture
- **Shiny modules**: Every major UI section is a Golem-style module (`mod_NAME_ui()` / `mod_NAME_server()`). Sub-modules are nested inside parent modules.
- **Reactive chain**: Outputs of one module (model object, selection state) are passed as reactive inputs to downstream modules.

### Modeling Pipeline
- **Progressive models**: Three nested specifications (weather only → + FE → + FE + controls) fitted identically for comparison.
- **Log-transform handling**: When welfare outcome is log-transformed, predictions are automatically back-transformed (`exp()`) throughout the pipeline.
- **Coefficient uncertainty propagation**: Cholesky factor of VCV matrix (`compute_chol_vcov`) enables fast Monte Carlo draws via factor loading matrix (`compute_factor_loading`), ~200x speedup over full-dimension draws.

### UI Conventions
- **Tables** are `reactable` with client-side CSV download (raw values exported; rounding lives in the column format). **Charts** are `echarts4r` built by `echart_*` functions, with the ggplot builders kept as static, export and test references; registered on-screen figures export through the echarts closure. Shared styling is in `utils_plot_theme.R`. Maps use only the MapLibre bridge (`fct_hexmap.R`). See `review/optimization_guidelines.md` sections 6 and 7.
- User-facing error text goes through `wise_user_error()` (full condition logged with an id, short classified message shown); do not show `conditionMessage()` directly in modules.
- Stage timing is logged one line per stage by `.wise_log_stage()` (`utils_log.R`).

### Policy Analysis
- **Policy decomposition**: Total effect = Main effect + Resilience effect (Repositioning + Interaction) — see `fct_policy_decompose.R`.
- **RIF (Recentered Influence Function)**: Unconditional quantile regression for distributional impact estimation (Firpo, Fortin & Lemieux 2009).
- **SP cash transfer column**: `.wiseapp_sp_transfer` is the single source of truth for social protection transfers of a regular program.
- **Shock-responsive SP** (`sp_type == "shock"`): ex post only; the trigger is evaluated per survey round, location and interview month on the historical thresholds (fixed under climate change); `apply_policy_to_svy()` writes the static eligibility `.wiseapp_sp_eligible` and no transfer. The transfer is paid per prediction row inside the annual correction: `apply_policy_delta_to_baseline(sp =, analysis_unit =)` builds the plan once, stores it on `prepared$shock`, and the one shared hook (`.policy_annual_channel_block(sp_dynamic =)`, fed by `.policy_sp_dynamic()` in all three consumers) adds it into `delta_sp`, `delta_main` and `delta_total` and returns it as `delta_sp_shock`. The amount is a fixed amount per activation or, with `budget_rule == "loss_share"`, a share of the modelled loss (plan rule 7) of the paid households of each triggered cell; an optional `annual_cap` on total cost scales a year's payments down pro rata (`sp_shock_pipeline_transfer()` is the one place that builds the per-row transfer, used by the run and the preview). RIF repositioning does not respond to the dynamic transfer (approximation). Run outputs travel as `pol_out$shock` (`rows`, `summary`, `paid_historical`) and are published as `shock_summary`; the published `policy_svy` marks households paid in at least one historical year in `.wiseapp_sp_transfer` so reach and diagnostics keep one definition of "treated". Phase 2 additions, all optional spec fields that leave the Phase 1 path unchanged at their defaults (the plan part is `NULL`): `budget_rule = "loss_share"` and `annual_cap` (budgets linked to modelled loss, pro-rata cap); `trigger_metric` other than `"weather"` (modelled loss, poverty rate or number of poor, read nationally, by GAUL admin level or by survey location; the metric becomes an exposure column so thresholds and scopes are the weather trigger's own code; poverty uses the trigger's own line (`trigger_pov_line` in the spec, prefilled from the outcome's poverty line, fixed per run)); `dyn_geo` / `dyn_welfare` (worst-affected share of triggered locations, predicted-welfare oracle with seeded errors); `transfer_timing = "anticipatory"` (forecast POD / FAR draws keyed by the unit the trigger is read at (survey cell, area, or the country for a national gate or national impact) and simulation year, shared by all members; multiplier `m` scales the welfare effect of hits only, never cost; `state$effect` is applied in `.policy_sp_shock_eval()` and cost keeps the nominal transfer; delay inputs scale ex-post welfare). Pass `seed` and `hist_exposure_map` (and `level_factor`) to `sp_shock_plan()`; `pov_line` is only a fallback for specs without `trigger_pov_line`. Plan: `review/sp_shock_responsive_plan.md`; tasks: `review/sp_shock_responsive_tasks.md`.

### Simulation Pipeline
- **`year` type contract**: `year` is a fixed-effect factor in the fitted models. Survey-weather frames carry it as a factor (`merge_survey_weather`), Step 2 projections and policy joins as character (`.step2_survey_projection`, `fct_simulations.R`), and period/sample helpers as numeric. fixest predicts identically from factor, character or integer `year`, but base-R and `model.matrix` engines (ranger, xgboost, lm-style fits) reject or mis-encode a type that differs from training, so do not unify the type without parity runs on every engine (CR-CQ-07).
- **Historical simulation**: Weather data joined to survey panel (one first-of-month date per (survey month × year) combination).
- **Future simulation**: SSP scenarios (SSP2-4.5, SSP3-7.0, SSP5-8.5) with additive/multiplicative perturbations per variable units.
- **Residual handling**: Options for display-time residual simulation (`original`, `normal`, `resample`, `none`).

### Config
- **Config via `config` package**: `inst/golem-config.yml` holds environment-specific settings; `app_config.R` reads them plus environment variables for deployment detection.

## Deployment

Target platform: **Posit Connect**. Automatic startup data-source selection is controlled by `WISEAPP_DATA_SOURCE`; see the environment variables below and the deployment scripts for deployment steps.

Runtime egress: the app serves its fonts locally (`inst/app/fonts/`), but the hex map loads its basemap style and tiles from CARTO (`tiles.basemaps.cartocdn.com`, see `inst/app/vendor/hexmap.js`). The server must allow HTTPS to that host for map views; everything else needs only the configured data source.

Bundled DuckDB extensions (`inst/duckdb_extensions/`: `h3` and `httpfs`) are built for the exact `duckdb` version pinned in `DESCRIPTION` (currently 1.5.6) and are checked against pinned SHA-256 values in `R/fct_load_data.R` before they are installed on Connect. To upgrade DuckDB, rebuild the binaries and update the pin, the version constant and the checksums together.

Optional resource limits (unset = package/DuckDB defaults). Every Connect process runs a main R process plus a mirai daemon, each with its own in-memory DuckDB, so cap them on shared hosts:
- `WISEAPP_DUCKDB_MEMORY_LIMIT` (for example `4GB`) and `WISEAPP_DUCKDB_THREADS` (per DuckDB instance); `WISEAPP_DUCKDB_TEMP_DIR` (spill directory, default a per-process temp dir)
- `WISEAPP_THREADS` (fixest and collapse threads)
- `WISEAPP_ASYNC_TIMEOUT_MIN` (Step 2 run limit, default 90), `WISEAPP_ASYNC_STEP3_TIMEOUT_MIN` (Step 3 run and metric jobs, default 90) and `WISEAPP_ASYNC_METADATA_TIMEOUT_SEC` (default 300); `0` disables
- `WISEAPP_ASYNC_STEP3=0` runs the Step 3 policy run and metric decomposition in the main process. By default they run on the shared mirai daemon and read the adopted Step 2 result and the Step 3 policy result from retained qs2 files under `WISEAPP_ASYNC_ARTIFACT_ROOT` (default a per-process temp dir, `retained/`). Budget temp disk per session: about 0.4-0.6 GB at a 3x3 Step 2 payload (BFA 2x2: 178 MB plus 216 MB). The files are removed when the result is replaced and at session end.
- `WISEAPP_STAGE_LOG=0` turns off the one-line-per-run stage log
- Weather disk cache: `WISEAPP_WEATHER_CACHE_DIR` (location, created 0700), `WISEAPP_WEATHER_CACHE_MAX_MB` (LRU budget), `WISEAPP_WEATHER_CACHE_DISABLE=1` (off)
- `WISEAPP_ASYNC_QUEUE_MEMORY_MB` (default 512; `0` disables): the mirai dispatcher's memory cap and the size limit for one job snapshot. A Step 2 or Step 3 snapshot over the cap is refused with a message instead of retried, and queued Step 2 snapshots may total at most twice the cap.
- `WISEAPP_ASYNC_SYNC=1` runs Step 2 and metadata tasks in the main process (debugging only; this removes the async protection)

Data-source allowlists (CR-SEC-02): `WISEAPP_ALLOWED_SOURCES` (comma-separated source types) restricts which sources the app accepts. Unset, a configured `WISEAPP_DATA_SOURCE` is the only enabled type, and on Posit Connect without one only `databricks` is; local and dev runs allow every type. For connections typed into the UI, `WISEAPP_ALLOWED_LOCAL_ROOTS`, `WISEAPP_ALLOWED_BUCKETS` (S3/GCS buckets, Azure containers, Hugging Face repos) and `WISEAPP_ALLOWED_VOLUME_ROOTS` (Databricks volume paths) restrict the location; an unset list does not restrict that field. Values containing control characters or `..` segments are always refused for UI connections. Environment-configured connections are trusted.

Key environment variables for production:
- `WISEAPP_DATA_SOURCE` (automatic source selector: `local`, `s3`, `gcs`, `azure`, `hf`, or `databricks`)
- `WISEAPP_DATA_PATH` (local data backend)
- `DATABRICKS_HOST`, `DATABRICKS_CLIENT_ID`, `DATABRICKS_CLIENT_SECRET`, `DATABRICKS_VOLUME_PATH` (for Databricks backend)
- `S3_BUCKET`, `S3_PREFIX`, `S3_REGION`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` (for S3)
- `GCS_BUCKET`, `GCS_PREFIX`, `GCS_ACCESS_KEY_ID`, `GCS_SECRET_ACCESS_KEY` (for GCS)
- `AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_CONTAINER`, `AZURE_STORAGE_PREFIX`, `AZURE_STORAGE_KEY` or `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`, `AZURE_TENANT_ID` (for Azure)
- `HF_REPO`, `HF_SUBDIR` (for public Hugging Face datasets)

## Git Remotes

- `origin`: official World Bank repo dev branch (`worldbank/wise-app:dev`)

## Testing

Tests are in `tests/testthat/` (122 files, named after the `fct_`/`mod_` file or concept they cover, e.g. `test-fct_hexmap.R`, `test-active-mask.R`) plus `tests/spelling.R`. New tests go in `test-<R file>.R`. Areas with dedicated coverage: connection/data loading, model fitting + coefficient uncertainty decomposition, prediction, aggregation delta, RIF helpers, hexmap payload contract, policy decomposition uncertainty, metric-aware decomposition, Step 3 lever modules, `app_server` wiring (stubbed step modules), weather selection/stats, export bundles, determinism.

- **Run from the source tree**, as CI does: `devtools::test()` or `testthat::test_local()` (about 4 to 6 minutes serial). `R CMD check` runs with `--no-tests`.
- **Environment is pinned** by `tests/testthat/setup-env.R` (weather caches in a per-run temp dir; `WISEAPP_DATA_*` and cloud credentials unset) and `setup-locale.R` (UTF-8). Do not rely on the developer's `.Renviron`.
- **Fixtures:** a builder moves into a `helper-*.R` file only when two or more test files use it.
- **Coverage:** use `covr::package_coverage(type = "none", code = ...)` with `testthat::test_dir("tests/testthat", load_package = "installed", package = "wiseapp", ...)`, excluding the files that start real mirai workers or headless Chrome (`step2-async*`, `fct-overview-metadata`, `export-bundle`, `mod-0-overview`); their trace files corrupt covr. Code run inside workers is not counted.
- **Browser smoke:** `tests/e2e/smoke.R` boots the app with `shinytest2`, checks one `conditionalPanel` and runs axe-core on the Overview and Step 2 screens (`Rscript tests/e2e/smoke.R` from the repo root; needs Chrome). It is not part of `devtools::test()` or R CMD check; the non-blocking CI job is `.github/workflows/e2e-smoke.yaml`. `WISEAPP_AXE_FAIL=1` fails it on serious or critical violations.
- **Not covered by R tests:** MapLibre rendering, `conditionalPanel` visibility, `update*Input()` on config import, and the real Step 0 to 3 flow (they need a browser).
