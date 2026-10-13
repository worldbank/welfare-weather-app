---
name: add-model-engine
description: >
  Guide for adding a new model engine (for example a quantile forest or a
  Bayesian model) to WISE-APP. Covers the ENGINE_REGISTRY contract, model-type
  selection, Step 1 results and diagnostics, and the Step 2 and Step 3 engine
  gates, with the determinism, packaging and protected-area constraints.
when_to_use: >
  When the user wants to add a new model type, register a new engine, or extend
  the modelling pipeline with a new estimation method, or asks which parts of
  the app an engine change touches.
user-invocable: true
allowed-tools: Read Grep Glob
argument-hint: "[engine-name]"
---

# Adding a model engine to WISE-APP

Verified against the code on 2026-10-13. Line numbers drift, so this guide
points at function names and grep landmarks. Re-verify anything you rely on
(`grep -rnE 'ENGINE_REGISTRY|identical\(.*engine|engine %in%' R`) before
editing.

## Read this first: engine tiers

Engines do not all reach the same depth today.

| Engine | Registered | Offered in the UI | Step 1 | Step 2 | Step 3 |
|---|---|---|---|---|---|
| `fixest` (linear, logistic) | yes | yes (logistic only for logical outcomes) | yes | yes | yes |
| `rif` (unconditional quantile) | yes | yes (numeric outcomes) | yes | yes (RIF path) | yes |
| `ranger`, `xgboost` | yes | **no** (`model_type_choices()` does not list them) | partial (fit, `ml` headline path) | **blocked** | **blocked** |

So "registered" is not "supported". Registering an engine only gets it as far
as `fit_model()`. Step 2 and Step 3 are gated to `fixest` and `rif`:

- `mod_2_01_weathersim.R` (`output$run_sim_ui`) shows "Simulations are not yet
  implemented for ... models" for any other engine.
- `fct_policy_sim.R` (annual policy correction) errors with "requires RIF or
  linear fixest"; `fct_policy_decompose.R` and
  `.policy_annual_channel_status()` in `fct_policy_metric_decompose.R` return
  NULL or `"unsupported"`. `tests/testthat/test-fct-policy-metric-decompose.R`
  asserts this for `ranger` and `xgboost`.
- Coefficient-uncertainty draws assume a `fixest` fit: `compute_chol_vcov()` in
  `fct_simulations.R` has `stopifnot(inherits(fit, "fixest"))`.

**Decide the target tier with the user before writing code:**

1. **Tier A, Step 1 only** (registry + selection + results). Small, additive.
   This is how `ranger` and `xgboost` sit today.
2. **Tier B, full pipeline** (Step 2 simulation and Step 3 policy). This edits
   protected numerics (see below), needs uncertainty and metric-decomposition
   design, and needs parity and determinism evidence. Treat it as a project,
   not a checklist item. Read `review/optimization_guidelines.md` and the
   relevant plans in `review/` first.

`optimization_guidelines.md` section 3 also says: do not migrate existing
`fixest` workflows to other engines without an explicit decision; `lightgbm`
and `xgboost` are future-engine candidates only.

## Constraints that apply to every engine

- **Protected areas** (`review/code_hygiene.md`): `fct_run_simulation.R`,
  `fct_step2_compute.R`, `fct_step2_async.R`, `fct_policy_decompose.R`,
  `fct_policy_metric_decompose.R`, `fct_decomposition_summary.R`, the
  `ENGINE_REGISTRY` and metric-registry fields, `.wiseapp_*` columns, and the
  `year` type contract (`AGENTS.md`, CR-CQ-07). Edits there are deliberate
  feature work, not drive-by changes; keep them minimal and covered by tests.
- **Determinism.** The registry passes `seed` to `make_spec()`. Engines must be
  reproducible and single-threaded (ranger sets `num.threads = 1L`, xgboost
  `nthread = 1L`) and must not change the caller's `.Random.seed`. Model after
  `tests/testthat/test-determinism.R` ("ranger engine is deterministic and
  single-threaded"). Workers run on a shared mirai pool, so a multi-threaded
  engine also fights the main process for cores.
- **Packaging.** Engine packages beyond `fixest` and `broom` live in
  `DESCRIPTION` `Suggests:`, not `Imports:`. `test-deploy-contract.R` asserts
  `parsnip`, `ranger` and `xgboost` are not imported. Changing `Suggests:` or
  `Imports:` needs the user's approval; list the proposed diff first. Check
  `requires` with `requireNamespace()` (done in `fit_model()`), never
  `library()`.
- **Memory.** Fits are stored in a reactive and snapshotted to async workers.
  Keep fit objects slim (see `.slim_fit()` in `fit_model()` and
  `step2_slim_model_fit()`), and do not embed the training data in the object.
- **Errors.** User-facing text goes through `wise_user_error()`, not
  `conditionMessage()` in modules.
- **Style.** Native pipe, `<-`, 2-space indent, 120-column lines, ASCII quotes
  and hyphens in code and comments, no `library()` in `R/`.

## Checklist, Tier A (Step 1)

Each step names the function to edit. See [reference.md](reference.md) for the
contracts and landmarks.

### 1. Register in `R/fct_fit_model.R`

Add an entry to `ENGINE_REGISTRY` with all six fields:

```r
myengine = list(
  requires = c("pkg"),
  model_types = c("My model type"),
  build_formulas = function(y_var, terms, fe_vars) { ... },  # formula1..3
  fit_one = function(formula, data, model_type, model_spec, opts) { ... },
  make_spec = function(model_type, use_logit, seed = WISEAPP_DEFAULT_SEED) { ... },
  prepare_outcome = function(df, y_var, use_logit) { ... }
)
```

`fit_model()` handles everything around it: engine lookup (lower-cased key),
`requires` check, `model_types` check, the logistic-to-linear fallback (recorded
in `fallbacks`), term building, complete-case filtering, `prepare_outcome()`
(after the filter, CR-BUG-03), `make_spec()`, three `fit_one()` calls wrapped in
`tryCatch`, slimming, and the return list. Do not special-case an engine
inside `fit_model()`; if you must, that is a signal to extend the registry
contract instead (and flag it).

Engines that need extra options get them through `engine_opts` (see the
`fixest` cluster and `rif` options). Engines that add outcome columns or
metadata pass them back through `prepare_outcome()` attributes (see `rif_taus`
and `rif_cols`).

### 2. Wire selection in `R/fct_model_select.R`

- `infer_engine()`: add the model-type string to the `switch()`.
- `model_type_choices()`: add the type to the right outcome branch **only if
  the engine should be user-selectable**. `ranger` and `xgboost` are
  deliberately absent from the UI.
- Update the roxygen on `infer_engine()` and `build_selected_model()` (both
  list the mapping), then run `devtools::document()` with roxygen2 7.3.3.

### 3. Prediction in `R/fct_predict_outcomes.R`

`predict_outcome()` detects `fixest` by class or `engine`, then `parsnip`
`model_fit`, then falls back to bare `lm`/`glm` via `broom::augment()`. A
parsnip-backed engine works without changes. Anything else needs a new branch
for both fitted values and residual extraction. The output contract is a data
frame with `.fitted`, a residual draw (or NA) and the outcome column.

### 4. Step 1 results and diagnostics

Search for engine branches rather than trusting a list:
`grep -rnE 'identical\(.*engine, "rif"\)|engine == "rif"|\.s1_engine' R`.
The places that branch today:

- `fct_results.R`: `extract_native_fit()`, `extract_rif_median()`,
  `make_regtable_df()`, `calc_fit_stats()`, `echart_make_coefplot()`,
  `echart_weather_effect_plot()`; `fct_results_table2.R` (focused and specs
  tables).
- `fct_step1_headline.R`: `.s1_engine()` maps `ranger` and `xgboost` to `"ml"`;
  headline cards branch on `rif`, `fixest` and `ml`. A new engine needs its own
  class there or an explicit decision to reuse `ml`.
- `mod_1_07_results.R` and `mod_1_08_modelfit.R`: RIF-specific labels, popovers
  and diagnostics unwrapping.
- `fct_model_card.R` (`model_badge()`): add an engine label if the key is not
  self-explanatory.

Engines that return a model with usable `residuals()` and `fitted()` get the
generic diagnostics (residuals, predicted vs actual, importance) without
changes. Check each against the new engine rather than assuming.

### 5. Provenance and export

The engine key is written to run provenance and exports
(`fct_provenance.R`, `fct_export.R`). Confirm a new key renders there and in
`test-provenance.R` / `test-export-bundle.R` style assertions.

### 6. Tests

New tests go in `tests/testthat/test-<R file>.R` (fixtures move to a
`helper-*.R` only when two or more files use them). Cover at least:

- registry entry shape and unknown-engine / unsupported-model-type errors
  (`test-fct_fit_model.R`, `test-fct_fit_model_fallback.R`)
- `infer_engine()` / `model_type_choices()` mapping
- determinism and single-thread behaviour (`test-determinism.R` pattern)
- prediction (`test-fct_predict_outcomes.R`)
- results functions and headline cards for the new branch
- `step2_slim_model_fit()` keeps what Step 2 needs (`test-step2-slim-model-fit.R`)
- that Step 2 and Step 3 still reject the engine cleanly if it is Tier A
  (`test-fct-policy-metric-decompose.R` asserts this for `ranger` and `xgboost`)
- regression: `fixest` and `rif` paths unchanged

Engine packages are Suggests-only, so tests that need them must
`skip_if_not_installed()`.

## Checklist, Tier B (Step 2 and Step 3)

Only after the user confirms the tier. This list is a starting map, not a
complete recipe; expect to discover more by running the pipeline.

1. **Step 2 gate.** `mod_2_01_weathersim.R`: `run_sim_ui` allow-list and the
   `fixest`/`rif` assumptions in the run observer (search `is_rif`).
2. **Simulation.** `run_sim_pipeline()` (`fct_simulations.R`) and the
   `fct_run_simulation.R` reference path. The default branch assumes a
   `fixest` fit for stored fitted values and coefficient draws.
   `compute_chol_vcov()` is `fixest`-only. Decide how this engine expresses
   coefficient or model uncertainty (or declare it unsupported) and which
   `uncertainty` sources apply.
3. **Async boundary.** Step 2 runs on a shared mirai daemon from a snapshot.
   Check `step2_slim_model_fit()` (kept fields), serialization size against
   `WISEAPP_ASYNC_QUEUE_MEMORY_MB`, and that the engine package is installed
   where workers run. Do not create a second pool.
4. **Metric registry.** `fct_metric_registry.R` carries a per-metric `engines`
   field (default `c("ols", "rif")`) and `uncertainty` sources. Extend only the
   metrics the engine genuinely supports.
5. **Step 3.** The gates in `fct_policy_sim.R`, `fct_policy_decompose.R` and
   `.policy_annual_channel_status()`. The decomposition (main effect plus
   resilience: repositioning and interaction) is defined for fixest and RIF;
   a new engine needs its own definition, not a bypass. Update
   `test-fct-policy-metric-decompose.R` expectations deliberately.
6. **Evidence.** Run the determinism tests, then benchmark with the
   `wise-benchmark` skill on the BFA and IRN payloads. Report parity against
   the existing engines.

## Final verification

- `devtools::document()` (roxygen2 7.3.3 per `DESCRIPTION`) if roxygen changed;
  confirm `NAMESPACE` and `man/` diffs are intended.
- Targeted tests: `devtools::test(filter = "fit_model")`, then the files above;
  full `devtools::test()` (4 to 6 minutes) before reporting done.
- `Rscript tests/spelling.R` if you touched roxygen or comments.
- If `DESCRIPTION` changed, `dev/00_make_manifest.R` regenerates
  `manifest.json`; confirm with the user before regenerating.
- Report what was verified, what was skipped (for example skipped tests
  because the engine package is not installed), and anything flagged as
  adjacent.
