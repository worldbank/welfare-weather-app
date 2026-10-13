# Reference: engine contracts and integration points

Verified against the code on 2026-10-13. Locate things by function name or the
grep landmark, not by line number.

## `ENGINE_REGISTRY` (R/fct_fit_model.R)

Engines today: `fixest`, `ranger`, `xgboost`, `rif`.

| Field | Signature / type | Notes |
|---|---|---|
| `requires` | character vector | Checked with `requireNamespace()` in `fit_model()` before anything else. |
| `model_types` | character vector | Must match `selected_model$type` strings exactly. |
| `build_formulas` | `function(y_var, terms, fe_vars)` | Returns `list(formula1, formula2, formula3)`. `terms` has `$hazard`, `$interactions_main`, `$covariates`. `fixest` puts FE after `\|`; `ranger`/`xgboost` add FE as ordinary features; `rif` returns RHS strings. |
| `fit_one` | `function(formula, data, model_type, model_spec, opts)` | Called three times. `opts$fixest` (cluster) and `opts$rif` (`taus`, `rif_cols`) are the existing option slots. |
| `make_spec` | `function(model_type, use_logit, seed = WISEAPP_DEFAULT_SEED)` | parsnip spec, or `NULL` for non-parsnip engines. Must be seeded and single-threaded. |
| `prepare_outcome` | `function(df, y_var, use_logit)` | Runs after the complete-case filter. May add columns and set attributes (`rif_taus`, `rif_cols`). |

### `fit_model()` flow (what you get for free)

Landmarks: `engine_key <- tolower(selected_model$engine %||% "fixest")`,
`backend <- ENGINE_REGISTRY[[engine_key]]`.

1. Unknown engine key stops with the list of registered engines.
2. Missing `requires` packages stop with the package names.
3. `selected_model$type` must be in `backend$model_types`.
4. Logistic request on a non-logical or non-0/1 outcome falls back to linear
   with a warning and a `fallbacks` record (`kind = "model_family"`).
5. Weather, polynomial and interaction terms are built (pairwise or saturated).
6. Cluster variables missing from the data fall back to an unclustered fit with
   a `fallbacks` record (`kind = "vcv"`); present ones join the complete-case
   filter.
7. `df <- df[stats::complete.cases(...), ]`: `train_data` has fewer rows than
   the survey frame. Use `svy`, not `train_data`, when matching simulation data.
8. `backend$prepare_outcome()`, then `backend$make_spec(model_type, use_logit,
   seed = seed)`.
9. `engine_opts`: `fixest` clustering, plus `rif` when `rif_taus` is set.
10. Three `fit_one()` calls, each wrapped so a failure reports which spec failed.
11. Slimming (`.slim_fit()`, `fixest` and `fixest_multi` only: drops the embedded
    `data` from `$call` and `$scores`) and `.cache_mm()` (stores the fit3 design
    matrix in attribute `wise_mm` before slimming; `model.matrix()` does not
    survive slimming). Non-fixest fits are returned unslimmed.
12. `build_rif_grid()` for RIF only.

### Return structure

```r
list(
  fit1, fit2, fit3,            # weather / + FE / + FE + controls
  weather_terms, interaction_terms, fe_terms,
  y_var, model_type,           # "linear" or "logistic"
  engine,                      # registry key
  train_data,                  # after complete.cases()
  formulas,
  rif_grid, taus,              # NULL unless RIF
  fallbacks                    # list of specification fallbacks applied
)
```

`step2_slim_model_fit()` (fct_step2_compute.R) keeps only: `engine`,
`weather_terms`, `interaction_terms`, `fe_terms`, `fit3`, `formulas`,
`train_data`, `y_var`, `model_type`, `taus`, `rif_grid`, `fallbacks`, `.sig`,
and `.snap$model`. New fields that Step 2 needs must be added there (a
protected file: deliberate edit and test).

## Selection (R/fct_model_select.R)

- `infer_engine(model_type)`: `switch()` from model-type string to key. Falls
  back to `"fixest"` for unknown strings. Currently maps Linear and Logistic
  to `fixest`, `"Random forest"` to `ranger`, `"XGBoost"` to `xgboost`, and
  `"Unconditional quantile regression (RIF)"` to `rif`.
- `model_type_choices(outcome_type)`: what the Step 1 UI offers. Logical
  outcome: Logistic, Linear. Numeric: Linear, RIF. `Random forest` and
  `XGBoost` are not offered.
- `build_selected_model(...)`: `engine` defaults to `infer_engine(model_type)`.
- `mod_1_06_model.R` renders `model_type_choices()` as a pill toggle and builds
  the spec with `build_selected_model()`; check it if the new type needs extra
  inputs.

## Prediction (R/fct_predict_outcomes.R)

`predict_outcome(model, newdata, type, residuals, id, outcome, train_data,
engine, seed)`:

- Detection: `is_fixest` (class `fixest` or `engine == "fixest"`), then
  `is_parsnip` (class `model_fit`), else bare `lm`/`glm`.
- Fitted values: `stats::predict(..., type = "response")` for `fixest` (checks
  row alignment; NA predictions pass through), `broom::augment(new_data = )`
  for parsnip (resolves `.pred` / `.pred_1`), `broom::augment(newdata = )` for
  bare models.
- Residual options: `"none"`, `"original"`, `"normal"`, `"resample"`. Residuals
  other than `"none"` need `train_data`.
- Output: data frame with `.fitted`, a residual draw (or NA) and the outcome
  column.
- The roxygen for `engine` lists `lm_glm` and `fixest`; update it if you add a
  branch.

## Results and diagnostics

Engine branches (find with `grep -rnE 'identical\(.*engine, "rif"\)|engine == "rif"' R`):

| File | Functions |
|---|---|
| `fct_results.R` | `extract_native_fit()` (unwraps parsnip to the native fit), `extract_rif_median()`, `make_regtable_df()`, `calc_fit_stats()`, `echart_make_coefplot()`, `echart_weather_effect_plot()` |
| `fct_results_table2.R` | `make_regtable_specs_df()`, `make_regtable_focused_df()` and the two reactable builders |
| `fct_step1_headline.R` | `.s1_engine()` (maps `ranger`/`xgboost` to `"ml"`) and the headline cards |
| `mod_1_07_results.R` | RIF labels, popovers, conditional UI |
| `mod_1_08_modelfit.R` | diagnostics unwrapping and the RIF welfare-quantile plot |
| `fct_model_card.R` | `model_badge()` engine label |

Generic and usually engine-free: `echart_residual_panels()`,
`echart_pred_vs_actual()`, `echart_importance()`. Confirm each with the new
fit class.

Charts are echarts (`echart_*`); the ggplot builders remain as static, export
and test references. Tables are `reactable` with a client-side CSV download.
Follow `review/optimization_guidelines.md` sections 6 and 7 for any new
figure or table.

## Step 2 (simulation)

- `mod_2_01_weathersim.R`: `output$run_sim_ui` allow-list is
  `c("fixest", "rif")`; others get the "not yet implemented" notice. The run
  observer reads `mf$engine` and derives `is_rif`.
- `fct_simulations.R`: `run_sim_pipeline()` defines `is_rif` from the engine
  plus `fit_multi`, `taus`, `weather_cols`; `compute_chol_vcov()` requires a
  `fixest` fit (or `fixest_multi` for RIF).
- `fct_run_simulation.R`: reference numerics (protected). The default branch
  uses `model$fitted.values` for `fixest` and `predict()` otherwise.
- `fct_step2_compute.R`: pure serial compute boundary and `step2_slim_model_fit()`.
- `fct_step2_async.R`: process-wide mirai coordinator (protected). The worker
  loads the package; engine packages must be installed there.

Residual controls: if residuals do not apply to the engine, disable or hide
the residual choice in `mod_2_01_weathersim.R` and force `"none"` rather than
silently ignoring it.

## Step 3 (policy)

- `fct_policy_sim.R`: annual policy correction stops unless the engine is
  `rif` or `fixest`.
- `fct_policy_decompose.R`: engine gates and RIF-specific repositioning.
- `fct_policy_metric_decompose.R`: `.policy_annual_channel_status()` returns
  `unsupported` for other engines or non-identity/log outcome transforms.
- `fct_step3_async.R`: RIF-specific `F_hat_pre` handling.
- `mod_3_09_decomposition.R`: derives the engine for the decomposition pane.
- `fct_metric_registry.R`: each metric has `engines` (default `c("ols", "rif")`)
  and `uncertainty` (default coefficient, weather, ensemble). These are
  metadata strings used by the UI; they are not the registry keys.

Policy decomposition is Total = Main + Resilience (Repositioning +
Interaction). Do not reuse the RIF or fixest definition for another engine
without a derivation and tests.

## Provenance and export

`fct_provenance.R` records `engine` in the immutable run record and the
selected-model summary; `fct_export.R` writes it into the metadata table.
Check both with the new key.

## Tests to model on

| Concern | File |
|---|---|
| Fit, fallbacks | `test-fct_fit_model.R`, `test-fct_fit_model_fallback.R` |
| Determinism, single thread | `test-determinism.R` (ranger and xgboost cases) |
| Prediction | `test-fct_predict_outcomes.R` |
| Model card badge | `test-fct_model_card.R` |
| Slim snapshot | `test-step2-slim-model-fit.R` |
| Step 2 contracts | `test-step2-compute.R`, `test-step2-contract.R`, `test-step2-payload.R` |
| Step 3 unsupported engines | `test-fct-policy-metric-decompose.R`, `test-policy-central-kernel.R` |
| Packaging | `test-deploy-contract.R` (Suggests-only engine packages) |
| Results | `test-fct_results.R`, `test-fct-results-fitdiagnostics.R`, `test-fct_step1_headline.R`, `test-mod_1_07_results.R`, `test-mod_1_08_modelfit.R` |

## Passing engine data through the pipeline (Tier B)

If an engine needs more than `model` and `engine` downstream (RIF carries
`rif_grid`, `taus`, and a stacked `fit_multi`):

1. Return it from `fit_model()` and keep it in `step2_slim_model_fit()`.
2. Read it in the module server that starts the run, and add it to the
   snapshot that goes to the worker.
3. Thread it through `run_sim_pipeline()` and the policy pipeline call sites;
   `grep -n 'run_sim_pipeline(' R` to find every call, because a missed call
   site silently drops the engine data.
4. Add a determinism and parity test before benchmarking.
