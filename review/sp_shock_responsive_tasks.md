# Social protection: remaining tasks

Companion to `review/sp_shock_responsive_plan.md` (design rules, Phase 2 and 3 design). Updated 2026-10-14.

Phases 0, 1 and the Phase 2 build tasks P2-1 to P2-8 are built and committed on `dev`. What each task did, with its measurements, is in the git history (`git log --grep "P2-"`); this file keeps only what binds later work or is still open. Status key: `☐` todo, `◐` built but not fully verified, `☑` done.

## 0. Conventions

- Repository rules: `fct_` files stay free of Shiny; touch only files the task needs; run the narrowest meaningful check; run `devtools::document()` when roxygen or exports change; no commits or pushes unless requested; code, comments and commit messages use ASCII quotes and hyphens. After a commit that changes R files, regenerate `manifest.json` (`dev/00_make_manifest.R`), re-run `test-deploy-contract.R`, commit separately.
- Reproducibility (plan rule 10): same inputs and seed give identical outputs regardless of row order, chunk size and thread count; preview equals run; every new spec field enters the run signature and the export; every task that changes numbers or draws carries a determinism test.
- A change in the Step 3 hot path (the annual correction loop) needs the benchmark in `review/optimization_guidelines.md`. Harness: `WISEAPP_STEP2_COUNTRIES=BFA WISEAPP_STEP2_MODELS=ols WISEAPP_STEP2_SSPS=ssp2_4_5 WISEAPP_STEP2_WORKLOADS=one_ssp_one_period WISEAPP_STEP2_INCLUDE_STEP3=1 WISEAPP_STEP3_POLICIES=<fixtures> Rscript dev/bench_step2.R`. Fixtures: `targeted_sp`, `shock_sp`, `shock_sp_dyn` (P2-8), `shock_sp_impact` (P2-5), `shock_sp_antic` (P2-7).
- New code reads `sp$currency` and uses `outcome_level_scale()` / `.policy_sp_transfer()`, never the raw transfer column. Loss-based outputs use plan rule 7; poverty lines follow rule 15. Read optional spec fields with `[[ ]]` (`$dyn_geo` partial-matches `dyn_geo_pct`).

## 1. Decisions that still bind

| # | Decision |
|---|---|
| 2 | "People lifted" is the net change in the number of poor, labelled as such. |
| 3 | Cost-effectiveness leads with cost per person lifted for a poverty rate, welfare gain per $1 otherwise; near-zero or wrong-signed effects print "not applicable". |
| 5 | Cost-effectiveness covers cash transfers only (transfer, administration, modelled welfare and poverty effect); the tab says so. |
| 7 | Share-of-loss budgets and the annual cap are built (P2-6); AAL sizing, poverty-linked budgets and the envelope per activation are P3-5. |
| 9 | Cost-effectiveness uses one seeded targeting draw and the Results poverty line. |
| 10 (revised 14 Oct) | Run-defining features (impact triggers on poverty) use the trigger's own poverty line, a spec field prefilled from the outcome's line and fixed per run; display metrics follow the Results line. |
| 14 (14 Oct) | Plan rule 10 is restated: targeting-error and forecast draws are keyed by unit and simulation year and shared across members (common random numbers), not keyed by member. Revisit if Monte Carlo intervals or member-year tail statistics of these draws are reported, or if members run on different household sets. |
| 15 (14 Oct) | Forecast draws are keyed by the unit the trigger is read at (decision 14 otherwise unchanged): one draw per year for a national impact metric or a national weather gate (the gate is forecast), per area-year for admin levels, per cell-year otherwise. |
| 13 (14 Oct) | Forecast skill is POD and FAR; a false alarm pays cash at full cost with no multiplier `m`; ex-post delay is weeks x percent lost per week, welfare only; breakeven `m` is on the transferred-value basis. |

## 2. Phase 2

| Task | Status | What binds later |
|---|---|---|
| P2-0a/b/c spikes | ☑ | Binned companions cost about +40 % on the weather part of a Step 2 result (accepted). Impact metrics are read nationally, by GAUL admin 1/2 or by survey location; poverty below national level needs 30 sampled households per unit-year; location poverty is unusable on BFA (12 households per cluster). |
| P2-1 response check | ☑ | Checked in the browser. Direction thresholds (20 % ends, 1 point, one half) are judgement calls. |
| P2-2 cost-effectiveness tab, shock outputs | ◐ | Not checked in a browser: tab order (after Diagnostics), and the "Historical" scenario label match between the Results series and the shock rows (tested only with stubs). |
| P2-3 decomposition channel | ◐ | Not checked in a browser. Compact frames built before it read the new channels as 0. |
| P2-4 binned triggers | ◐ | Not checked in a browser. Companion `<var>_cont` never reaches a model matrix (bit-identical predictions gate). |
| P2-6 budgets | ◐ | Not checked in a browser. Not run through the harness: a loss-share or capped run; LCU cost weights in the cap (tested in PPP only). |
| P2-8 layered targeting | ◐ | Not checked in a browser. Welfare-error draws are keyed by (survey row, year) and shared by all members on purpose (decision 14). Predicted welfare is an oracle, labelled. |
| P2-5 impact triggers | ◐ | Not checked in a browser. No response check or record-length text for impact triggers. A threshold at the historical minimum never fires ("no adverse tail"). |
| P2-7 anticipatory design | ◐ | Not checked in a browser. Forecast draws keyed by (trigger unit, year), shared by all members (decisions 14, 15). Breakeven `m` is exact for mean welfare, approximate for poverty metrics. `m` is an assumption; the static model cannot show the mechanism. |

BFA hot-path figures (P2-9 run, correction loop `policy_seconds`, ols, one SSP and one period, median of 3, uncertainty disabled / enabled; before = `7545ab3`, end of Phase 1; machine noise is about +-0.5 s, and a unit-test run overlapped the before run): `shock_sp` 8.2 / 8.2 s before, 7.8 / 7.8 s after (no regression; Phase 2 features are inactive at their defaults); `targeted_sp` 3.8 / 3.7 s before, 3.9 / 3.9 s after; with a Phase 2 feature on: `shock_sp_dyn` 9.0 / 8.9 s, `shock_sp_impact` 8.9 / 8.8 s, `shock_sp_antic` 9.5 / 9.5 s (+1.1 to +1.7 s, +15 to +20 % over `shock_sp`). Peak parent RSS 2.4 to 2.9 GB, not higher than before. `shock_sp` output fingerprints are identical across P2-8, P2-5 and P2-7. IRN has not been run for Phase 2.

### P2-9: Tests, benchmark and docs for Phase 2 ☑ (14 October 2026)

Plan invariants extended and audited against the tests; two new tests (all Phase 2 features together; every feature at its neutral setting equals Phase 1); BFA before/after benchmark above; `AGENTS.md` and the plan updated. Remaining: IRN benchmark and the browser pass (section 3).

## 3. Owed by the user

Record in `review/REVIEW-2026-10-06-tracking.md`:

- Browser checks for every `◐` row above, in one Step 0 to 3 pass with a shock program.
- BFA before and after headline numbers for P0-8 (synthetic check: realised weighted inclusion 15.8 % / exclusion 12.2 % before, 15.0 % / 10.0 % after, for 15 % / 10 % sliders).
- BFA and IRN Step 3 benchmark figures with the shock fixture.
- The BFA LCU re-run (R2-BUG-04 / CR-BUG-02) entry for the Decision log.
- **P0-9** (admin presets and error defaults) stays deferred: literature pass on cost-transfer ratios and PMT error rates, then a preset dropdown. The 10 % / 10 % defaults and a zero admin share stay; the cost-effectiveness tab prints the admin share beside every figure.

## 4. Open points

- The impact-level select is filtered by the survey columns; a restored config that asks for a missing level falls back to National silently.
- Unit costs for non-cash levers (`mod_3_02` to `mod_3_05`) stay out of scope until the policy team supplies them.

## 5. Phase 3: design-space tools (not scheduled)

| Task | Work | Size |
|---|---|---|
| P3-1 presets | Named presets (protective, preventive, promotive, custom) as data; combined "cash plus" scenarios use the existing levers. | S |
| P3-2 design sweep | Grid of shock designs reusing baseline pipelines and prepared channels; cost vs poverty-reduction frontier; bounded grid, progress, resource limits. Shock arm only at first. | M |
| P3-3 reserve dynamics | Carry-over and depletion need an ordered-year rule; years are draws today, so decide the rule first. | M |
| P3-4 amount rules | Gap-filling and tiered amounts; loss-proportional only as an oracle or not at all. | S to M |
| P3-5 further budget rules | AAL sizing and premium-equivalent cost; budget proportional to the rise in the number of poor or the poverty gap (uses P2-5 and rule 15); envelope per activation. Reuses the P2-6 ceiling and cap code. | M |

Phase 3 follows Phase 2 outputs (P2-2, P2-6); P3-5 also needs P2-5.

## 6. Risks

| Risk | Mitigation |
|---|---|
| Hot-loop slowdown (Phase 2 adds 0.5 to 2 s on BFA over `shock_sp`) | Benchmark each change; cheaper options if needed: a one-time key index in place of the keyed join, one `rowsum` for annual cost, skip the ex-post comparator state unless the breakeven is shown. |
| Anticipatory `m` or POD/FAR read as calibrated numbers | Defaults 1, 100 %, 0 %; breakeven display; in-app statement of the static-model limitation. |
| Impact triggers unobservable in practice | Labelled as a benchmark in the UI; national and area levels by default. |
| Cost-effectiveness read as covering all levers | Decision 5: scope statement shown in the tab. |
| Two poverty lines give inconsistent numbers | Decision 10: the trigger's own line for run-defining features, Results line for display, each printed. |
| Stale cached arm after re-running Step 3 | Key shared caches on the policy run signature (regression test in `test-policy-sim-compare-agg-cache.R`). |

## 7. Phase 2 review (14 October 2026)

Code review of P2-1 to P2-9 (`c4bbf9e..1f1cc71`) with real-data probes. Nothing was changed in `R/`; the findings below are open. IDs are `PR-n`; severity is H (wrong or unusable result), M (misleading result or label), L (speed, polish).

### 7.1 Verification

- Tests: `test-sp-shock-correction.R` (59), `test-fct_sp_shock.R` (57), `test-fct_sp_effectiveness.R` (14), `test-mod_3_10_cost_effectiveness.R` (9), `test-weather-continuous-companion.R` (7), `test-policy-exposure-mapping.R` (9), `test-fct-policy-metric-decompose.R` (26): 181 pass, 0 fail.
- BFA real data (small payload, `WISEAPP_DATA_PATH`): Step 2 built with the harness's own input code (ols, SSP2-4.5 2025-2035, 16 members, 30 historical years, 7,128 households, 596 survey locations, 10,350 trigger cells). Step 3 run through `apply_policy_delta_to_baseline()` as the app calls it (plan built inside, with `level_factor` and `seed`). Program: ex-ante poorest 30 %, $30 per activation, `spei6` above 1-in-5, local scope. Each variant took 7.8 to 10.8 s.

| Variant (historical / SSP) | Years with payments | Mean annual cost | FP / FN rate | Note |
|---|---|---|---|---|
| Base | 0.933 / 0.960 | 5.70M / 6.99M | 0.065 / 0.068 (hist.) | |
| National loss impact trigger, 1-in-5 | 0.200 / 0.442 | 5.70M / 12.6M | 0.141 / 0.481 | fires 2.2x as often under SSP2-4.5 |
| Same + worst-affected 50 % | 0.200 / 0.442 | 2.59M / 5.72M | 0.141 / 0.481 | PR-2, PR-6 |
| Poverty-rate impact trigger | error | | | PR-1: "Step 2 has no poverty line" |
| Same with `hs$pov_line <- 3` set by hand | 0.200 / 0.429 | 5.70M / 12.2M | | works once the line exists |
| Loss share 50 %, cap 50M, admin 10 % | 0.933 / 0.960 | 36.1M / 39.9M | | 1-in-20 cost = cap exactly; cap binds 60 % / 70 % of years |
| Worst-affected 50 % + predicted welfare 30 % | 0.900 / 0.954 | 1.64M / 1.98M | | |
| Either end, 1-in-10 | 1.000 / 0.998 | 3.80M / 5.70M | 0.080 / 0.575 | |

- Confirmed correct: return-period thresholds fire 10.0 % of cell-years for 1-in-10 (`spei6` below 0.1001, `t` above 0.1008); POD 100 %, FAR 0 %, m = 1 reproduces ex post exactly (same cost, breakeven 1); the annual cap is never exceeded; response check on BFA reads "wet years, high spei6, 12.8 points deeper" and "cold years, low t, 2.4 points"; the Historical scenario in all summaries uses the published scenario label; Step 3 tab order is Results, Diagnostics, Cost-effectiveness, Decomposition (observer creation order).

### 7.2 Findings

| ID | Sev | Finding | Evidence | Suggested fix |
|---|---|---|---|---|
| PR-1 | H | Poverty impact triggers (`poverty_rate`, `poor_count`) can never run in the app: `hist_sim$pov_line` is never set. `hist_sim_result` (`fct_run_simulation.R:897`) has no `pov_line`, and `mod_2_01_weathersim.R` adds only `hist_label`, `sim_summary` and `.sig`. The preview (`fct_sp_shock.R:1654`) and the run (`fct_policy_sim.R:1723`) pass `NULL`, so every poverty trigger aborts with "Step 2 has no poverty line". Tests pass only because fixtures set `hs$pov_line` by hand. Knock-on: the cost-effectiveness "Shock-affected poor" table falls back to the same field (`mod_3_scenario.R:356`) and shows "Needs a positive poverty line" whenever the Results metric is not a poverty metric. The `pov_line_sim` input documented at `mod_2_01_weathersim.R:20` no longer exists. | BFA run: error for `poverty_rate`; works when `hs$pov_line <- 3` is set. | Store the Step 2 line on the result at run time (the resolved `.step2_resolve_display()$pov_line`, else `so$povline`), include it in the Step 2 signature, and add a test that reads it from a `fct_run_simulation()` result. Until then, grey out the two poverty choices with the reason. |
| PR-2 | H | "Worst-affected share" is arbitrary under an impact trigger read nationally (and within each area for admin levels). Severity is depth beyond the threshold over the cell's SD (`.sp_trigger_severity()`); a national metric is identical in every cell, so the ranking falls to the tie-break on the cell key string (`fct_sp_shock.R:945`). | BFA, national loss trigger + 50 %: the paid set is exactly the alphabetically first 320 of 596 locations (`loc_id` "1.0" to "387.0"). | For impact triggers rank cells by their own modelled loss (rule 7) or by the location-level metric; otherwise refuse `worst_share` with a national impact trigger in `.sp_dyn_problem()` and the UI. Add a test that the paid set does not depend on key order. |
| PR-3 | H | Forecast draws (P2-7) are per cell even when the event is national. (a) National impact trigger, POD 70 %: each event year pays about 70 % of locations instead of 70 % of event years paying everyone (exposed share 0.20 to 0.14, years with payments unchanged at 0.20). (b) National gate: independent per-cell false alarms add a near-constant exposed share, so FAR mostly lowers the gate. | BFA, national gate k = 20 %, POD 100 %: FAR 0/5/10/20 % gives years paid 0.467/0.500/0.533/0.833 and mean cost 4.92M/5.50M/6.16M/9.06M (+84 % at FAR 20 %). | Key the forecast by the trigger unit: one draw per year for a national metric, per area-year for admin levels, per location-year for location impact; for a weather trigger with a national gate, forecast the gate (one draw per year) rather than each cell. Update decision 13 and plan rule 10. |
| PR-4 | M | Anticipatory with `national_all`: households paid in non-triggered cells of a gate year count as "false alarms" and get no multiplier m, because `hit` is the cell's realised trigger (`fct_sp_shock.R:1129`). | BFA, perfect forecast (POD 100 %, FAR 0 %): "Payments before no event" 63 %. | Under national scopes define a hit on the gate (the national event happened), not on the cell. |
| PR-5 | M | The breakeven m compares transferred value, not value per cost (decision 13), but the label and help ("at which the forecast design matches the ex-post design") read as a value-for-money breakeven. | BFA local, POD 70 %, FAR 0 %: breakeven m = 1.42, while the anticipatory design costs 30 % less (4.02M vs 5.70M); per unit of cost the breakeven is 1. | Rename ("m needed to match the ex-post welfare effect") and add the per-cost breakeven beside it. |
| PR-6 | M | Basis risk scores the trigger before the national gate and the layers: `fired = state$exceeds` (`fct_sp_shock.R:1208`), yet the rows are labelled "False negatives (loss event, not paid)" and "False positives (paid, no loss event)". | BFA: national loss trigger with and without worst-affected 50 % give the same FP/FN (0.141/0.481) although about half the locations go unpaid; national gate k = 20 % gives the same FN as local (0.068). | Score a cell as paid when any eligible household in it is in scope, or relabel the rows "trigger fired" and add a paid-based pair. |
| PR-7 | M | Under anticipatory payments, the worst-affected layer ranks forecast-fired cells by realised depth, so false-alarm cells (negative depth) are always ranked last: the layer uses information an ex-ante program does not have. | Code reading (`.sp_dyn_scope()` uses `exceeds_t` after the forecast, severity from realised values). | Rank fired cells without realised information (for example equal rank with the seeded key) or label the combination as an oracle. |
| PR-8 | M | Diagnostics "Cost and targeting effectiveness" for a shock program substitutes `shock_summary()$per_household` (`mod_3_08_diagnostics.R:518`). Under `loss_share` that is the mean payment in paid historical years (0 if never paid), so "Coverage of the poor" means "paid at least once historically"; under a fixed amount with layers it means "statically eligible", including households the layers never pay. One label, two definitions. | Code reading (`fct_policy_sim.R:1812`). | Compute coverage from static eligibility and the amount from the per-activation amount separately, or state the definition per budget rule. |
| PR-9 | M | Impact-trigger controls reuse weather wording: the pill reads "Trigger on: Weather level", the threshold help says "where the variable is typically more extreme", and there is no unit, historical range or record-length line for the metric (open point in section 2). The user types a loss percent or a number of poor without a reference. | UI reading (`mod_3_01_sp.R:293-380`). | Relabel the pill ("Fixed level"), show the metric unit and historical percentiles from the plan's historical metric column. |
| PR-10 | M | Two different "1-in-20" costs on the Cost-effectiveness tab: the climate table's "1-in-20 year cost" is the 95th percentile of annual cost (`sp_shock_summary()`); the cost-effectiveness row "1-in-20 year" uses the cost in the baseline-anchored adverse welfare year (`sp_shock_cost_by_scenario()`). | Code reading. | Rename the climate column ("95th percentile annual cost") or add a note. |
| PR-11 | M | Accessibility: the new `checkboxInput(ns("dyn_welfare"))` places `info_popover()` (role button, tabindex 0) inside the checkbox label, the only checkbox in the app that does. Nested interactive control: clicking the icon can toggle the checkbox, and screen readers read "More information: ..." as part of the checkbox name. | `mod_3_01_sp.R:474`; `grep` over `mod_*.R`. | Put the popover beside the label (the `sp-inline-label` pattern). Run `tests/e2e/smoke.R` on Step 3 with a shock program. |
| PR-12 | L | Sidebar preview speed: 0.7 to 0.8 s per debounced change on BFA; about 60 % is `sp_trigger_thresholds()` (`stats::quantile` per cell via `vapply`, 0.34 s for 10,350 cells), recomputed when only the amount, scope or a layer changes. It scales with the number of cells (IRN larger). `.sp_trigger_cell_sd()` has the same pattern. | `Rprof` of five previews. | Grouped type-7 quantile in one pass (order by cell and value, then index arithmetic), or cache thresholds on (trigger fields, exposure identity). Benchmark per guidelines section 9. |
| PR-13 | L | Every metric-decomposition job re-evaluates the shock state per pipeline (`fct_policy_metric_decompose.R:729`: impact column, layers, forecast). P2-9 measured only the correction loop (`policy_seconds`), not metric switches with Phase 2 features on. | Code reading. | Report metric-switch times for `shock_sp_*` fixtures; cache the per-pipeline transfer in the retained Step 3 artifact if it matters. |
| PR-14 | L | Harness drift: the Step 3 benchmark builds the shock plan without `level_factor` and `seed` (`dev/bench_step3_helpers.R:428-437`), unlike the app; a loss-share fixture or an LCU outcome would benchmark a different plan. No `loss_share` or cap fixture exists (row P2-6). | Code reading. | Build the plan as `apply_policy_delta_to_baseline()` does and add a `shock_sp_budget` fixture. |
| PR-15 | L | After the loss-event share is edited, Diagnostics re-scores live while the "Shock-affected poor" table keeps the run's share (labelled). Two shares on one screen. | Code reading. | Note the difference next to the table or offer a re-run hint. |
| PR-16 | L | Plan rule 10 says draws are never keyed by row position, but targeting-error and welfare-layer draws index `runif(n_svy)` by survey row position in the frame; a reordered survey frame changes them (prediction-row order is safe). | `.sp_dyn_uniform()`. | Key by a stable household id, or restate the rule as "survey row of the run's frame". |
| PR-17 | L | Polish: `t_weather <-proc.time()` spacing (`fct_run_simulation.R:1052`); mis-indented `conditionalPanel` block (`mod_3_01_sp.R:300-333`); the forecast help says "seeded by location and year" but the draw is per (survey round, location, interview month, year); "Poorest at the trigger (bottom x%)" selects more than x % in bad years because the cut is a quantile of historical mean welfare (the help says so). | Code reading. | Fix text and formatting. |

### 7.3 Resolution (14 October 2026, uncommitted on `dev`)

Decisions taken with the maintainer: PR-1 trigger-config poverty line; PR-2 rank by own modelled loss; PR-3 forecast draws follow the trigger unit (new decision 15, decision 10 revised, plan rules 10 and 15 updated).

| ID | Status | What changed |
|---|---|---|
| PR-1 | fixed | Spec field `trigger_pov_line` (outcome units per day) with a numeric input shown for poverty metrics, prefilled from `so$povline`, kept when edited or imported. Enters the run signature and export. `pov_line` argument stays as a fallback. The reach table falls back to the outcome's line. |
| PR-2 | fixed | Impact triggers rank cells by own modelled loss (`.sp_cell_loss_depth()`). BFA: paid locations no longer the alphabetical first. |
| PR-3 | fixed | `.sp_forecast_unit()`: year (national impact), area (admin), cell (location impact or local weather), gate (weather trigger with a national scope: the gate is forecast). BFA, gate k = 20 %, POD 100 %: FAR 20 % now adds 1.9 % to mean cost (was +84 %). |
| PR-4 | fixed | `hit` is the realised payout scope. BFA national_all, perfect forecast: false-alarm share 0 (was 63 %). |
| PR-5 | fixed | Rows renamed; added `breakeven_m_cost` (per dollar of spending), preview card row and test. |
| PR-6 | fixed | Basis risk scores a cell as paid when any row is in the final scope. Test updated. |
| PR-7 | fixed | Anticipatory worst-affected ranking uses a seeded (cell, year) draw, no realised depth. |
| PR-8 | fixed | `.sp_shock_design_transfer()`: recipients are the statically eligible households under every budget rule; help text states it. |
| PR-9 | fixed | Pill renamed "Fixed level"; impact unit and historical min/median/max shown (`impact_range` from plan and preview). |
| PR-10 | fixed | Climate column renamed "1-in-20 year cost (95th percentile)"; note on the tab. |
| PR-11 | fixed | Predicted-welfare popover moved beside the checkbox. Axe/browser pass still owed. |
| PR-12 | fixed (part) | Grouped quantile `.sp_group_quantile()` (identical thresholds): thresholds 0.34 to 0.07 s, preview about 0.8 to 0.5 s on BFA. A threshold cache was not added. |
| PR-13 | measured, open | State plus transfer over the 17 BFA pipelines costs 1.7 s (base) to 3.5 s (anticipatory + layers, impact at location level) per metric job. Caching it in the retained artifact is not done. |
| PR-14 | fixed | Harness builds the plan with `level_factor` and `seed`, passes `seed` to the correction, new fixture `shock_sp_budget`. |
| PR-15 | fixed | Note on the reach table. |
| PR-16 | fixed (docs) | Plan rule 10 restated; draws still index by survey row position of the run's frame. |
| PR-17 | fixed | Spacing, indentation, forecast help text. |

Checks: full `devtools::test()`: 1388 tests, 0 failures. About 12 new tests in `test-sp-shock-correction.R` and `test-fct_sp_shock.R`. BFA benchmark (correction loop, ols, one SSP, disabled / enabled, median of 3; P2-9 figures in brackets): `shock_sp` 8.0 / 7.8 s [7.8 / 7.8], `shock_sp_dyn` 9.0 / 9.1 [8.9 / 8.9], `shock_sp_impact` 8.8 / 8.7 [8.8 / 8.8], `shock_sp_antic` 10.2 / 10.2 [9.5 / 9.5], new `shock_sp_budget` 9.2 / 9.1, `targeted_sp` 4.0 / 3.9 [3.9 / 3.9]. The anticipatory fixture is about 0.7 s slower (realised scope is now computed alongside the forecast); the others are within noise. The full test suite ran while the benchmark process was still finishing, so treat timings as upper bounds. IRN not run. Browser and axe passes (section 3) still owed.

Suggested order: PR-1, PR-2 and PR-3 before the browser pass in section 3 (they change what the pass would check), then PR-4 to PR-11 with the pass, PR-12 to PR-17 as time allows.
