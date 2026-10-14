# Social protection: remaining tasks (Phase 2 and 3)

Companion to `review/sp_shock_responsive_plan.md` (design rules, Phase 2 and 3 design). Updated 2026-10-08.

Phases 0 and 1 are built, verified and committed on `dev` (495ae85 to 7545ab3); what each task did is in the git history. Section 1 keeps only what is still owed from them. Sizes: S under a week of focused work for one person, M one to three weeks, L more. They come from reading the code, not from prototyping.

Status key: `☐` todo · `◐` partly done · `☑` done · `✗` not doing.

## 0. Conventions

- Task ids: `P0-n` (Phase 0 leftovers), `P1-n` (Phase 1 close-out, closed), `P2-n`, `P3-n`. A spike ends in a written answer and may change later tasks.
- Every task lists files, dependencies, "done when" and tests.
- Repository rules: `fct_` files stay free of Shiny; touch only files the task needs; run the narrowest meaningful check (`devtools::test(filter = "...")`); run `devtools::document()` when roxygen or exports change; no commits or pushes unless requested; code, comments and commit messages use ASCII quotes and hyphens.
- Reproducibility (plan rule 10): same inputs and seed give identical outputs regardless of row order, chunk size and thread count; preview equals run; every new spec field enters the run signature and the export; every task that changes numbers or draws carries a determinism test.
- Any change in the Step 3 hot path (the annual correction loop) needs the benchmark in `review/optimization_guidelines.md`; the harness has a `shock_sp` fixture (`WISEAPP_STEP3_POLICIES=targeted_sp,shock_sp WISEAPP_STEP2_INCLUDE_STEP3=1 Rscript dev/bench_step2.R`).
- Tests are named after the file or concept they cover (`test-fct_sp_shock.R`, `test-sp-shock-correction.R`, `test-sp-reach.R`).
- New tasks read `sp$currency` and use `outcome_level_scale()` / `.policy_sp_transfer()`, never the raw transfer column. Loss-based outputs use the plan's rule 7 definition; poverty lines follow rule 15.

## Decisions (8 October 2026)

| # | Decision |
|---|---|
| 1 | Finish the Phase 1 close-out before Phase 2 feature work (done). |
| 2 | "People lifted" is the net change in the number of poor, labelled as such. Gross movements are not built (the annual aggregates keep no household-level before/after status). |
| 3 | The cost-effectiveness section leads with cost per person lifted when the metric is a poverty rate, and welfare gain per $1 otherwise. Near-zero or wrong-signed effects print "not applicable", never an extreme ratio. |
| 4 | Cost-effectiveness gets a dedicated Step 3 tab (not a Results section). |
| 5 | Scope: cost-effectiveness applies to cash transfers only, and only to the modelled benefits and costs listed in the tab (transfer, administration, modelled welfare and poverty effect). Other levers (`mod_3_02` to `mod_3_05`) are out of scope: no unit costs exist. The tab must state this. |
| 6 | The decomposition channel split (P2-3) comes right after P2-1 and P2-2. |
| 7 | P2-6 builds share-of-loss budgets with a ceiling and the cap with pro-rata scale-down. AAL sizing, poverty-linked budgets and the envelope per activation move to Phase 3 (P3-5). |
| 8 | P0-8 (weighted error counts) was done before the close-out as a correctness fix. P0-9 (admin presets, error defaults) is deferred. |
| 9 | Cost-effectiveness uses one seeded targeting draw (no repeated-draw range) and the Results poverty line input. |
| 10 | Poverty lines (plan rule 15): run-defining features (impact triggers on poverty, poverty-linked budgets) use the Step 2 line fixed per run and show "not available" without one; display metrics follow the Results line and print it. |
| 11 | Build order after P2-4: P2-6 (budgets) before P2-5 (impact triggers), then P2-8, then P2-7. P2-6 needs only the Phase 1 household loss (rule 7). P2-8 does not need the impact metrics. |
| 12 | P2-1's loss by decile uses rule 7 (level scale, back-transformed, weighted, no residual draws), deciles of the variable over household-years. |

## 1. Closed work: what is still owed

Phase 1 (P1-13 to P1-20) is closed. P1-13, P1-14, P1-15 (production run) and P1-20 were ticked on the user's report; I (the assistant) did not run them. Side effects of that work: the real-app run exposed a stale historical policy arm (shared aggregation cache keyed on Step 2's signature only; fixed, with a regression test); the SP sidebar order and amount-basis text changed (plan section 1); `.sp_trigger_unit()` shows the unit of transformed trigger variables; `.policy_annual_channels_reference()` is documented as regular-only (shock parity is covered by `test-sp-shock-correction.R`); `SP_ELIGIBLE_COL` needs no entry in the covariate exclude lists (it is logical, and only the policy frame carries it).

Owed by the user, to record in `review/REVIEW-2026-10-06-tracking.md`:

- BFA before and after headline numbers for P0-8 (weighted error counts). Synthetic check: realised weighted inclusion 15.8 % and exclusion 12.2 % before, 15.0 % and 10.0 % after, for 15 % / 10 % sliders.
- BFA and IRN Step 3 benchmark figures with the shock fixture (known BFA correction-loop figure: 7.0 s shock vs 3.6 s regular).
- The BFA LCU re-run (R2-BUG-04 / CR-BUG-02) entry for the Decision log.

Phase 0 leftovers: **P0-5** (poverty effect per cost) moved to P2-2. **P0-9** (admin presets and error defaults) is deferred: literature pass on cost-transfer ratios and PMT error rates, then a preset dropdown; the 10 % / 10 % defaults and a zero admin share stay, and the cost-effectiveness tab prints the admin share beside every figure. Owner: the project owner.

## 2. Phase 2: order and tasks

**Order (decisions 6, 11):** P2-1, P2-2, P2-3, P2-4 (after spike P2-0a), P2-6, P2-5 (after spike P2-0b), P2-8, P2-7 (after spike P2-0c), P2-9. P2-1 and P2-2 come first: the response check fixes the BFA problem (users picking a trigger variable that does not drive loss) and the outputs finish the finance-ministry story; neither touches the hot path. P2-3 comes while the numbers are stable and before more terms are added to Main. P2-7 goes last because it adds the most assumptions.

### P2-0: Spikes (S each, before the tasks they gate)

- **P2-0a binned-variable cost (gates P2-4). Done 13 October 2026: GO for option 1, with the conditions below.** Method: the Step 2 harness with `WISEAPP_STEP2_WEATHER_FORM=Continuous` and `Binned` (t and spei6 both binned), ols, cold cache, 1 repetition, in memory mode. A numeric column is 8 bytes per row and a factor 4, so a companion adds 8 bytes per row per binned variable on top of the binned frame, which is twice the Continuous-minus-Binned difference measured below. Results: BFA historical weather frame 13.7 MB binned (310,000 rows), +10 MB with companions (+24 % of the result object); BFA one SSP and one period result 240.8 MB binned, +94 MB (+39 %); BFA three SSPs and three periods 1,831 MB binned, +770 MB (+42 %); IRN historical weather frame 51.7 MB binned (1.17 million rows), +38 MB (+20 % of the result object). IRN future members were not run (the harness requested one key for IRN). The growth is proportional to weather rows, so it applies to the reference weather store on disk (`step2_weather_store_put()`, one RDS per member) and to the exposure table rebuilt in workers; the worker snapshot carries store references, not frames, so the payload does not grow beyond what the workers read from the store. Caches: the weather disk cache stores `loc_monthly`, the location-month table before rolling, transformation and binning, and its key does not depend on binning; `.apply_binning()` runs only after collect (three call sites), so no cache holds binned values and none needs a new version. Conditions for P2-4: (1) keep the companion only for variables set to Binned, as a numeric column holding the transformed pre-bin value (the same value the bins were cut from); (2) it must never reach a model matrix: bit-identical predictions on ols, rif, ranger and xgboost fits are the gate; (3) accept roughly +40 % on the weather part of the Step 2 result and store, which is the price of the trigger on a binned variable; if that is too much on IRN-size runs, fall back to option 2. Original task text: Measure the extra memory and worker payload of a companion `<var>_cont` column per binned variable per member on BFA and IRN, and confirm the prepared-weather and weather-disk caches store pre-binning values. Output: go or no-go for option 1.
- **P2-0b aggregate loss metrics (gates P2-5). Done 14 October 2026: GO, with the scope below.** Method: BFA, ols, historical 1991-2020 (30 years) plus SSP2-4.5 / 2025-2035 (16 members), Step 2 harness result saved to scratch, central `y_point` back-transformed (log outcome), rule 7 loss against each household's own historical mean, poverty line 3.00 (PPP, the harness default; the app uses `hs$pov_line`), survey weights. Results: (1) **National** (one series of 30 years): loss, poverty rate and number of poor are all well behaved; within-series correlation of loss with `spei6` is -0.97 (with `t` 0.36), of poverty rate with loss -0.98. A historical 1-in-10 trigger fires in exactly 10 % of historical years for every metric; under the 16 SSP members the same threshold fires in 31.5 % of member-years on average for loss (members range 0 to 60 %) and 34.2 % for poverty (0 to 63 %). (2) **Survey location** (596 clusters, median 12 sampled households and 25,000 weighted people per location-year): loss is well behaved (1-in-10 history frequency 0.10, cor with `spei6` -0.97), **poverty is not**: 99.5 % of location-years have a poverty rate of exactly 0 or 1, so a 1-in-10 threshold fires in 91.5 % of historical years (ties). (3) **Correction (14 October, user prompt):** the raw survey files do carry GAUL 2024 admin codes, `gaul1_code` (13 to 14 regions) and `gaul2_code` (42 to 45 provinces); they are in `variable_list.csv` as fixed-effect candidates. A second correction followed: the Step 2 survey frame holds every column of the source file, whatever the model uses, so both codes are in `hist_sim$svy` (87 columns on BFA) and no join is needed; my first look missed them because my column search did not include "gaul". The numbers below come from a re-run with the 2021 codes joined by `loc_id` (597 of 597 locations map to one code), which gives the same codes: admin 1 (13 units, median 527 households per unit-year) works for loss (1-in-10 history frequency 0.10, 0.32 under SSP members) and for poverty (0.13 from mild ties, 0.29 under members); admin 2 (41 units, median 132 households, minimum 36) works for loss (0.10, 0.30) but poverty has ties (11.6 % of unit-years at 0 or 100 %, frequency 0.39) because many provinces have a poverty rate of 0 in most years. A 23-cell lat/lng grid behaved like admin 2. This led to the "no adverse tail" rule in P2-5. (4) Within a unit the weights are fixed across years, so **number of poor is proportional to the poverty rate**: identical activations for every return-period trigger (also nationally); only an absolute threshold differs (people instead of percent). (5) Cell size: a trigger cell (round, location, interview month, year) holds 1 to 12 households (median 12 across 21,000 cell-years that have households); impact metrics are therefore defined per survey location (months pooled), not per cell. Decision for P2-5: metrics `loss` (percent of own typical welfare, positive = loss), `poverty_rate` (percent) and `poor_count` (survey-weight people), at `national` and `location` level, direction always "above" (more loss, more poor is adverse); poverty metrics at location level need a minimum number of households per location-year (30, unweighted) and are otherwise not available; poverty metrics need the Step 2 line (rule 15). Original task text: Prototype location and national modelled loss, poverty rate and number of poor per (member, year) from the central predictions, using the Step 2 poverty line (rule 15); check behaviour of the historical thresholds and the record-length rule on real BFA exposure. Output: which metric is defined where, and the trigger input units.
- **P2-0c forecast skill (gates P2-7).** Decide POD/FAR input versus noisy hazard copy, the seeded key (location, year, member), and how a false alarm is costed. Output: written choice.

### P2-1: Response check for the trigger variable (S to M) ☑

**Status (13 October 2026):** built, unit-tested and checked in the browser by the user (the decile table is collapsed under the hint). `sp_response_check()` in `R/fct_sp_shock.R` reuses `sp_shock_reference()` and the `level / reference - 1` loss of `sp_shock_pipeline_rows()` (rule 7; log outcomes back-transformed before averaging). Weighted deciles over household-years (equal values stay together, so a coarse variable gives fewer bins); direction `above` / `below` / `either` / `none` from the mean loss in the outer fifth of the deciles against the middle (an end counts from 1 percentage point deeper; both ends count when the shallower is at least half the deeper). The hint names wet / dry (spi, spei, precipitation names) or hot / cold (temperature names) years. Unusable variables return `problem`, not an error. The Trigger settings flyout (`output$trigger_response`) shows the hint, the decile table and, for a valid spec, the preview's false-negative and false-positive rates. Tests: five blocks in `test-fct_sp_shock.R` (hand-calculated weighted bins, log back-transform, one- and two-sided direction, unusable variables, determinism and row order, module render). Thresholds (20 % ends, 1 point, one half) are judgement calls, flagged for review.

Mean modelled loss by decile of the chosen variable from the Step 2 historical predictions (rule 7 loss; decision 12); show where losses concentrate, suggest high, low or either end, and show the loss-event share's basis risk beside it. Pure function in `fct_sp_shock.R`, shown in the Trigger settings flyout.

- Files: `R/fct_sp_shock.R`, `R/mod_3_01_sp.R`.
- Done when: on BFA the hint says "wet years, high `spei6`" (not `t`); an unusable variable shows a message, not an error; deterministic.
- Tests: hand-calculated deciles on a small panel (level scale, weights); direction suggestion on a one-sided and a two-sided response; module render.

### P2-2: Shock outputs and the cost-effectiveness tab (L) ◐

**Status (13 October 2026):** built, unit-tested and benchmarked on BFA; not yet checked in a browser.

- Engine (`R/fct_sp_effectiveness.R`): `sp_effect_by_scenario()` joins the Results expected effect (with coefficient SD) and the baseline-anchored 1-in-20 effect; `sp_cost_effectiveness()` gives cost per person lifted (net) for the poverty rate, poverty gap closed per $1 for the gap, and welfare gain per $1 for mean and population sum of a money-per-day outcome; "not applicable" for a near-zero (under 0.01 pp of the population), wrong-signed or unspent result, "not available" without a poverty line, "not bounded" when the interval reaches zero; every row is "not applicable" when another lever is active (the effect cannot be credited to the transfer). `sp_effectiveness(shock = TRUE)` reports who can be paid and the cost of a year in which every eligible household is paid; the "Not reported for a shock-responsive program" notice is gone.
- Shock cost (`R/fct_sp_shock.R`): `sp_shock_cost_by_scenario()` pools the annual cost over the effect's own matched member-years and applies each member's adverse-year support (`apply_adverse_year_support()`) to the cost, so numerator and denominator describe the same years; `sp_shock_climate_cost()` is the climate-adjusted cost table (activation and mean cost against the historical arm).
- Tab (`R/mod_3_10_cost_effectiveness.R`, wired in `R/mod_3_scenario.R`, results API extended in `R/mod_3_07_results.R` and `R/fct_policy_sim_compare.R`): scope statement, context line (program, metric, Results poverty line, administration share), the cost-effectiveness table, notes, and for shock runs the climate-adjusted table. Exports: `policy_cost_effectiveness` (numeric, with metric, line, admin share, currency and program) and `policy_shock_climate_cost`.
- Tests: `test-fct_sp_effectiveness.R` (hand-calculated ratios and intervals, near-zero / wrong-sign / unbounded, cost pairing, row order, climate table, shock design figures), `test-mod_3_10_cost_effectiveness.R` (module render, empty states, other levers, shock pairing, exports), a Diagnostics block in `test-fct_sp_shock.R`.
- Coverage and adequacy of the shock-affected poor (`sp_shock_reach()`, tab section "Shock-affected poor", export `policy_shock_affected_poor`): over household-years with a household-level loss event (loss of at least the run's loss-event share of the household's own typical level), the share in which a baseline-poor household was paid, and payments as a share of the modelled loss. The poor flag follows the Results line (rule 15), so the run stores line-free per-survey-row, per-scenario sums inside the correction loop (`.sp_shock_reach_rows()`, published as `shock$reach`) and the line is applied afterwards. The counts use the loss-event share at the time of the run; a later change of the share rescoring Diagnostics does not change them (the tab prints the share used). Household-level events can differ from the location-level events of basis risk once the model has weather-covariate interactions.
- Hot-path benchmark (BFA, ols, one SSP and one period, 3 repetitions, `targeted_sp` and `shock_sp`; before = ede252c, after = this change): correction loop (`policy_seconds`, median) shock 7.16 s -> 7.34 s (+0.18 s, +2.5 %), regular 3.50 s -> 3.67 s (unchanged code path, run-to-run noise); output fingerprints identical for all four ols cases (disabled and enabled uncertainty, both fixtures); peak parent RSS 3.29 / 3.47 GB -> 3.08 / 3.09 GB (not higher). IRN not run (small BFA case agreed as adequate).
- Open points to check in the app: the tab sits after Diagnostics and before Decomposition; scenario names of the historical arm must match between the Results series and the shock rows (the code relabels the "Historical" member with the Results label, but this was only tested with stubs).

Climate-adjusted cost; coverage of the shock-affected poor; adequacy; poverty effect per cost in average and 1-in-20 years (closes P0-5); and a dedicated cost-effectiveness tab that works for regular and shock cash transfers. Most inputs already exist in `out$shock$rows` and `$cells` and in `sp_effectiveness()`. Decisions 2 to 5 and 9 apply (net people lifted, lead metric by outcome type, "not applicable" for near-zero or wrong-signed effects, dedicated tab, one seeded targeting draw, Results poverty line printed in the tab).

- Scope statement shown in the tab: cash transfers only; only the modelled transfer, administration, welfare and poverty effects count; delivery of other levers, targeting surveys and financing are excluded; other levers have no cost-effectiveness figure. Admin share shown beside every figure.
- Files: new tab module (`R/mod_3_10_cost_effectiveness.R` or the next free number, wired in `R/mod_3_scenario.R`), `R/fct_sp_shock.R`, `R/fct_sp_effectiveness.R`, `R/mod_3_08_diagnostics.R` (move or link the existing effectiveness table), export registration in `R/fct_export.R`. Design and arithmetic: `review/step3_cost_effectiveness_plan.md` (its section 8 now points to plan section 3.8).
- Done when: metrics match hand calculations on a small panel; the "not reported for a shock program" notice is gone; exports carry the new tables; poverty line absent shows "Not available"; the tab is empty-state safe when the SP lever is off.
- Tests: extend `test-fct_sp_shock.R` and `test-fct_sp_effectiveness.R`; export test; a module render test; `app_server` wiring test.

### P2-3: Separate decomposition channel (L) ☑

**Status (13 October 2026):** built, unit-tested and benchmarked on BFA; not yet checked in a browser. Both decompositions carry the shock-responsive transfer as its own channel; a regular run has the channel at exactly 0 and shows nothing new.

- Compact (Decomposition tab) stats: new channels `delta_sp_shock` and `lvl_sp_shock` (`.compact_decomp_channels`, `.compact_level_channels`); `delta_main`, `delta_sp` and `lvl_main` are now the static values, so static + shock is the old combined value. Repositioning, interaction and the totals are computed from the combined effect and do not change. Compact frames built before this change read the new channels as 0 (`.compact_optional_channels`).
- Metric-aware states (Results cards, tails): five states (baseline, after static main, after main, after repositioning, policy) and a new `sp_shock` contribution; `main` is static; `main + sp_shock + repositioning + interaction = total`. Without a shock program the extra state is copied, not aggregated again.
- Presentation: the headline chart, decile chart, decile table and exports show "Shock-responsive transfer" only when it carries an effect; the Results resilience card popover lists it beside the main effect.
- Tests: identity of static + shock against the household-level channels of the same run for identity and log outcomes, level channels adding to the level total, a regular run (zero channel, `after_static_main == after_main`, no shock row in the summary), the headline summary with and without the channel, and updated fixtures in `test-fct-policy-metric-decompose.R` and `test-sp-shock-correction.R`.
- Benchmark (BFA, ols, one SSP and one period, 3 repetitions; before = 87aaf15): correction loop (`policy_seconds`, median) shock 7.34 s -> 7.59 / 7.63 s (+0.25 s, +3.5 %), regular 3.67 s -> 3.74 / 3.87 s (noise); metric decomposition job (`metric_headcount_ratio_seconds`) shock 4.09 -> 4.28 / 4.45 s, regular 2.57 -> 2.65 / 2.66 s; peak parent RSS not higher (3.04 / 3.07 GB). The harness output fingerprint changes for all four cases because it hashes the compact channel summary, which has new columns; the old columns were not byte-compared on BFA (the tests prove the identities instead).

Split `delta_sp_shock` out of Main into its own channel in the compact stats, level channels, summaries and exports. Numbers must not change, only grouping.

- Files: `R/fct_policy_metric_decompose.R`, `R/fct_decomposition_summary.R`, `R/mod_3_09_decomposition.R` (`.compact_decomp_channels`, `:109`), `R/fct_export.R`, and the three consumers of the correction.
- Done when: totals, Main plus the new channel, equal the old Main for every method and metric; the channel identity checks pass; labels and exports updated; regular runs show no new channel.
- Tests: identity before and after on a shock fixture; snapshot updates for exports; the three consumers agree. Hot-path benchmark.
- Risk: the largest uncertainty in Phase 2 (touches snapshots and exports).

### P2-4: Binned weather triggers (M, after P2-0a) ☑

**Status (13 October 2026):** built, unit-tested and checked on real BFA data; not yet checked in a browser.

- `get_weather(keep_continuous = FALSE)` and `.apply_binning(keep_continuous)` keep each binned variable's transformed pre-bin value in a numeric companion `<var>_cont` (historical frame and every member, one change in `.apply_binning()` serves all three call sites). Step 2 (`fct_run_simulation.R`, four call sites) asks for it; Step 1 weather statistics and every other caller are unchanged. The prepared-weather cache signature includes the flag, so no cache written before this is served.
- `run_sim_pipeline()` strips the companions from the frame it joins and predicts with, so predictions cannot depend on them, and keeps them in `weather_raw` and the exposure table (`.weather_companion_cols()`). `.sp_trigger_variables()` already offers numeric exposure columns, so the companion qualifies and the binned factor does not; the Trigger flyout lists it as "<label> (continuous)" and reads its unit from the binned variable.
- Equivalence gate (real BFA data, binned `t` and `spei6`, historical and one SSP / one period, ols and rif, old code from a git worktree against new): prediction fingerprints (`y_point`, `sim_year`, `svy_row_id`, `weight` of the historical arm and every member) identical in all four cases. Cost: result object +9.9 MB (historical) and +94.4 MB (one SSP, one period), matching the P2-0a estimate; weather stage 2.0 / 10.4 s vs 2.1 / 10.7 s, pipelines and parent peak RSS within run-to-run noise. The harness gained `prediction_fingerprint` (ignores stored weather) for this kind of comparison. Not run on ranger or xgboost fits, which cannot see the companions by construction (the joined frame never has them).
- End to end: the Step 3 harness shock fixture runs on binned BFA weather with the companion as its trigger variable (status ok, correction loop 7.9 s, in line with the continuous case).
- Tests: `test-weather-continuous-companion.R` (companion only when asked, same bins, bit-identical predictions with and without it for inline and cached joins, exposure table, trigger thresholds on the companion, binned variable refused, flyout candidates and units). The exposure-mapping fixture moved to `helper-policy-exposure.R` because two files use it.

Option 1 from plan section 3.2 if P2-0a passes: companion continuous column kept before `.apply_binning()` in the historical slice and every member, added to `weather_columns`, offered through `.sp_trigger_variables()` with its unit.

- Files: `R/fct_get_weather.R` (three call sites), `R/fct_simulations.R` (`weather_columns`), `R/fct_sp_shock.R`.
- Done when: existing model matrices and outputs are bit-identical (equivalence gate); a binned variable can be used for weather-level and return-period triggers; the companion is never a model term.
- Tests: companion excluded from model matrices; bit-identical outputs for existing runs; trigger thresholds on a binned variable equal those on its continuous source. Benchmark per the optimization guidelines.
- Fallback: option 2 ("bin at or beyond k") if P2-0a is no-go.

### P2-6: Budgets linked to need (M) ☑

**Status (13 October 2026):** built, unit-tested and benchmarked on BFA; not yet checked in a browser. Scope change agreed with the user: one annual cap on total cost (including administration), no separate per-cell ceiling.

- Spec fields `budget_rule` (`fixed` | `loss_share`), `loss_share_pct` (0 to 100) and `annual_cap` (blank = none), validated in `.sp_shock_problem()`; they are in the SP list, so in the run signature and the export config. `has_sp_change()` needs a share instead of an amount under `loss_share`.
- Loss share: a cell (survey round, location, interview month, year) gets a budget of `s` times the weighted modelled loss (rule 7; gains count as zero) of its statically eligible households in the payout scope, shared equally per household or per person (`.sp_shock_loss_share_transfer()`). Under the national gate with all targeted households each location is budgeted from its own loss. Documented in the Budget rule popover.
- Annual cap (`.sp_shock_apply_cap()`): applies to both rules; a year whose transfer cost would exceed `cap * (1 - admin share)` has every payment scaled by the same factor, so total cost equals the cap in that year.
- `sp_shock_pipeline_transfer()` is now the single place that builds the per-row transfer (run, attribution and sidebar preview all call it). New row columns `cap_binds`, `loss_value`, `paid_value`; new summary columns `cost_cv`, `loss_offset_share` (payments over all modelled loss) and `cap_exhaustion_share`; shown in the Diagnostics shock table and the sidebar card. The published `per_household` is, under `loss_share`, the mean payment in the historical years a household was paid.
- Share of affected poor not covered: not repeated beside the rule; it is `1 - coverage` in the cost-effectiveness tab's "Shock-affected poor" section (Results poverty line).
- Tests (`test-sp-shock-correction.R`): fixed rule and a distant cap bit-identical to Phase 1; hand-calculated loss share (own cell and shared cells, both amount bases); proportionality in the share; cap never exceeded, pro rata, binding years equal the cap; summary columns; row-order and repeat determinism; preview equals run; spec checks.
- Benchmark (BFA, ols, one SSP and one period, 3 repetitions, `shock_sp`, fixed rule; before = 0e26b49): correction loop (`policy_seconds`, median) 7.51 s -> 7.66 s (+0.15 s, +2 %); output fingerprints identical (disabled and enabled uncertainty); peak parent RSS not higher (3.58 / 3.69 GB, run-to-run noise). The `targeted_sp` fixture is unchanged code path. IRN not run.
- Not covered: a loss-share or capped run through the harness; LCU cost weights in the cap (the formula mirrors `.sp_transfer_totals_values()` but is only unit-tested in PPP).

Original scope text (decision 7): budget as a share of modelled loss (rule 7) with a ceiling, and the cap with pro-rata scale-down. Report coefficient of variation of annual cost, share of loss offset, share of affected poor not covered, and budget exhaustion (share of years the cap binds) beside each rule.

- Files: `R/fct_sp_shock.R`, `R/fct_policy_sim.R`, `R/mod_3_01_sp.R`, `R/mod_3_08_diagnostics.R`.
- Done when: budget identity holds (total = net transfers + admin); an envelope is never exceeded; the ceiling binds as set; exhaustion share reported; new spec fields in the run signature and export.
- Tests: identities, ceiling, monotonicity in the share, determinism; preview parity. Hot-path benchmark.

### P2-5: Impact-based triggers (M to L, after P2-0b) ◐

**Status (14 October 2026):** engine, UI, tests and BFA benchmark done; not checked in a browser; not committed. Scope follows the P2-0b result.

- Spec fields `trigger_metric` (`weather` default | `loss` | `poverty_rate` | `poor_count`) and `trigger_level` (`location` | `national`), validated in `.sp_shock_problem()`; for an impact metric the weather variable and direction are not read and the trigger always fires on high values. Threshold units: loss in percent of the households' own typical welfare (positive = loss), poverty rate in percent, number of poor in survey-weight people.
- Mechanism (`.sp_impact_column()`): per (unit, simulation year), the unit being the whole sample or a survey location (interview months pooled), from the pipeline's central predictions with the rule 7 loss and the plan's household reference. The column is added to the exposure table (`SP_IMPACT_COL`), so thresholds (weather level or 1-in-N with the N-year record rule), trigger state, payout scopes, the national gate, the severity ranking of P2-8, basis risk and the loss-share budget are the weather trigger's own code. Thresholds come from the historical pipeline and apply unchanged to climate members; the metric of a member comes from that member's pipeline (state function, so run, preview and attribution agree).
- Poverty (rule 15): uses the Step 2 line `hist_sim$pov_line`, fixed per run; without it the plan aborts with "A trigger on poverty is not available: Step 2 has no poverty line." (shown by the preview). Poverty at survey-location level needs `SP_IMPACT_MIN_HH` (30) sampled households per location-year, otherwise the value is missing and never fires; when no location qualifies, the plan aborts with a message that points to the national level. For a return period the rate and the number of poor fire in the same years (fixed weights).
- **Levels (added 14 October after the admin-code finding):** `trigger_level` is `national`, `admin1` (`gaul1_code`), `admin2` (`gaul2_code`) or `location`. The area codes come from the run's survey frame (`svy` of the plan); the level select lists only the levels the survey frame supports (`.sp_impact_level_choices()`: an admin level needs at least two distinct codes, location needs `loc_id`; refreshed when Step 2 changes, keeping the current choice if still offered), and a column missing at plan time (another data source, an old config) gives "The survey data of this run has no 'gaul2_code' ... Use another level." Poverty below national level needs 30 sampled households per unit-year. No adverse tail: for an impact trigger a cell whose 1-in-N threshold equals its historical minimum (a poverty rate of 0 in most years) never fires, and the plan aborts with a message when that holds everywhere (otherwise `>=` the minimum would fire every year). Units are not split by survey round (Step 2 uses the latest wave).
- API: `sp_shock_plan(hist_exposure_map =, pov_line =)` (callers: `apply_policy_delta_to_baseline()`, `sp_shock_preview()`, the benchmark helper).
- UI (Trigger flyout): "Trigger measures" select with an explanatory popover (labelled as a benchmark: a real program needs an impact estimate this timely), survey-location / national toggle, weather variable, response check and direction hidden for impact metrics; summary line shows the metric.
- Tests (`test-sp-shock-correction.R`): hand-calculated national and location metrics (loss, rate, count), missing rows, poverty without line and with too few households, never- and always-fires for each metric (and the always-on impact trigger pays what an always-on weather trigger pays), a 1-in-2 trigger fires in the worse year with rate = count, prediction row order, preview equals run (including the message without a line), spec checks. Suites green: `sp-shock-correction`, `fct_sp_shock`, `sp-reach`, `fct-policy-metric-decompose`, `mod_3_06`, `mod_3_10`, `determinism`, `step3_async`, `ui-migration-step3`, `bench-step3`, `policy-sim-compare`, `weather-continuous`.
- Benchmark (BFA, ols, one SSP and one period, 3 repetitions, cold; the `shock_sp` fingerprints equal the P2-8 run): correction loop `shock_sp` 7.55 s (disabled) / 7.52 s (enabled), new harness fixture `shock_sp_impact` (national modelled-loss trigger, 1-in-5) 8.19 s / 8.18 s (+0.65 s, +8.5 %); peak parent RSS not higher. IRN not run.
- Not covered: the trigger on real BFA data was checked in the spike (activation 10 % in history, 31.5 % under SSP2-4.5 members for a national 1-in-10 loss trigger) but the end-to-end app flow and the new controls were not seen in a browser; no response check or record-length text for impact triggers; location-level poverty is unusable on BFA (12 sampled households per location).

Trigger on modelled loss, poverty rate or number of poor at location and national level. New `trigger_type` values, thresholds on the historical distribution of the metric (same record rule), applied unchanged to SSP members. Household-level modelled quantities only as a labelled oracle.

- Files: `R/fct_sp_shock.R` (thresholds, state), `R/mod_3_01_sp.R` (flyout), `R/fct_policy_sim.R` (`has_sp_change`, spec validation).
- Depends on: P2-0b; the Step 2 poverty line `hs$pov_line` (rule 15).
- Done when: never-fires and always-fires invariants hold for each metric; preview equals run; basis risk scored against the same loss events; poverty-based types show "not available" without a line; spec fields in signature and export.
- Tests: hand-calculated metric thresholds; scope truth table with the new trigger types; determinism.

### P2-8: Layered geographic and predicted-welfare targeting (M) ◐

**Status (14 October 2026):** engine, UI, tests and BFA benchmark done; not yet checked in a browser; not committed. The scope is `static rule AND payout scope AND geography AND predicted welfare`; with both layers off the plan has no `dyn` part and the Phase 1 code path runs unchanged.

- Spec fields (shock only, in the SP list so in the run signature and export): `dyn_geo` (`all` | `worst_share`), `dyn_geo_pct`, `dyn_welfare` (logical), `dyn_welfare_pct`, `dyn_incl_error_pct`, `dyn_excl_error_pct`, validated in `.sp_dyn_problem()`. Worst share is refused with "pay all targeted households" (that scope pays non-triggered cells by design). Read these fields with `[[` in code: `$dyn_geo` partial-matches `dyn_geo_pct`.
- Geography (`.sp_dyn_scope()`): in each year the triggered cells are ranked by severity = depth beyond the trigger limit in units of the cell's own historical sd (`.sp_trigger_severity()`, sd from `.sp_trigger_cell_sd()`, zero when no spread), ties by cell key; a cell is paid while the population before it is below the share of the year's triggered population (a tiny share still pays the worst cell; 100 % keeps all). Hazard-based, not an oracle.
- Predicted welfare (oracle, labelled in the UI): a household is selected when its central predicted level that year is at or below the weighted `dyn_welfare_pct` quantile of the households' historical mean levels (fixed per run, no poverty line needed). A selected household is dropped with probability `excl`, an unselected one added with probability `incl` (defaults 20 % / 20 %). One uniform per (survey row, simulation year) from `wise_seed(seed, "policy", "sp-dynamic", year)`.
- **Deviation from rule 10 (flag for review):** the draw is keyed by (survey row, year), not member, so every member and the historical arm share a household's luck (common random numbers). Keying by member would need the member name threaded through all three consumers; the decomposition consumer does not have it. Results are still independent of row order and chunk size.
- `sp_shock_plan(seed =)`, `sp_shock_preview(seed)` and `apply_policy_delta_to_baseline(seed =)` (new argument, passed from `fct_step3_async.R`) carry the run seed. `.sp_trigger_limits()` and `.sp_key_chr()` were extracted from `.sp_trigger_exceeds()` (same behaviour).
- UI (`mod_3_01_sp.R`, Trigger flyout): "Within triggered locations" select with share slider, welfare checkbox with cutoff and two error sliders; layers appear in the trigger summary. The loss-share budget and the cap use the final scope automatically (`state$in_scope`).
- Tests (`test-sp-shock-correction.R`): layers off or non-binding bit-identical to Phase 1; hand-calculated ranking and cumulative-share rule for four shares; monotone in share; row-order permutation, seed and repeat determinism; welfare cutoff, 0 % / 100 % error limits; cost lower with layers and preview equals run; spec checks. Suites run green: `sp-shock-correction`, `fct_sp_shock`, `sp-reach`, `fct-policy-metric-decompose`, `mod_3_06`, `mod_3_10`, `determinism`, `fct_step3_async`, `ui-migration-step3`, `bench-step3`, `policy-sim-compare`.
- Benchmark (BFA, ols, one SSP and one period, 3 repetitions, cold; before = 8da2e76): correction loop (`policy_seconds`, median) `shock_sp` 7.98 -> 7.49 s (disabled) and 7.51 -> 7.35 s (enabled), `targeted_sp` 3.70 -> 3.68 and 3.64 -> 3.50 (noise); output fingerprints identical for all four cases; peak parent RSS not higher. With both layers on (`shock_sp_dyn`, new harness fixture): 8.42 s disabled, 8.41 s enabled (about +0.9 s, +12 % over `shock_sp`). IRN not run.
- Not covered: the "reach card reflects the layers" item reads through the sidebar preview (cost, activation, basis risk use the layered scope); the static reach text is unchanged. No Diagnostics wording for the layers. Not checked in a browser: layout of the new controls.

`static rule AND dynamic rule` with geography (triggered locations, or worst-affected share) and optionally predicted welfare at trigger (oracle, labelled, larger default errors). Replaces the single dropdown in shock mode. Reuses the trigger and payout-scope code; does not need P2-5.

- Files: `R/fct_sp_shock.R`, `R/fct_policy_sim.R`, `R/mod_3_01_sp.R`.
- Done when: with the dynamic rule off, results equal Phase 1 exactly; the oracle option carries its label; the reach card reflects the layers.
- Tests: truth table for the layers; regression to Phase 1; preview parity.

### P2-7: Anticipatory design (M to L, after P2-0c and P2-6)

Forecast skill (POD, FAR) with seeded draws keyed by (location, year, member); effectiveness multiplier on welfare, not on cost; delivery-delay multiplier reviving `timeliness_weeks`; breakeven `m` display; in-app statement of the static-model limitation.

- Files: `R/fct_sp_shock.R`, `R/mod_3_01_sp.R`, `R/mod_3_08_diagnostics.R`.
- Done when: POD = 1, FAR = 0, `m` = 1 reproduces the ex-post design exactly; false-alarm cost shows in cost outputs; the breakeven `m` is reproducible; draws independent of row order and chunk size.
- Tests: the identity above; monotonic in skill; determinism; breakeven hand example.

### P2-9: Tests, benchmark and docs for Phase 2 (S to M)

Extend the invariants list in the plan; hot-path benchmark of the correction loop before and after Phase 2; update `AGENTS.md` (file entry, pipeline description) and roxygen; update the plan's decisions.

## 3. Phase 3: design-space tools (not scheduled)

| Task | Work | Size |
|---|---|---|
| P3-1 presets | Named presets (protective, preventive, promotive, custom) as data; combined "cash plus" scenarios use the existing levers. | S |
| P3-2 design sweep | Grid of shock designs reusing baseline pipelines and prepared channels; cost vs poverty-reduction frontier; bounded grid size, progress reporting, resource limits. Shock arm only at first. | M |
| P3-3 reserve dynamics | Carry-over and depletion need an ordered-year rule; years are draws today, so decide the rule first. | M |
| P3-4 amount rules | Gap-filling and tiered amounts; loss-proportional only as an oracle or not at all. | S to M |
| P3-5 further budget rules | Moved from P2-6 (decision 7): annual expected loss (AAL) sizing and premium-equivalent cost; budget proportional to the rise in the number of poor or the poverty gap (needs P2-5 and rule 15); envelope per activation. Reuse the P2-6 ceiling and cap code. | M |

## 4. Dependencies

```
P2-1 --> P2-2 --> P2-3 --> P2-4 (needs spike P2-0a) --> P2-6 --> P2-5 (needs spike P2-0b) --> P2-8 --> P2-7 (needs spike P2-0c, and P2-6) --> P2-9
```

The order is a recommendation, not a hard chain. Hard dependencies: P2-4 needs P2-0a; P2-5 needs P2-0b; P2-7 needs P2-0c and P2-6; P2-9 needs all. P2-6 and P2-8 do not need P2-5. Phase 3 follows Phase 2 outputs (P2-2, P2-6); P3-5 also needs P2-5.

## 5. Risks

| Risk | Where | Mitigation |
|---|---|---|
| Decomposition split changes snapshots and exports | P2-3 | Numbers unchanged by construction; identity test before and after; update exports once |
| Weather-path change alters existing results or memory | P2-4 | Spike P2-0a first; bit-identical gate; fallback option 2 |
| Hot-loop slowdown (already +95 % in the loop) | P2-3, P2-5 to P2-7 | Benchmark each; cheaper key index and one `rowsum` for annual cost are ready options |
| Anticipatory multiplier read as a calibrated number | P2-7 | Default 1, breakeven display, explicit in-app limitation text |
| Impact-based triggers unobservable in practice | P2-5 | Location or national level by default; household level only as a labelled oracle |
| Cost-effectiveness read as covering all policy levers | P2-2 | Decision 5: cash transfers only, with the scope statement shown in the tab |
| Two poverty lines give inconsistent numbers | P2-2, P2-5 | Decision 10 (rule 15): Step 2 line for run-defining features, Results line for display, each printed |
| Stale cached arm after re-running Step 3 | any new cached Step 3 output | Key shared caches on the policy run signature (regression test in `test-policy-sim-compare-agg-cache.R`) |

## 6. Open questions

None blocking. Still to settle when reached: the unit-cost question for non-cash levers (out of scope until the policy team supplies unit costs) and the P0-9 literature pass (deferred).
