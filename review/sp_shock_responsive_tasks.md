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

- **P2-0a binned-variable cost (gates P2-4).** Measure the extra memory and worker payload of a companion `<var>_cont` column per binned variable per member on BFA and IRN, and confirm the prepared-weather and weather-disk caches store pre-binning values. Output: go or no-go for option 1.
- **P2-0b aggregate loss metrics (gates P2-5).** Prototype location and national modelled loss, poverty rate and number of poor per (member, year) from the central predictions, using the Step 2 poverty line (rule 15); check behaviour of the historical thresholds and the record-length rule on real BFA exposure. Output: which metric is defined where, and the trigger input units.
- **P2-0c forecast skill (gates P2-7).** Decide POD/FAR input versus noisy hazard copy, the seeded key (location, year, member), and how a false alarm is costed. Output: written choice.

### P2-1: Response check for the trigger variable (S to M) ☑

**Status (13 October 2026):** built, unit-tested and checked in the browser by the user (the decile table is collapsed under the hint). `sp_response_check()` in `R/fct_sp_shock.R` reuses `sp_shock_reference()` and the `level / reference - 1` loss of `sp_shock_pipeline_rows()` (rule 7; log outcomes back-transformed before averaging). Weighted deciles over household-years (equal values stay together, so a coarse variable gives fewer bins); direction `above` / `below` / `either` / `none` from the mean loss in the outer fifth of the deciles against the middle (an end counts from 1 percentage point deeper; both ends count when the shallower is at least half the deeper). The hint names wet / dry (spi, spei, precipitation names) or hot / cold (temperature names) years. Unusable variables return `problem`, not an error. The Trigger settings flyout (`output$trigger_response`) shows the hint, the decile table and, for a valid spec, the preview's false-negative and false-positive rates. Tests: five blocks in `test-fct_sp_shock.R` (hand-calculated weighted bins, log back-transform, one- and two-sided direction, unusable variables, determinism and row order, module render). Thresholds (20 % ends, 1 point, one half) are judgement calls, flagged for review.

Mean modelled loss by decile of the chosen variable from the Step 2 historical predictions (rule 7 loss; decision 12); show where losses concentrate, suggest high, low or either end, and show the loss-event share's basis risk beside it. Pure function in `fct_sp_shock.R`, shown in the Trigger settings flyout.

- Files: `R/fct_sp_shock.R`, `R/mod_3_01_sp.R`.
- Done when: on BFA the hint says "wet years, high `spei6`" (not `t`); an unusable variable shows a message, not an error; deterministic.
- Tests: hand-calculated deciles on a small panel (level scale, weights); direction suggestion on a one-sided and a two-sided response; module render.

### P2-2: Shock outputs and the cost-effectiveness tab (L)

Climate-adjusted cost; coverage of the shock-affected poor; adequacy; poverty effect per cost in average and 1-in-20 years (closes P0-5); and a dedicated cost-effectiveness tab that works for regular and shock cash transfers. Most inputs already exist in `out$shock$rows` and `$cells` and in `sp_effectiveness()`. Decisions 2 to 5 and 9 apply (net people lifted, lead metric by outcome type, "not applicable" for near-zero or wrong-signed effects, dedicated tab, one seeded targeting draw, Results poverty line printed in the tab).

- Scope statement shown in the tab: cash transfers only; only the modelled transfer, administration, welfare and poverty effects count; delivery of other levers, targeting surveys and financing are excluded; other levers have no cost-effectiveness figure. Admin share shown beside every figure.
- Files: new tab module (`R/mod_3_10_cost_effectiveness.R` or the next free number, wired in `R/mod_3_scenario.R`), `R/fct_sp_shock.R`, `R/fct_sp_effectiveness.R`, `R/mod_3_08_diagnostics.R` (move or link the existing effectiveness table), export registration in `R/fct_export.R`. Design and arithmetic: `review/step3_cost_effectiveness_plan.md` (its section 8 now points to plan section 3.8).
- Done when: metrics match hand calculations on a small panel; the "not reported for a shock program" notice is gone; exports carry the new tables; poverty line absent shows "Not available"; the tab is empty-state safe when the SP lever is off.
- Tests: extend `test-fct_sp_shock.R` and `test-fct_sp_effectiveness.R`; export test; a module render test; `app_server` wiring test.

### P2-3: Separate decomposition channel (L)

Split `delta_sp_shock` out of Main into its own channel in the compact stats, level channels, summaries and exports. Numbers must not change, only grouping.

- Files: `R/fct_policy_metric_decompose.R`, `R/fct_decomposition_summary.R`, `R/mod_3_09_decomposition.R` (`.compact_decomp_channels`, `:109`), `R/fct_export.R`, and the three consumers of the correction.
- Done when: totals, Main plus the new channel, equal the old Main for every method and metric; the channel identity checks pass; labels and exports updated; regular runs show no new channel.
- Tests: identity before and after on a shock fixture; snapshot updates for exports; the three consumers agree. Hot-path benchmark.
- Risk: the largest uncertainty in Phase 2 (touches snapshots and exports).

### P2-4: Binned weather triggers (M, after P2-0a)

Option 1 from plan section 3.2 if P2-0a passes: companion continuous column kept before `.apply_binning()` in the historical slice and every member, added to `weather_columns`, offered through `.sp_trigger_variables()` with its unit.

- Files: `R/fct_get_weather.R` (three call sites), `R/fct_simulations.R` (`weather_columns`), `R/fct_sp_shock.R`.
- Done when: existing model matrices and outputs are bit-identical (equivalence gate); a binned variable can be used for weather-level and return-period triggers; the companion is never a model term.
- Tests: companion excluded from model matrices; bit-identical outputs for existing runs; trigger thresholds on a binned variable equal those on its continuous source. Benchmark per the optimization guidelines.
- Fallback: option 2 ("bin at or beyond k") if P2-0a is no-go.

### P2-6: Budgets linked to need (M)

Decision 7 scope: budget as a share of modelled loss (rule 7) with a ceiling, and the cap with pro-rata scale-down. Report coefficient of variation of annual cost, share of loss offset, share of affected poor not covered, and budget exhaustion (share of years the cap binds) beside each rule.

- Files: `R/fct_sp_shock.R`, `R/fct_policy_sim.R`, `R/mod_3_01_sp.R`, `R/mod_3_08_diagnostics.R`.
- Done when: budget identity holds (total = net transfers + admin); an envelope is never exceeded; the ceiling binds as set; exhaustion share reported; new spec fields in the run signature and export.
- Tests: identities, ceiling, monotonicity in the share, determinism; preview parity. Hot-path benchmark.

### P2-5: Impact-based triggers (M to L, after P2-0b)

Trigger on modelled loss, poverty rate or number of poor at location and national level. New `trigger_type` values, thresholds on the historical distribution of the metric (same record rule), applied unchanged to SSP members. Household-level modelled quantities only as a labelled oracle.

- Files: `R/fct_sp_shock.R` (thresholds, state), `R/mod_3_01_sp.R` (flyout), `R/fct_policy_sim.R` (`has_sp_change`, spec validation).
- Depends on: P2-0b; the Step 2 poverty line `hs$pov_line` (rule 15).
- Done when: never-fires and always-fires invariants hold for each metric; preview equals run; basis risk scored against the same loss events; poverty-based types show "not available" without a line; spec fields in signature and export.
- Tests: hand-calculated metric thresholds; scope truth table with the new trigger types; determinism.

### P2-8: Layered geographic and predicted-welfare targeting (M)

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
