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
| 10 | Run-defining features (impact triggers on poverty) use the Step 2 line fixed per run and show "not available" without one; display metrics follow the Results line. |
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
| P2-8 layered targeting | ◐ | Not checked in a browser. Welfare-error draws are keyed by (survey row, year), shared by all members: a deviation from rule 10, accepted (the member name is missing in the decomposition consumer). Predicted welfare is an oracle, labelled. |
| P2-5 impact triggers | ◐ | Not checked in a browser. No response check or record-length text for impact triggers. A threshold at the historical minimum never fires ("no adverse tail"). |
| P2-7 anticipatory design | ◐ | Not checked in a browser. Forecast draws keyed by (cell, year), shared by all members (same deviation). Breakeven `m` is exact for mean welfare, approximate for poverty metrics. `m` is an assumption; the static model cannot show the mechanism. |

BFA hot-path figures (correction loop, ols, one SSP and one period, median of 3, uncertainty disabled): `shock_sp` 7.5 to 8.0 s (Phase 1 baseline 7.0 s; `targeted_sp` 3.6 s); `shock_sp_dyn` 8.4 s; `shock_sp_impact` 8.2 s; `shock_sp_antic` 9.5 s. `shock_sp` output fingerprints are identical across P2-8, P2-5 and P2-7. IRN was never run for Phase 2.

### P2-9: Tests, benchmark and docs for Phase 2 (S to M) ☐

- Extend the invariants list in the plan (never fires equals baseline, always fires equals the static program, monotone in threshold, amount, skill and share, preview equals run, determinism under row order) with the P2-1 to P2-8 cases; check each has a test.
- Hot-path benchmark of the correction loop before Phase 2 (`7545ab3`) and now, BFA, plus IRN if the data allows, with the fixtures above; record the figures in the tracking file.
- Update `AGENTS.md` (file entry for `fct_sp_shock.R` and `fct_sp_effectiveness.R`, pipeline text for dynamic scope, impact triggers and timing, seed argument), roxygen, and the plan's sections 1 to 3 (move built items out, keep rules).
- Done when: the plan reads as "built" for Phase 2 with the remaining open points in section 4 below.

## 3. Owed by the user

Record in `review/REVIEW-2026-10-06-tracking.md`:

- Browser checks for every `◐` row above, in one Step 0 to 3 pass with a shock program.
- BFA before and after headline numbers for P0-8 (synthetic check: realised weighted inclusion 15.8 % / exclusion 12.2 % before, 15.0 % / 10.0 % after, for 15 % / 10 % sliders).
- BFA and IRN Step 3 benchmark figures with the shock fixture.
- The BFA LCU re-run (R2-BUG-04 / CR-BUG-02) entry for the Decision log.
- **P0-9** (admin presets and error defaults) stays deferred: literature pass on cost-transfer ratios and PMT error rates, then a preset dropdown. The 10 % / 10 % defaults and a zero admin share stay; the cost-effectiveness tab prints the admin share beside every figure.

## 4. Open points

- Draws shared across members (P2-8, P2-7): decide whether to thread the member name through the three consumers of the correction (`.policy_annual_channels`, the production adapter, the metric attribution) to restore rule 10.
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
| Two poverty lines give inconsistent numbers | Decision 10: Step 2 line for run-defining features, Results line for display, each printed. |
| Stale cached arm after re-running Step 3 | Key shared caches on the policy run signature (regression test in `test-policy-sim-compare-agg-cache.R`). |
