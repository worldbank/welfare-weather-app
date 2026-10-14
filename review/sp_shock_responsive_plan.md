# Social protection: shock-responsive scenarios, design and remaining plan

Scope: the Step 3 social protection (SP) module (`mod_3_01_sp`). Phase 0 (design options for the regular program) and Phase 1 (shock-responsive MVP) are built, verified and committed on `dev` (495ae85 to 7545ab3). This document keeps the design rules the code must follow and the plan for Phases 2 and 3. Work items are in `review/sp_shock_responsive_tasks.md`; what each finished task did is in the git history.

Updated 2026-10-14 (Phase 2 built). Written for the WISE-APP maintainers and the policy team who decide what to build next.

Evidence note: statements about the code come from reading the files cited. Literature claims are limited to what a search confirmed (Sources). Anything unverified is marked "to verify".

## 1. Where things stand

**Built (Phase 0).** Admin cost as a share of total cost (default 0), per-household or per-person amounts, distance-weighted targeting errors (concentration 0 reproduces the old uniform flips exactly), weighted error counts (P0-8: errors hit the weighted share the sliders describe; unweighted or constant-weight surveys are bit-identical), a "Cost and targeting effectiveness" table in Diagnostics (cost per person, admin share, realised errors, coverage, leakage, adequacy), and the SP panel flyouts ("Targeting details", "Payment settings").

**Built (Phase 1, closed 8 October 2026).**

- Shock mode runs end to end: `R/fct_sp_shock.R` holds thresholds, trigger state, payout scope, the run-level plan, the per-row dynamic transfer, run outputs and the sidebar preview. One shared hook (`.policy_annual_channel_block(sp_dynamic =)`) feeds all three consumers of the correction (Results, Decomposition, attribution), so they agree.
- Triggers: weather level or return period (1-in-N), on a continuous weather variable, direction above, below or either end, per survey location and interview month, with thresholds fixed on the historical record. The flyout shows the unit of a transformed variable.
- Payout scope: local, national gate with triggered locations, national gate with all targeted households.
- Response: a fixed amount per household (or person) per activation, N payments per activation, admin markup. Shock mode does not use the budget mode and has no envelope budget.
- Outputs: activation frequency, annual cost distribution (mean, median, 1-in-20, transfers and admin split), basis risk at location level (false positives and negatives against modelled loss events, spending in locations without a loss event). The loss-event share is a scoring input only: Diagnostics re-scores the finished run live and a change does not mark Step 3 stale.
- The dynamic effect is folded into Main (`delta_sp`, `delta_main`, `delta_total`) and kept as its own vector `delta_sp_shock`, so the Phase 2 channel split is a presentation change.
- Sidebar order: Trigger (own section, shock only), Amount with Payment settings beneath it (the amount basis, per household or per person, is shown there), Targeting.

**Owed outside the build** (tracker `review/REVIEW-2026-10-06-tracking.md`): BFA before and after headline numbers for P0-8; the BFA and IRN Step 3 benchmark figures with the shock fixture (harness: `dev/bench_step3_helpers.R`, fixture `shock_sp`); the BFA LCU re-run entry.

## 2. Design rules that still bind the code

These come from the Phase 1 decisions and are the contract for Phase 2.

1. **Static program stays static.** The regular program writes `.wiseapp_sp_transfer`. Shock mode writes only the static eligibility `.wiseapp_sp_eligible` and no transfer; the transfer is paid per prediction row inside the annual correction. Never write both.
2. **Trigger rules.** The threshold is defined on the historical record and applied unchanged to SSP members, so a fixed 1-in-10 trigger fires more often under warming (the headline result for finance planners). A 1-in-N trigger needs N finite historical years in the cell (the Step 2 rule); show the years behind it. For direction "either end" the 1-in-N chance is shared over two 1-in-2N tails and needs 2N years. The trigger reads the model's transformed value, so its unit follows the weather transformation. Return-period triggers give identical activations under any per-location rescaling; only a weather-level threshold depends on the unit.
3. **Trigger unit.** Survey location (`loc_id`), evaluated per (survey round, location, interview month, simulation year). It is not an admin boundary; label it "survey location".
4. **Two trigger behaviours.** A common weather value fires more where the variable is typically more extreme. A return-period trigger gives every location the same activation probability. No location-specific weather thresholds.
5. **Activation rule vs payout scope** are separate questions, set by one control with three choices (local; national gate, pay triggered locations; national gate, pay all targeted households). The national gate fires when the survey-weighted exposed share is above 0 and at least `k`. "Any location fires, pay everyone" is not offered.
6. **Ex-ante poor** is defined on the whole survey frame (national line), then restricted by scope. Preview and run use the same definition.
7. **Loss definition.** Modelled weather loss of a household in year t is its level-scale central prediction minus its own historical mean level, with no residual draws, weighted by survey weights. Back-transform before averaging (averaging logs shifts results by about 0.45 points). A location has a loss event when its mean loss is at or below minus the user-set share of welfare. With a weather-only model, household and location basis risk coincide; they diverge once the model has weather-covariate interactions. Every loss-based output (basis risk, response check, budgets, adequacy) uses this one definition.
8. **RIF repositioning does not respond to the dynamic transfer.** This approximation is stated in code and help. The OLS path is exact.
9. **Preview equals run.** The sidebar card, Diagnostics and exports use the same functions on the same rows.
10. **Reproducibility.** Every random draw comes from a `wise_seed()` stream keyed by its unit (survey row for targeting errors, cell for forecasts) and the simulation year, never by row position, chunk or clock. Draws are shared across climate members and the historical arm on purpose (common random numbers): the same household-year or cell-year has the same luck in every scenario, so scenario differences reflect the weather and the design, and the correction consumers need no member name. Each draw is correctly distributed in every member; sharing only raises the variance of pooled estimates, which the app does not report. Revisit if Monte Carlo intervals or member-year tail statistics of these draws are ever reported, or if members run on different household sets. Every new spec field enters the run signature (the whole SP list is hashed) and the export config; UI-only toggles use the `_toggle` suffix so they stay out of the export. Each task that changes numbers carries a determinism test.
11. **Currency.** New amounts and costs read `sp$currency` and use `outcome_level_scale()` and `.policy_sp_transfer()`; the stored transfer stays on the 2021 PPP scale.
12. **Hot path.** The annual correction loop is benchmarked before a change is accepted (`review/optimization_guidelines.md`). Phase 1 measured +3.4 s (+95 %) on BFA for the correction loop (7.0 s vs 3.6 s); trigger state is 1.3 s of that and annual cost and basis-risk rows 1.8 s. Cheaper options if it matters: a one-time key index in place of the keyed join, and one `rowsum` for annual cost.
13. **Vertical vs horizontal expansion is not modelled.** The survey has no beneficiary flag, so the simulated program is an idealised new program with its own targeting. Do not claim expansion of a named program. Revisit if a data source gains a beneficiary flag, or add a labelled synthetic registry (poorest x% with a coverage parameter).
14. **Limits of the static annual model.** No poverty traps, asset scarring, price effects, local multipliers or crowd-out. Any multiplier is a labelled assumption. State this in the app documentation.
15. **Poverty lines.** Anything that fixes run behaviour (impact-based triggers on poverty, budgets linked to the number of poor) uses the Step 2 line (`hs$pov_line`), fixed per run, and shows "not available" without one. A line the user can edit after the run must not change activations or costs, or preview would stop equalling run. Display metrics (cost-effectiveness, Results) follow the Results poverty line input and print the line they used.

Code comments and tests cite Phase 1 decision numbers that no longer exist as a list. The substance is in rule 5 ("decision 10"), rule 7 ("decision 6", the loss-event share) and the Response bullet in section 1 ("decision 12", budget mode not consulted in shock mode); other numbers are not mapped and the comments stay as they are.

## 3. Phase 2: impact-based, anticipatory, richer targeting

**Built (14 October 2026), tasks P2-1 to P2-8** (commit history: `git log --grep "P2-"`). The subsections below are the design record; where the build differs from them:

- 3.1 impact triggers: metrics are national, GAUL admin 1/2 or survey location (months pooled); direction is always "above"; poverty below national level needs 30 sampled households per unit-year; a threshold at the historical minimum never fires. The metric is read from the pipeline's central predictions and is an exposure column.
- 3.2 binned variables: option 1 (continuous companion `<var>_cont`) was built.
- 3.4 budgets: a share of modelled loss and one annual cap on total cost (including administration), no separate per-cell ceiling.
- 3.5 anticipatory: forecast skill is POD and FAR; a false alarm pays cash at full cost with no multiplier; the delay is weeks x percent lost per week; the breakeven `m` is on the transferred-value basis. Forecast draws are keyed by (cell, simulation year), shared by all members.
- 3.7 layered targeting: geography is a worst-affected share of the triggered population ranked by depth beyond the trigger in historical standard deviations; predicted welfare is an oracle with seeded inclusion and exclusion errors, also keyed by (survey row, year) and shared by all members.

Rule 10 now states that these two draws are shared across members by design; a test pins it (`common random numbers` in `test-sp-shock-correction.R`).

Rough effort was 4 to 6 weeks, from reading the code, not prototyping.

**Build order (decided 8 October 2026):** P2-1 response check and P2-2 outputs with the cost-effectiveness tab (no hot-path change); P2-3 decomposition split while the numbers are stable; P2-4 binned triggers after a spike; P2-6 budgets; P2-5 impact-based triggers; P2-8 layered targeting; P2-7 anticipatory last (most assumptions). P2-9 closes the phase. P2-6 comes before P2-5 because share-of-loss budgets and the cap need only the household loss of rule 7, which Phase 1 already has; P2-5 is the larger, more uncertain task and should not block them.

### 3.1 Impact-based triggers

Triggers on modelled loss, poverty rate or number of poor, at location and national level. Real triggers fire on observable indices at a geographic unit, so household-level modelled quantities are offered only as a labelled oracle benchmark.

- Thresholds are set on the historical distribution of the chosen metric and applied unchanged to SSP members, with the same record-length rule.
- Poverty-based triggers use the Step 2 poverty line (rule 15) and show "not available" without one.
- Basis risk is scored against the same loss events (rule 7).

### 3.2 Binned weather variables as triggers

Excluded in Phase 1 because a binned variable reaches the exposure table as a category label (`.sp_trigger_variables()` keeps numeric columns only). The continuous value is lost in `get_weather()` where `.apply_binning()` overwrites the column (`fct_get_weather.R:1491` historical, `:1937` and `:1969` futures). Only the historical slice keeps a pre-binning copy, as the descriptive `continuous_weather` attribute.

- **Option 1 (preferred): keep the continuous value.** Before `.apply_binning()`, copy each binned variable to a companion column (for example `<var>_cont`) in the historical slice and every member, and add those columns to `weather_columns` in `run_sim_pipeline()`. Triggers then treat them like any continuous variable, so thresholds, record-length rule and direction handling need no change, and the trigger reads a physical hazard value. Work: three call sites, the exposure column list, the trigger-variable list (offer the companion with its unit), and tests that the companion is excluded from model matrices and existing outputs are bit-identical. Measure first: one extra numeric column per binned variable per member in the weather payload and compact exposure, and mirai worker payload size. It touches the weather path, so the equivalence gate applies. Confirm the prepared-weather and weather-disk caches store pre-binning values.
- **Option 2 (fallback): "bin at or beyond k".** The bin order comes from the factor levels (`.bin_level_order()`), so no weather-path change. Coarse; no return-period input (no 1-in-N on a discrete scale), so report the implied historical activation frequency per cell; show the bin edges and unit.
- Start from option 1 if the memory and equivalence checks pass; ship option 2 first only if binned triggers are needed before the weather-path change can be benchmarked.

### 3.3 Response check for the trigger variable

From the Step 2 historical predictions, show mean modelled loss (rule 7: level scale, weighted, no residual draws) by decile of the chosen variable over household-years, where losses concentrate, and a suggestion of high, low or either end, with the loss-event share's basis risk beside it. Reason: a weather variable has no built-in adverse tail. On BFA the model's welfare loss was driven by `spei6` with wet years adverse (correlation -0.95, against 0.08 for `t`); a 1-in-10 temperature trigger missed 98.6 % of loss events, a high-`spei6` trigger missed 40 % with under 1 % false positives. Phase 1 ships "either end" but no automatic hint.

### 3.4 Budgets linked to need

Phase 1 pays a fixed amount per activation. Phase 2 adds two rules, each with a user-set ceiling so a catastrophic draw does not give an absurd bill:

- **Share of modelled loss:** budget for a triggered location is `s` times the aggregate modelled loss of its households, split by the amount rule.
- **Cap with pro-rata scale-down:** an annual cap on total cost; when activations would exceed it, payments scale down pro rata. Budget exhaustion (share of years the cap binds) is reported.
- Show next to each rule the measurable trade-offs: coefficient of variation of annual cost, share of modelled loss offset, and share of shock-affected poor not covered.

Moved to Phase 3 (P3-5): annual expected loss (AAL) sizing and premium-equivalent cost; budget proportional to the modelled rise in the number of poor or the poverty gap (needs P2-5 and rule 15); envelope per activation. Reserve dynamics (carry-over, depletion) also wait for Phase 3 because simulation years are draws, not an ordered sequence.

### 3.5 Anticipatory design

Phase 1 is ex post only. The model predicts annual-mean welfare, so timing alone is invisible: a payment before a flood and the same payment after it land in the same year. Timing matters only through three ingredients, each a labelled assumption:

1. **Forecast skill.** The trigger fires on a noisy copy of the hazard, or directly on probability of detection (POD) and false-alarm rate (FAR), with seeded draws keyed by (location, year, member). It produces what anticipatory programs trade off: false-alarm cost against missed events. Skill typically falls with lead time.
2. **Effectiveness multiplier `m`** on the transferred amount, default 1, applied to the welfare effect but not to cost. Show the breakeven `m` at which an anticipatory design matches an ex-post design on the chosen outcome; do not ask users to pick a number the model cannot calibrate. A WFP Bangladesh evaluation supports the direction (better outcomes for cash received before a flood) but not a transferable numeric `m`.
3. **Delivery delay** for ex-post designs: a multiplier below 1 for weeks of delay. This revives the dead `timeliness_weeks` field.

The app must state that a static annual model cannot capture the mechanism behind most anticipatory benefits (avoided asset sales, scarring).

### 3.6 Separate decomposition channel

Scheduled right after the response check and the missing outputs, before binned triggers and budgets. Split the dynamic SP effect out of Main so the "does the transfer offset climate losses" story is visible. It touches `.compact_decomp_channels` (`mod_3_09_decomposition.R:109`), the level channels, decomposition summaries and exports. Do not fold it into Interaction; that channel stays a model-derived coefficient-times-weather term. Snapshots and exports change once. Because `delta_sp_shock` already travels separately, the numbers do not change, only their grouping.

### 3.7 Layered targeting

`eligible = static household rule AND dynamic rule(year, member)`. The static rule is the existing dropdown; the dynamic rule adds geography (triggered locations, or the worst-affected share of locations) and optionally predicted welfare at trigger. Predicted welfare uses the model's own prediction, which is an oracle: pair it with larger default error rates and label it an upper bound. Layered rules replace the single targeting dropdown in shock mode. It reuses the trigger and payout-scope code but not the impact metrics, so it does not wait for P2-5.

### 3.8 Outputs still missing

| Output | Definition | Task |
|---|---|---|
| Climate-adjusted cost | Cost under SSP members vs historical with the same fixed trigger (the data is in `out$shock`; needs a presentation) | P2-2 |
| Coverage of the shock-affected poor | Share of poor households with a modelled loss that received a transfer | P2-2 |
| Adequacy | Transfer as a share of modelled loss or of the poverty gap | P2-2 |
| Poverty effect per cost | Net change in the number of poor, labelled as such (gross movements are not built), per unit cost, in average and bad years. Leads with cost per person lifted for poverty-rate metrics and welfare gain per $1 otherwise; "not applicable" for near-zero or wrong-signed effects. Design in `review/step3_cost_effectiveness_plan.md` | P2-2 |
| Cost-effectiveness for shock programs | Suppressed today because its annual-cost basis assumes a transfer paid every year | P2-2 |
| Budget exhaustion | Share of years where a cap binds | P2-6 |

Decisions (8 October 2026) for the cost-effectiveness work: a dedicated Step 3 tab; cash transfers only, and only the modelled transfer, administration, welfare and poverty effects, with the tab saying so (the other levers have no unit costs and are out of scope); one seeded targeting draw; the Results poverty line input (rule 15); the administration share shown beside every figure.

## 4. Phase 3: design-space tools

Rough effort 3 to 4 weeks.

- **Presets** (protective, preventive, promotive, custom) that set parameter groups at once. They are data, not engine logic. A transformative design is a combined scenario with the infrastructure, labour, digital and education levers (`mod_3_02` to `mod_3_05`), not a new SP engine feature.
- **Design sweep** with a cost vs poverty-reduction frontier. If the dynamic layer is additive and sits outside the RIF channels, a grid of designs reuses baseline pipelines and prepared channels and recomputes only the cheap layer. The static program is not additive in the RIF path, so the sweep applies to the shock arm only at first. Bound the grid size and report progress.
- **Reserve dynamics** and optional gap-filling and tiered amounts (a gap-filling amount tops each recipient up toward a welfare line; a useful adequacy benchmark). Loss-proportional amounts are not observable in practice: oracle benchmark or skip.

## 5. Open items outside the build order

- **Admin cost presets** (P0-9) are deferred. They need a literature pass (cost-transfer ratio sources below); the same pass should check whether 10 % / 10 % is a defensible default for targeting errors. Until then the admin share defaults to 0 and is shown beside every cost-effectiveness figure.
- **Spatially correlated errors:** exclusion in a poorly connected region is correlated across households. Not planned.
- **Duration of support** after an activation (promotive designs) is covered by payments per activation; richer schedules are Phase 3 at most.
- **Neighbour spillover** (paying adjacent locations) is not planned; it could be added as a scope option.

## Testing invariants (kept as tests; extend for each Phase 2 feature)

Never triggers equals baseline; always triggers matches the static program (OLS exact, RIF within the documented approximation); monotonic in threshold and amount; preview parity; determinism under row order and chunk size; budget identities (total = net transfers + admin, envelopes never exceeded); a warming member fires a fixed historical heat trigger at least as often.

Phase 2 additions, all in `test-sp-shock-correction.R` unless noted:

- Every feature at its neutral setting (weather trigger, `dyn_geo = "all"`, welfare layer off, ex post with no delay, fixed budget, no cap) gives a plan with no `dyn`, `timing` or `impact` part and a run bit-identical to Phase 1 ("neutral setting" test).
- Never fires and always fires hold for each impact metric; an always-on impact trigger pays what an always-on weather trigger pays.
- Layers and skill only remove or add payments in the expected direction: a larger worst-affected share, a larger POD or FAR, and a lower exclusion error never reduce the paid set; a 100 % share, or every household added, equals Phase 1.
- POD 100 %, FAR 0 % and `m` = 1 reproduce the ex-post run; `m` and the delay scale welfare only, never cost.
- Draws (predicted-welfare errors, forecasts) are keyed by (survey row or cell, simulation year), so they do not depend on row order, chunk size or repeats, they move with the seed, and the same unit and year get the same draw in every pipeline (common random numbers; the forecast draw is also independent of whether the event happened).
- Impact thresholds: a threshold equal to a cell's historical minimum never fires; rate and number of poor fire in the same years for a return-period trigger.
- All Phase 2 features together ("Phase 2 features together" test): chunk size and repeat invariance, budget identity, cap never exceeded, preview equals run, attribution reconstructs the run.

## Sources

Verified by search:

- Pople et al., "The Importance of Being Early: Anticipatory Cash Transfers for Flood-Affected Households": https://www.csae.ox.ac.uk/node/2132846
- WFP, "Bangladesh anticipatory action impact evaluation": https://www.wfp.org/publications/bangladesh-anticipatory-action-impact-evaluation
- Brown, Ravallion and van de Walle, proxy-means testing in Africa: https://www.nber.org/papers/w22919.pdf and https://www.imf.org/external/pubs/ft/fandd/2017/12/brown.htm
- Caldes, Coady and Maluccio (2006) on cost-transfer ratios: https://www.peiglobal.org/sites/pei/themes/pei/kc_files/Caldés et al. 2006.pdf
- Bastagli et al., cash transfer evidence review: https://www.peiglobal.org/cash-transfers-what-does-evidence-say-rigorous-review-programme-impact-and-role-design-and
- OPM, cost-efficiency of cash in humanitarian programmes: https://opml.co.uk/projects/cash-transfers-humanitarian-programmes-assessing-cost-efficiency
- Shock-responsive social protection building blocks (trigger, financing, delivery): https://openknowledge.worldbank.org/server/api/core/bitstreams/6f3a1a7d-d5d6-4f4f-9d6d-4417b0ff6f69/content

To verify before relying on it: the OPM vertical/horizontal expansion typology citation, numeric cost-transfer ratio ranges, typical PMT error rates for default slider values, and national-index-cover wording for payout scope (c).
