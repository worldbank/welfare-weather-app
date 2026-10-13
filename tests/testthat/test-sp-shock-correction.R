# ============================================================================ #
# tests/testthat/test-sp-shock-correction.R                                    #
# P1-6: the shock-responsive transfer inside the annual correction. One hook,  #
# three consumers (Results pipelines, the production adapter, the metric       #
# attribution); an always-on trigger equals the regular program; a trigger     #
# that never fires equals no program.                                          #
# ============================================================================ #

library(testthat)

shk_so <- function(transform) list(name = "welfare", transform = transform, type = "numeric")

shk_spec <- function(...) {
  utils::modifyList(
    list(sp_type = "shock", targeting = "exante_poor", targeting_threshold = 50,
         inclusion_error_pct = 0, exclusion_error_pct = 0,
         transfer_amount_usd = 40, payments_per_activation = 3L,
         trigger_type = "weather", trigger_variable = "temp",
         trigger_direction = "above", trigger_value = 20,
         trigger_return_period_years = NA_real_, payout_scope = "local",
         national_k_pct = 0, budget_mode = "transfer_first"),
    list(...)
  )
}

# Twelve households, two simulation years; exposure temperatures spread over
# 0..45 so a threshold of 20 splits rows.
shk_fixture <- function(transform = "none", spec = shk_spec()) {
  n <- 12L
  base <- data.frame(welfare = seq(1, 6, length.out = n), hhsize = rep(1:4, 3),
    temp = seq(10, 21), rain = rep(1:3, 4), x = rep(0:1, 6),
    loc_id = rep(1:2, 6), int_month = rep(c(1L, 6L), 6),
    code = "BFA", year = "2018", survname = "wave-a", weight = seq_len(n))
  train <- expand.grid(temp = c(10, 30), rain = c(1, 3), x = 0:1)
  y <- 2 + train$temp / 10 + train$rain / 20 + train$x / 5
  train$welfare <- if (transform == "log") exp(y) else y
  lhs <- if (transform == "log") "log(welfare)" else "welfare"
  model <- list(engine = "fixest", fit3 = lm(as.formula(paste(lhs, "~ temp + rain + x")), train),
    train_data = train, weather_terms = c("temp", "rain"))
  ids <- rep(c(3L, 1L, 12L, 5L, 3L), 2)
  weather <- base[ids, c("code", "year", "survname", "loc_id", "int_month", "temp", "rain")]
  years <- rep(c(2030L, 2031L), each = 5)
  weather$timestamp <- as.Date(sprintf("%d-%02d-01", years, weather$int_month))
  weather$temp <- seq(0, 45, length.out = length(ids))
  weather$rain <- rep(c(0, 2, 8, 1, 10), 2)
  pipe <- list(y_point = seq(1, 2, length.out = length(ids)), svy_row_id = ids,
    sim_year = years, weight = base$weight[ids], id_vec = ids)
  pipe$weather_exposure <- c(list(status = "ok", table = weather,
    row_index = seq_along(ids), prediction_row_id = seq_along(ids)),
    pipe[c("svy_row_id", "sim_year", "weight", "id_vec")])

  shock_policy <- apply_policy_to_svy(base, sp = spec, seed = 5L)
  regular_policy <- apply_policy_to_svy(base, sp = utils::modifyList(spec, list(
    sp_type = "regular", transfer_n_payments = spec$payments_per_activation)), seed = 5L)
  context <- function(policy) .build_decomposition_context(base, policy, model,
    shk_so(transform), skip_coef = TRUE, run_identity = "annual-run")
  list(base = base, model = model, pipeline = pipe, spec = spec, transform = transform,
    shock_policy = shock_policy, regular_policy = regular_policy,
    ctx_shock = context(shock_policy), ctx_regular = context(regular_policy),
    ctx_none = context(base))
}

shk_prepared <- function(fx, shock = TRUE) {
  plan <- if (shock) {
    sp_shock_plan(fx$spec, fx$pipeline$weather_exposure$table,
      fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh")
  }
  .prepare_policy_annual_channels(fx$ctx_shock, "annual-run", shock = plan)
}

shk_channels <- function(fx, prepared) {
  .policy_annual_channels(fx$pipeline, prepared, "annual-run")
}

test_that("an always-on trigger gives the regular program's correction", {
  for (trans in c("none", "log")) {
    fx <- shk_fixture(trans, shk_spec(trigger_value = -1e9))
    shock <- shk_channels(fx, shk_prepared(fx))
    regular <- shk_channels(fx, .prepare_policy_annual_channels(fx$ctx_regular, "annual-run"))
    expect_gt(sum(regular$delta_sp != 0), 0L)
    for (field in c("delta_sp", "delta_main", "delta_res1", "delta_res2", "delta_total")) {
      expect_equal(shock[[field]], regular[[field]], info = paste(trans, field))
    }
    expect_equal(shock$delta_sp_shock, shock$delta_sp)
  }
})

test_that("a trigger that never fires equals no program", {
  for (trans in c("none", "log")) {
    fx <- shk_fixture(trans, shk_spec(trigger_value = 1e9))
    shock <- shk_channels(fx, shk_prepared(fx))
    none <- shk_channels(fx, .prepare_policy_annual_channels(fx$ctx_none, "annual-run"))
    expect_true(all(shock$delta_sp == 0))
    expect_identical(shock$delta_total, none$delta_total)
  }
})

test_that("a regular run has no shock vector and the block is unchanged", {
  fx <- shk_fixture("log")
  prepared <- .prepare_policy_annual_channels(fx$ctx_regular, "annual-run")
  expect_null(prepared$shock)
  ch <- shk_channels(fx, prepared)
  expect_false("delta_sp_shock" %in% names(ch))
})

test_that("only exceeding rows of eligible households are paid", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20))
  ch <- shk_channels(fx, shk_prepared(fx))
  tbl <- fx$pipeline$weather_exposure$table
  eligible <- fx$shock_policy[[SP_ELIGIBLE_COL]][fx$pipeline$svy_row_id]
  paid <- ch$delta_sp_shock > 0
  expect_identical(paid, tbl$temp >= 20 & eligible)
  expect_true(any(paid) && !all(paid))
  # Same amount as the regular program on the paid rows
  regular <- shk_channels(fx, .prepare_policy_annual_channels(fx$ctx_regular, "annual-run"))
  expect_equal(ch$delta_sp_shock[paid], regular$delta_sp[paid])
})

test_that("a lower threshold never reduces payments", {
  cost <- vapply(c(40, 30, 20, 10, 0), function(th) {
    fx <- shk_fixture("none", shk_spec(trigger_value = th))
    sum(shk_channels(fx, shk_prepared(fx))$delta_sp_shock)
  }, numeric(1))
  expect_true(all(diff(cost) >= 0))
  expect_gt(cost[5], cost[1])
})

test_that("the national gate pays only in years where enough people are exposed", {
  # Year 1 temperatures are 0..~18 and year 2 ~22..45, so a threshold of 20
  # exposes few rows in year 1 and most in year 2.
  fx <- shk_fixture("none", shk_spec(trigger_value = 20, payout_scope = "national_all",
    national_k_pct = 60))
  ch <- shk_channels(fx, shk_prepared(fx))
  year <- fx$pipeline$sim_year
  eligible <- fx$shock_policy[[SP_ELIGIBLE_COL]][fx$pipeline$svy_row_id]
  expect_true(all(ch$delta_sp_shock[year == 2030] == 0))
  expect_identical(ch$delta_sp_shock[year == 2031] > 0, eligible[year == 2031])
})

test_that("results, adapter and attribution agree for the same shock run", {
  for (trans in c("none", "log")) {
    fx <- shk_fixture(trans)
    p <- fx$pipeline
    hist <- list(pipeline = p, so = shk_so(trans),
      shared_context = list(train_aug = data.frame(id = 1:12, .resid = 0)), residuals = "none")
    run <- function(chunk_size) apply_policy_delta_to_baseline(
      fx$base, fx$shock_policy, fx$model, shk_so(trans), hist,
      decomp_context = fx$ctx_shock, run_identity = "annual-run", chunk_size = chunk_size,
      sp = fx$spec, analysis_unit = "hh"
    )
    out <- run(3L)
    # Chunk size does not change the correction
    expect_identical(out$hist_sim$pipeline$y_point, run(100L)$hist_sim$pipeline$y_point)
    ch <- shk_channels(fx, out$annual_channels)
    expect_equal(out$hist_sim$pipeline$y_point, p$y_point + ch$delta_total)
    expect_gt(sum(ch$delta_sp_shock), 0)
    # The attribution path stops unless its reconstruction equals the run
    policy_hist <- hist
    policy_hist$pipeline <- out$hist_sim$pipeline
    result <- .policy_metric_decomposition(hist, policy_hist, list(), list(),
      out$annual_channels, "mean", requested_residuals = "none")
    expect_identical(result$status, "ok", info = result$reason)
    # P2-3: the shock-responsive transfer is its own contribution, so the old
    # combined Main is main + sp_shock and the total is unchanged
    a <- result$annual
    expect_gt(sum(abs(a$sp_shock)), 0)
    expect_equal(a$total, a$main + a$sp_shock + a$repositioning + a$interaction)
    expect_equal(a$total, a$policy - a$baseline)
  }
})

test_that("a shock plan that does not fit the run is refused", {
  fx <- shk_fixture("none")
  plan <- sp_shock_plan(fx$spec, fx$pipeline$weather_exposure$table,
    fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh")
  plan$per_household <- plan$per_household[-1]
  expect_error(.prepare_policy_annual_channels(fx$ctx_shock, "annual-run", shock = plan),
    "does not match")
})

# Run outputs (P1-7) ----

shk_run <- function(fx, scenarios = list()) {
  p <- fx$pipeline
  hist <- list(pipeline = p, so = shk_so(fx$transform),
    shared_context = list(train_aug = data.frame(id = 1:12, .resid = 0)), residuals = "none")
  apply_policy_delta_to_baseline(
    fx$base, fx$shock_policy, fx$model, shk_so(fx$transform), hist, scenarios,
    decomp_context = fx$ctx_shock, run_identity = "annual-run",
    sp = fx$spec, analysis_unit = "hh"
  )
}

test_that("a regular run has no shock outputs", {
  fx <- shk_fixture("none")
  hist <- list(pipeline = fx$pipeline)
  out <- apply_policy_delta_to_baseline(fx$base, fx$regular_policy, fx$model,
    shk_so("none"), hist, decomp_context = fx$ctx_regular, run_identity = "annual-run")
  expect_null(out$shock)
  expect_false("shock" %in% names(out))
})

test_that("annual cost and activation match a hand calculation", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20))
  out <- shk_run(fx, list(future = list(pipelines = list(m1 = fx$pipeline),
    year_range = c("2030", "2031"))))
  rows <- out$shock$rows
  expect_setequal(rows$scenario, c("Historical", "future"))
  expect_equal(nrow(rows), 4L)
  p <- fx$pipeline
  tbl <- p$weather_exposure$table
  eligible <- fx$shock_policy[[SP_ELIGIBLE_COL]][p$svy_row_id]
  daily <- 40 * 3 / 365 / fx$base$hhsize[p$svy_row_id] # amount x payments / 365 / household size
  paid <- tbl$temp >= 20 & eligible
  for (yr in c(2030L, 2031L)) {
    idx <- p$sim_year == yr
    expected <- sum(daily[idx & paid] * p$weight[idx & paid]) * 365
    row <- rows[rows$scenario == "future" & rows$sim_year == yr, ]
    expect_equal(row$total_cost, expected)
    expect_equal(row$admin_cost, 0)
    expect_identical(row$activated, any(idx & paid))
  }
  expect_true(any(rows$activated))
  s <- out$shock$summary
  expect_equal(s$activation_freq[s$scenario == "future"], mean(rows$activated[rows$scenario == "future"]))
  expect_equal(s$mean_total_cost[s$scenario == "future"],
    mean(rows$total_cost[rows$scenario == "future"]))
  expect_true(is.na(s$cost_1_in_20[1])) # two years cannot support a 1-in-20 cost
})

test_that("the shock transfer is its own decomposition channel and totals do not change (P2-3)", {
  for (trans in c("none", "log")) {
    fx <- shk_fixture(trans)
    out <- shk_run(fx, list(future = list(pipelines = list(m1 = fx$pipeline),
      year_range = c("2030", "2031"))))
    ch <- shk_channels(fx, out$annual_channels)
    p <- fx$pipeline
    tbl <- out$decomp_scenarios$channel_summary
    mean_of <- function(rows, channel) rows[[paste0("sum_", channel)]] / rows[[paste0("weight_", channel)]]
    hist <- tbl[tbl$scenario != "future", ]
    # Per year, compared with the household-level channels of the same run
    for (yr in hist$sim_year) {
      idx <- p$sim_year == yr
      w <- p$weight[idx]
      wm <- function(x) sum(x[idx] * w) / sum(w)
      row <- hist[hist$sim_year == yr, ]
      shock <- wm(ch$delta_sp_shock)
      expect_gt(abs(shock), 0)
      # Static main + shock is the old combined Main; the total is unchanged
      expect_equal(mean_of(row, "delta_sp_shock"), shock)
      expect_equal(mean_of(row, "delta_main") + mean_of(row, "delta_sp_shock"), wm(ch$delta_main))
      expect_equal(mean_of(row, "delta_sp") + mean_of(row, "delta_sp_shock"), wm(ch$delta_sp))
      expect_equal(mean_of(row, "delta_total"), wm(ch$delta_total))
      # Outcome-unit channels add up to the level total
      expect_equal(
        mean_of(row, "lvl_main") + mean_of(row, "lvl_sp_shock") + mean_of(row, "lvl_res1") +
          mean_of(row, "lvl_res2"),
        mean_of(row, "lvl_total")
      )
      expect_gt(abs(mean_of(row, "lvl_sp_shock")), 0)
    }
  }
})

test_that("a regular run shows no shock channel and keeps the old numbers (P2-3)", {
  fx <- shk_fixture("none")
  hist <- list(pipeline = fx$pipeline, so = shk_so("none"),
    shared_context = list(train_aug = data.frame(id = 1:12, .resid = 0)), residuals = "none")
  out <- apply_policy_delta_to_baseline(fx$base, fx$regular_policy, fx$model,
    shk_so("none"), hist, decomp_context = fx$ctx_regular, run_identity = "annual-run")
  tbl <- out$decomp_scenarios$channel_summary
  expect_equal(tbl$sum_delta_sp_shock, rep(0, nrow(tbl)))
  expect_equal(tbl$sum_lvl_sp_shock, rep(0, nrow(tbl)))
  # Metric-aware states: the shock state equals the main state and adds nothing
  policy_hist <- hist
  policy_hist$pipeline <- out$hist_sim$pipeline
  res <- .policy_metric_decomposition(hist, policy_hist, list(), list(),
    out$annual_channels, "mean", requested_residuals = "none")
  expect_identical(res$status, "ok", info = res$reason)
  expect_equal(res$annual$after_static_main, res$annual$after_main)
  expect_equal(res$annual$sp_shock, rep(0, nrow(res$annual)))
  expect_equal(res$annual$total, res$annual$main + res$annual$repositioning + res$annual$interaction)
  # The headline summary has no shock row
  rows <- data.frame(delta_main = 1, delta_sp_shock = 0, delta_res1 = 0.1,
    delta_res2 = 0.2, delta_total = 1.3, weight = 1)
  expect_false("Shock-responsive transfer" %in%
    .decomposition_outcome_summary(rows, list(transform = "none"))$channel)
  rows$delta_main <- 0.6
  rows$delta_sp_shock <- 0.4
  s <- .decomposition_outcome_summary(rows, list(transform = "none"))
  expect_identical(s$channel, c("Main effect", "Shock-responsive transfer", "Repositioning",
    "Interaction", "Total"))
  expect_equal(s$value, c(0.6, 0.4, 0.1, 0.2, 1.3))
})

test_that("reach counts per survey row match a brute-force calculation (P2-2)", {
  spec <- shk_spec(trigger_value = 20, loss_event_pct = 10)
  fx <- shk_fixture("none", spec)
  out <- shk_run(fx, list(future = list(pipelines = list(m1 = fx$pipeline),
    year_range = c("2030", "2031"))))
  p <- fx$pipeline
  tbl <- p$weather_exposure$table
  plan <- sp_shock_plan(spec, tbl, fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh")
  transfer <- sp_shock_pipeline_transfer(plan, p, p$weather_exposure)
  ref <- tapply(p$y_point, p$svy_row_id, mean)[as.character(p$svy_row_id)]
  event <- (p$y_point / ref - 1) <= -0.10
  expected <- cbind(
    n_event = event, n_covered = event & transfer > 0,
    loss = ifelse(event, pmax(ref - p$y_point, 0), 0), paid = ifelse(event, transfer, 0)
  )
  expected <- rowsum(expected, p$svy_row_id)
  n <- nrow(fx$shock_policy)
  dense <- matrix(0, n, 4L, dimnames = list(NULL, colnames(expected)))
  dense[as.integer(rownames(expected)), ] <- expected
  expect_true(any(dense[, "n_event"] > 0))
  expect_setequal(names(out$shock$reach), c("Historical", "future"))
  expect_equal(out$shock$reach$Historical, dense)
  expect_equal(out$shock$reach$future, dense)
  expect_equal(out$shock$reach_loss_event_pct, 10)
  # The reach feeds coverage and adequacy at any display line
  r <- sp_shock_reach(out$shock$reach, fx$base, poverty_line = 1e6)
  expect_equal(nrow(r), 2L)
  expect_true(all(r$coverage >= 0 & r$coverage <= 1))
})

test_that("basis risk and leakage follow the loss events", {
  for (scope in c("local", "national_all")) {
    spec <- shk_spec(trigger_value = 20, loss_event_pct = 10, payout_scope = scope,
      national_k_pct = 20)
    fx <- shk_fixture("none", spec)
    out <- shk_run(fx)
    rows <- out$shock$rows
    p <- fx$pipeline
    tbl <- p$weather_exposure$table
    # Every prediction row is its own cell here; the reference is the household's
    # own mean over the historical rows
    ref <- tapply(p$y_point, p$svy_row_id, mean)[as.character(p$svy_row_id)]
    event <- (p$y_point / ref - 1) <= -0.10
    fired <- tbl$temp >= 20
    pop <- p$weight
    transfer <- sp_shock_pipeline_transfer(
      sp_shock_plan(spec, tbl, fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh"),
      p, p$weather_exposure
    )
    for (yr in c(2030L, 2031L)) {
      i <- p$sim_year == yr
      row <- rows[rows$sim_year == yr, ]
      expect_equal(row$pop_tp, sum(pop[i & fired & event]))
      expect_equal(row$pop_fp, sum(pop[i & fired & !event]))
      expect_equal(row$pop_fn, sum(pop[i & !fired & event]))
      expect_equal(row$pop_tn, sum(pop[i & !fired & !event]))
      expect_equal(row$spend_total, sum((transfer * pop)[i]))
      expect_equal(row$spend_no_event, sum((transfer * pop)[i & !event]))
    }
    s <- out$shock$summary
    expect_equal(s$false_positive_rate, sum(rows$pop_fp) / sum(rows$pop_fp + rows$pop_tn))
    expect_equal(s$false_negative_rate, sum(rows$pop_fn) / sum(rows$pop_fn + rows$pop_tp))
    expect_equal(s$leakage_share, sum(rows$spend_no_event) / sum(rows$spend_total))
    expect_gte(s$leakage_share, 0)
    expect_lte(s$leakage_share, 1)
  }
})

test_that("the modelled-loss reference averages levels, not logs", {
  p <- list(y_point = log(c(100, 400, 50, 50)), svy_row_id = c(1L, 1L, 2L, 2L))
  ref <- sp_shock_reference(p, 3L, is_log = TRUE)
  expect_equal(ref, c(250, 50, NA))
  expect_equal(sp_shock_reference(list(y_point = c(1, 3, NA), svy_row_id = c(1L, 1L, 2L)), 2L, FALSE),
    c(2, NA))
})

test_that("the summary needs 20 member-years for a 1-in-20 cost and pools rates", {
  rows <- data.frame(
    scenario = rep(c("A", "B"), c(25, 3)), member = "m", sim_year = c(1:25, 1:3),
    activated = c(rep(c(TRUE, FALSE), length.out = 25), TRUE, TRUE, FALSE),
    exposed_share = 0.1, transfer_cost = c(1:25, 1:3) * 10, admin_cost = c(1:25, 1:3) * 2,
    total_cost = c(1:25, 1:3) * 12,
    pop_tp = 1, pop_fp = 3, pop_fn = 2, pop_tn = 6, spend_total = 10, spend_no_event = 4
  )
  s <- sp_shock_summary(rows)
  expect_identical(s$scenario, c("A", "B"))
  expect_equal(s$cost_1_in_20[1], unname(stats::quantile((1:25) * 12, 0.95)))
  expect_true(is.na(s$cost_1_in_20[2]))
  expect_equal(s$false_positive_rate, c(3 / 9, 3 / 9))
  expect_equal(s$false_negative_rate, c(2 / 3, 2 / 3))
  expect_equal(s$leakage_share, c(0.4, 0.4))
  expect_equal(s$mean_transfer_cost[2], 20)
})

test_that("the run reports which survey rows were paid in the historical years", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20))
  out <- shk_run(fx)
  p <- fx$pipeline
  tbl <- p$weather_exposure$table
  eligible <- fx$shock_policy[[SP_ELIGIBLE_COL]][p$svy_row_id]
  expect_identical(out$shock$paid_historical,
    seq_len(12) %in% p$svy_row_id[tbl$temp >= 20 & eligible])
  expect_equal(out$shock$per_household, as.numeric(
    .sp_dynamic_per_household(fx$shock_policy[[SP_ELIGIBLE_COL]], fx$spec, fx$shock_policy, "hh")))
})

# Invariants (P1-10) ----

test_that("the preview equals the run for the historical arm", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20, admin_cost_pct = 20))
  out <- shk_run(fx)
  hist_rows <- out$shock$rows[out$shock$rows$scenario == "Historical", ]
  p <- fx$pipeline
  preview <- sp_shock_preview(fx$spec, list(pipeline = p, svy = fx$base), fx$base, "hh", seed = 5L)
  expect_equal(preview$annual$total_cost, hist_rows$total_cost)
  expect_equal(preview$annual$transfer_cost, hist_rows$transfer_cost)
  expect_equal(preview$annual$admin_cost, hist_rows$admin_cost)
  expect_identical(preview$annual$activated, hist_rows$activated)
  # Budget identity: total cost is net transfers plus administration, with
  # administration 20 percent of the total
  expect_equal(hist_rows$total_cost, hist_rows$transfer_cost + hist_rows$admin_cost)
  expect_equal(hist_rows$admin_cost, 0.2 * hist_rows$total_cost)
})

test_that("results do not depend on prediction row order", {
  fx <- shk_fixture("log", shk_spec(trigger_value = 20))
  prepared <- shk_prepared(fx)
  p <- fx$pipeline
  ch <- shk_channels(fx, prepared)
  ord <- c(10:1)
  perm <- p
  for (field in c("y_point", "svy_row_id", "sim_year", "weight", "id_vec")) perm[[field]] <- p[[field]][ord]
  ex <- p$weather_exposure
  perm$weather_exposure <- ex
  perm$weather_exposure$row_index <- ex$row_index[ord]
  perm$weather_exposure$prediction_row_id <- ex$prediction_row_id[ord]
  for (field in c("svy_row_id", "sim_year", "weight", "id_vec")) perm$weather_exposure[[field]] <- p[[field]][ord]
  ch_perm <- .policy_annual_channels(perm, prepared, "annual-run")
  expect_equal(ch_perm$delta_total, ch$delta_total[ord])
  expect_equal(ch_perm$delta_sp_shock, ch$delta_sp_shock[ord])
  # Annual cost and activation do not change either
  plan <- prepared$shock
  state <- sp_shock_pipeline_state(plan, p, ex)
  state_perm <- sp_shock_pipeline_state(plan, perm, perm$weather_exposure)
  a <- sp_shock_annual(plan$per_household[p$svy_row_id] * state$in_scope, p,
    plan$cost_frame, state, "hh", "PPP", 0)
  b <- sp_shock_annual(plan$per_household[perm$svy_row_id] * state_perm$in_scope, perm,
    plan$cost_frame, state_perm, "hh", "PPP", 0)
  expect_equal(a, b)
})

test_that("a warmer member fires a fixed historical trigger at least as often", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20))
  plan <- shk_prepared(fx)$shock
  p <- fx$pipeline
  warm <- p$weather_exposure
  warm$table$temp <- warm$table$temp + 5
  base_n <- sum(sp_shock_pipeline_state(plan, p, p$weather_exposure)$exceeds)
  warm_n <- sum(sp_shock_pipeline_state(plan, p, warm)$exceeds)
  expect_gte(warm_n, base_n)
  expect_gt(warm_n, base_n)
  # The same holds for a low-tail variable under cooling
  below <- shk_fixture("none", shk_spec(trigger_value = 20, trigger_direction = "below"))
  plan_b <- shk_prepared(below)$shock
  cool <- p$weather_exposure
  cool$table$temp <- cool$table$temp - 5
  expect_gte(
    sum(sp_shock_pipeline_state(plan_b, p, cool)$exceeds),
    sum(sp_shock_pipeline_state(plan_b, p, p$weather_exposure)$exceeds)
  )
})

test_that("a larger amount never reduces welfare", {
  gain <- vapply(c(10, 40, 80), function(amount) {
    fx <- shk_fixture("log", shk_spec(trigger_value = 20, transfer_amount_usd = amount))
    sum(shk_channels(fx, shk_prepared(fx))$delta_total)
  }, numeric(1))
  expect_true(all(diff(gain) > 0))
})

# Scoring the finished run again (loss-event share) ----

test_that("the loss-event share re-scores a finished run without a new run", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20, loss_event_pct = 10))
  out <- shk_run(fx)
  shock <- out$shock
  expect_true(is.data.frame(shock$cells))
  expect_setequal(names(shock$cells), c("scenario", "member", "sim_year", "pop", "rel", "fired", "spend"))
  # The same share reproduces the run
  same <- sp_shock_rescore(shock, 10)
  expect_equal(same$rows, shock$rows)
  expect_equal(same$summary, shock$summary)
  # A higher share has fewer loss events: false negatives cannot rise, spending
  # in cells without an event cannot fall
  strict <- sp_shock_rescore(shock, 40)
  expect_lte(sum(strict$rows$pop_fn + strict$rows$pop_tp), sum(shock$rows$pop_fn + shock$rows$pop_tp))
  expect_gte(sum(strict$rows$spend_no_event), sum(shock$rows$spend_no_event))
  # Costs and activation are untouched; only the scoring columns move
  expect_equal(strict$rows$total_cost, shock$rows$total_cost)
  expect_identical(strict$rows$activated, shock$rows$activated)
  # Equals a run made with that share
  fx40 <- shk_fixture("none", shk_spec(trigger_value = 20, loss_event_pct = 40))
  expect_equal(strict$rows, shk_run(fx40)$shock$rows)
  expect_equal(strict$summary, shk_run(fx40)$shock$summary)
  # Invalid shares and runs without cells are returned unchanged
  expect_identical(sp_shock_rescore(shock, NA), shock)
  expect_identical(sp_shock_rescore(shock, -3), shock)
  no_cells <- shock
  no_cells$cells <- NULL
  expect_identical(sp_shock_rescore(no_cells, 20), no_cells)
})
