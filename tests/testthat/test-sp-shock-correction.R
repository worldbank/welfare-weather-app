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

# P2-6: budgets linked to need ---------------------------------------------

shk_plan <- function(fx, spec = fx$spec, ...) {
  sp_shock_plan(spec, fx$pipeline$weather_exposure$table,
    fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh",
    hist_pipeline = fx$pipeline, is_log = FALSE, ...)
}

# The "below" trigger fires the 2030 rows, whose predictions sit below the
# household's own mean, so there is a modelled loss to share
shk_loss_spec <- function(...) {
  shk_spec(trigger_direction = "below", trigger_value = 20, budget_rule = "loss_share",
    loss_share_pct = 50, transfer_amount_usd = 0, ...)
}

shk_with_cells <- function(p, row_index) {
  p$weather_exposure$row_index <- row_index
  p
}

test_that("the fixed rule is unchanged and a distant cap leaves it bit-identical", {
  fx <- shk_fixture("none", shk_spec(trigger_value = 20))
  p <- fx$pipeline
  plan <- shk_plan(fx)
  state <- sp_shock_pipeline_state(plan, p, p$weather_exposure)
  phase1 <- plan$per_household[p$svy_row_id] * state$in_scope
  tr <- sp_shock_pipeline_transfer(plan, p, p$weather_exposure)
  expect_identical(as.numeric(tr), phase1)
  expect_null(attr(tr, "cap_binds"))
  far <- sp_shock_pipeline_transfer(shk_plan(fx, utils::modifyList(fx$spec, list(annual_cap = 1e12))),
    p, p$weather_exposure)
  expect_identical(as.numeric(far), phase1)
  expect_false(any(attr(far, "cap_binds")))
})

test_that("loss share pays the share of each household's modelled loss (own cell)", {
  fx <- shk_fixture("none", shk_loss_spec())
  p <- fx$pipeline
  plan <- shk_plan(fx)
  tr <- sp_shock_pipeline_transfer(plan, p, p$weather_exposure)
  state <- sp_shock_pipeline_state(plan, p, p$weather_exposure)
  ref <- tapply(p$y_point, p$svy_row_id, mean)[as.character(p$svy_row_id)]
  paid <- state$in_scope & fx$shock_policy[[SP_ELIGIBLE_COL]][p$svy_row_id]
  expected <- ifelse(paid, 0.5 * pmax(ref - p$y_point, 0), 0)
  expect_gt(sum(expected > 0), 0L)
  expect_equal(as.numeric(tr), as.numeric(expected))
  # No transfer for the households that are not paid, whatever their loss
  expect_true(all(tr[!paid] == 0))
})

test_that("a cell budget is shared by the amount basis (hand calculation)", {
  for (basis in c("per_household", "per_capita")) {
    fx <- shk_fixture("none", shk_loss_spec(amount_basis = basis))
    p <- shk_with_cells(fx$pipeline, c(1, 1, 2, 3, 3, 6, 6, 7, 8, 8))
    plan <- shk_plan(fx)
    tr <- as.numeric(sp_shock_pipeline_transfer(plan, p, p$weather_exposure))
    state <- sp_shock_pipeline_state(plan, p, p$weather_exposure)
    ids <- p$svy_row_id
    loss <- pmax(tapply(p$y_point, ids, mean)[as.character(ids)] - p$y_point, 0)
    scale <- if (basis == "per_household") fx$base$hhsize[ids] else rep(1, length(ids))
    sel <- state$in_scope & fx$shock_policy[[SP_ELIGIBLE_COL]][ids]
    expected <- numeric(length(ids))
    for (cell in unique(p$weather_exposure$row_index)) {
      i <- which(p$weather_exposure$row_index == cell & sel)
      if (!length(i)) next
      a <- 0.5 * sum(p$weight[i] * loss[i]) / sum(p$weight[i] / scale[i])
      expected[i] <- a / scale[i]
    }
    expect_gt(sum(expected > 0), 0L)
    expect_equal(tr, expected, info = basis)
    # The cell budget is spent in full: cost identity on the cell level
    for (cell in unique(p$weather_exposure$row_index)) {
      i <- which(p$weather_exposure$row_index == cell & sel)
      if (!length(i)) next
      expect_equal(sum(p$weight[i] * tr[i] * scale[i] / scale[i]),
        0.5 * sum(p$weight[i] * loss[i]), info = basis)
    }
  }
})

test_that("loss-share cost is proportional to the share, and zero without loss", {
  fx <- shk_fixture("none", shk_loss_spec())
  p <- fx$pipeline
  cost <- function(pct) {
    spec <- utils::modifyList(fx$spec, list(loss_share_pct = pct))
    sum(sp_shock_pipeline_transfer(shk_plan(fx, spec), p, p$weather_exposure) * p$weight)
  }
  expect_equal(cost(50), 2 * cost(25))
  expect_equal(cost(100), 4 * cost(25))
  # A trigger that never fires pays nothing
  never <- utils::modifyList(fx$spec, list(trigger_value = -1e9))
  expect_true(all(sp_shock_pipeline_transfer(shk_plan(fx, never), p, p$weather_exposure) == 0))
})

test_that("the annual cap scales payments pro rata and is never exceeded", {
  for (rule in c("fixed", "loss_share")) {
    base_spec <- if (rule == "fixed") {
      shk_spec(trigger_direction = "below", trigger_value = 20, admin_cost_pct = 10)
    } else {
      shk_loss_spec(admin_cost_pct = 10)
    }
    fx <- shk_fixture("none", base_spec)
    p <- fx$pipeline
    open <- shk_plan(fx)
    tr0 <- sp_shock_pipeline_transfer(open, p, p$weather_exposure)
    a0 <- sp_shock_annual(tr0, p, open$cost_frame, NULL, "hh", "PPP", open$admin_share)
    expect_gt(max(a0$total_cost), 0)
    cap <- 0.5 * max(a0$total_cost)
    spec <- utils::modifyList(base_spec, list(annual_cap = cap))
    plan <- shk_plan(fx, spec)
    tr <- sp_shock_pipeline_transfer(plan, p, p$weather_exposure)
    a <- sp_shock_annual(tr, p, plan$cost_frame, NULL, "hh", "PPP", plan$admin_share)
    expect_true(all(a$total_cost <= cap * (1 + 1e-9)), info = rule)
    binds <- a0$total_cost > cap
    expect_true(any(binds))
    expect_equal(a$total_cost[binds], rep(cap, sum(binds)), info = rule)
    expect_equal(a$total_cost[!binds], a0$total_cost[!binds], info = rule)
    expect_identical(unname(attr(tr, "cap_binds")), unname(binds), info = rule)
    # Pro rata: one factor per capped year
    for (yr in p$sim_year[1]) {
      i <- p$sim_year == yr & tr0 > 0
      expect_equal(length(unique(round(tr[i] / tr0[i], 12))), 1L, info = rule)
    }
    expect_true(all(tr <= tr0 + 1e-15))
  }
})

test_that("run rows report cap exhaustion, cost variability and loss offset", {
  spec <- shk_loss_spec(loss_event_pct = 10)
  fx <- shk_fixture("none", spec)
  open <- shk_run(fx)$shock
  cap <- 0.5 * max(open$rows$total_cost)
  fxc <- shk_fixture("none", utils::modifyList(spec, list(annual_cap = cap)))
  out <- shk_run(fxc)$shock
  s <- out$summary
  expect_equal(s$cap_exhaustion_share, mean(out$rows$total_cost >= cap * (1 - 1e-9)))
  expect_gt(s$cap_exhaustion_share, 0)
  expect_equal(s$cost_cv, sd(out$rows$total_cost) / mean(out$rows$total_cost))
  expect_equal(s$loss_offset_share, sum(out$rows$paid_value) / sum(out$rows$loss_value))
  expect_lte(s$loss_offset_share, 0.5 + 1e-12)
  expect_equal(open$summary$cap_exhaustion_share, 0)
  expect_lt(sum(out$rows$total_cost), sum(open$rows$total_cost))
  # Published amount of one activation: the mean payment of the paid years
  expect_length(out$per_household, nrow(fxc$shock_policy))
  expect_true(any(out$per_household > 0))
  expect_equal(out$paid_historical, out$per_household > 0)
})

test_that("the budget draws no randomness: row order and repeated runs agree", {
  fx <- shk_fixture("none", shk_loss_spec(annual_cap = 2))
  p <- fx$pipeline
  plan <- shk_plan(fx, fx$spec)
  p <- shk_with_cells(p, c(1, 1, 2, 3, 3, 6, 6, 7, 8, 8))
  tr <- as.numeric(sp_shock_pipeline_transfer(plan, p, p$weather_exposure))
  ord <- c(10:1)
  perm <- p
  for (field in c("y_point", "svy_row_id", "sim_year", "weight", "id_vec")) perm[[field]] <- p[[field]][ord]
  perm$weather_exposure$row_index <- p$weather_exposure$row_index[ord]
  for (field in c("svy_row_id", "sim_year", "weight", "id_vec")) perm$weather_exposure[[field]] <- p[[field]][ord]
  tr_perm <- as.numeric(sp_shock_pipeline_transfer(plan, perm, perm$weather_exposure))
  expect_equal(tr_perm, tr[ord])
  expect_identical(tr, as.numeric(sp_shock_pipeline_transfer(plan, p, p$weather_exposure)))
})

test_that("preview equals run for the budget rules", {
  spec <- shk_loss_spec(loss_event_pct = 10)
  fx <- shk_fixture("none", spec)
  cap <- 0.5 * max(shk_run(fx)$shock$rows$total_cost)
  fxc <- shk_fixture("none", utils::modifyList(spec, list(annual_cap = cap)))
  hist <- list(pipeline = fxc$pipeline, so = shk_so("none"))
  pre <- sp_shock_preview(fxc$spec, hist, fxc$base, "hh", seed = 5L,
    exposure = fxc$pipeline$weather_exposure)
  run <- shk_run(fxc)$shock
  hist_rows <- run$rows[run$rows$scenario == "Historical", ]
  expect_equal(pre$mean_cost, mean(hist_rows$total_cost))
  expect_equal(pre$cap_exhaustion_share, run$summary$cap_exhaustion_share)
  expect_equal(pre$cost_cv, run$summary$cost_cv)
  expect_equal(pre$loss_offset_share, run$summary$loss_offset_share)
})

test_that("spec checks cover the budget rule, share and cap", {
  ok <- shk_spec(trigger_value = 20)
  expect_null(.sp_shock_problem(ok))
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(budget_rule = "x"))), "budget rule")
  ls <- utils::modifyList(ok, list(budget_rule = "loss_share", loss_share_pct = 50))
  expect_null(.sp_shock_problem(ls))
  for (bad in list(NA_real_, 0, -5, 101)) {
    expect_match(.sp_shock_problem(utils::modifyList(ls, list(loss_share_pct = bad))), "share")
  }
  expect_null(.sp_shock_problem(utils::modifyList(ok, list(annual_cap = NA_real_))))
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(annual_cap = 0))), "cap")
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(annual_cap = -1))), "cap")
  # A loss-share program spends without an amount; the fixed rule needs one
  expect_true(has_sp_change(utils::modifyList(ls, list(transfer_amount_usd = 0))))
  expect_false(has_sp_change(utils::modifyList(ok, list(transfer_amount_usd = 0))))
})

# P2-8: layered dynamic targeting ------------------------------------------

shk_dyn_state <- function(fx, spec, p = fx$pipeline, seed = 5L) {
  plan <- sp_shock_plan(spec, fx$pipeline$weather_exposure$table,
    fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh",
    hist_pipeline = fx$pipeline, is_log = FALSE, seed = seed)
  list(plan = plan, state = sp_shock_pipeline_state(plan, p, p$weather_exposure))
}

shk_dyn_spec <- function(...) shk_spec(trigger_value = 20, dyn_geo_pct = 100, ...)

test_that("with both layers off the plan has no layer and the scope is Phase 1", {
  fx <- shk_fixture("none", shk_dyn_spec())
  off <- shk_dyn_state(fx, fx$spec)
  expect_null(off$plan$dyn)
  expect_identical(off$state$in_scope, off$state$exceeds)
  # Layers that cannot remove anyone (a 100 % share, every household added) leave
  # the scope bit-identical
  all_geo <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(dyn_geo = "worst_share")))
  expect_identical(all_geo$state$in_scope, off$state$in_scope)
  wide <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(
    dyn_welfare = TRUE, dyn_welfare_pct = 50, dyn_incl_error_pct = 100, dyn_excl_error_pct = 0
  )))
  expect_identical(wide$state$in_scope, off$state$in_scope)
})

test_that("the worst-affected share ranks cells by depth in historical sd units", {
  fx <- shk_fixture("none", shk_dyn_spec(dyn_geo = "worst_share"))
  p <- fx$pipeline
  tbl <- p$weather_exposure$table
  base_in <- shk_dyn_state(fx, shk_dyn_spec())$state$in_scope
  cell <- paste(tbl$loc_id, tbl$int_month)
  sd_cell <- tapply(tbl$temp, cell, stats::sd)[cell]
  sev <- (tbl$temp - 20) / sd_cell
  pop <- p$weight
  for (share in c(10, 40, 70, 100)) {
    spec <- utils::modifyList(fx$spec, list(dyn_geo_pct = share))
    got <- shk_dyn_state(fx, spec)$state$in_scope
    expected <- rep(FALSE, length(pop))
    for (yr in unique(p$sim_year)) {
      i <- which(base_in & p$sim_year == yr)
      i <- i[order(-sev[i], i)]
      before <- cumsum(pop[i]) - pop[i]
      expected[i] <- before < share / 100 * sum(pop[i])
    }
    expect_identical(got, expected, info = share)
  }
  # Monotone in the share, and a tiny share still pays the worst cell of a year
  small <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(dyn_geo_pct = 1)))$state$in_scope
  big <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(dyn_geo_pct = 60)))$state$in_scope
  expect_true(all(big[small]))
  expect_equal(sum(small[p$sim_year == 2031]), 1L)
})

test_that("the layered scope does not depend on prediction row order", {
  fx <- shk_fixture("none", shk_dyn_spec(
    dyn_geo = "worst_share", dyn_geo_pct = 50, dyn_welfare = TRUE,
    dyn_welfare_pct = 60, dyn_incl_error_pct = 30, dyn_excl_error_pct = 30
  ))
  p <- fx$pipeline
  ref <- shk_dyn_state(fx, fx$spec)
  expect_true(any(ref$state$in_scope) && !all(ref$state$in_scope))
  perm <- c(7L, 2L, 10L, 5L, 1L, 9L, 3L, 8L, 6L, 4L)
  q <- p
  for (f in c("y_point", "svy_row_id", "sim_year", "weight", "id_vec")) q[[f]] <- p[[f]][perm]
  q$weather_exposure$row_index <- p$weather_exposure$row_index[perm]
  moved <- sp_shock_pipeline_state(ref$plan, q, q$weather_exposure)
  expect_identical(moved$in_scope, ref$state$in_scope[perm])
  # Same inputs, same seed: identical; another seed moves the draws
  expect_identical(shk_dyn_state(fx, fx$spec)$state$in_scope, ref$state$in_scope)
  u1 <- .sp_dyn_uniform(5L, p$sim_year, p$svy_row_id, 12L)
  u2 <- .sp_dyn_uniform(6L, p$sim_year, p$svy_row_id, 12L)
  expect_false(identical(u1, u2))
  expect_identical(.sp_dyn_uniform(5L, q$sim_year, q$svy_row_id, 12L), u1[perm])
})

test_that("predicted-welfare targeting selects below the cutoff, with errors", {
  spec <- shk_dyn_spec(dyn_welfare = TRUE, dyn_welfare_pct = 50,
    dyn_incl_error_pct = 0, dyn_excl_error_pct = 0)
  fx <- shk_fixture("none", spec)
  p <- fx$pipeline
  base_in <- shk_dyn_state(fx, shk_dyn_spec())$state$in_scope
  got <- shk_dyn_state(fx, fx$spec)
  ref <- tapply(p$y_point, p$svy_row_id, mean)
  cut <- .sp_welfare_quantile(
    as.numeric(ref[as.character(seq_len(12))]), fx$base$weight, 0.5
  )
  expect_equal(got$plan$dyn$welfare_cut, cut)
  expect_identical(got$state$in_scope, base_in & p$y_point <= cut)
  expect_true(any(base_in & !(p$y_point <= cut)))
  # Everyone wrongly excluded, nobody wrongly included
  none <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(dyn_excl_error_pct = 100)))
  expect_identical(none$state$in_scope, base_in & p$y_point > cut & FALSE)
  # Everyone wrongly included on top of the correct ones
  all_in <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(
    dyn_incl_error_pct = 100)))
  expect_identical(all_in$state$in_scope, base_in)
  # More exclusion error never adds payments
  few <- shk_dyn_state(fx, utils::modifyList(fx$spec, list(dyn_excl_error_pct = 40)))
  expect_true(all(got$state$in_scope[few$state$in_scope]))
})

test_that("layers reduce cost, and preview equals run", {
  base_spec <- shk_dyn_spec()
  fx0 <- shk_fixture("none", base_spec)
  spec <- utils::modifyList(base_spec, list(
    dyn_geo = "worst_share", dyn_geo_pct = 50, dyn_welfare = TRUE,
    dyn_welfare_pct = 60, dyn_incl_error_pct = 20, dyn_excl_error_pct = 20
  ))
  fx <- shk_fixture("none", spec)
  p <- fx$pipeline
  hist <- list(pipeline = p, so = shk_so("none"),
    shared_context = list(train_aug = data.frame(id = 1:12, .resid = 0)), residuals = "none")
  run <- apply_policy_delta_to_baseline(
    fx$base, fx$shock_policy, fx$model, shk_so("none"), hist, list(),
    decomp_context = fx$ctx_shock, run_identity = "annual-run",
    sp = spec, analysis_unit = "hh", seed = 5L
  )$shock
  open <- shk_run(fx0)$shock
  expect_lte(sum(run$rows$total_cost), sum(open$rows$total_cost))
  expect_lt(sum(run$rows$total_cost), sum(open$rows$total_cost))
  pre <- sp_shock_preview(spec, list(pipeline = p, svy = fx$base, so = shk_so("none")),
    fx$base, "hh", seed = 5L, exposure = p$weather_exposure)
  hist_rows <- run$rows[run$rows$scenario == "Historical", ]
  expect_equal(pre$annual$total_cost, hist_rows$total_cost)
  expect_identical(pre$annual$activated, hist_rows$activated)
  # The run is repeatable
  again <- apply_policy_delta_to_baseline(
    fx$base, fx$shock_policy, fx$model, shk_so("none"), hist, list(),
    decomp_context = fx$ctx_shock, run_identity = "annual-run",
    sp = spec, analysis_unit = "hh", seed = 5L
  )$shock
  expect_identical(again$rows, run$rows)
})

test_that("spec checks cover the dynamic layers", {
  ok <- shk_dyn_spec()
  expect_null(.sp_shock_problem(ok))
  geo <- utils::modifyList(ok, list(dyn_geo = "worst_share"))
  expect_null(.sp_shock_problem(geo))
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(dyn_geo = "x"))), "within")
  for (bad in list(NA_real_, 0, -1, 101)) {
    expect_match(.sp_shock_problem(utils::modifyList(geo, list(dyn_geo_pct = bad))), "worst-affected")
  }
  expect_match(.sp_shock_problem(utils::modifyList(geo, list(
    payout_scope = "national_all", national_k_pct = 20))), "worst-affected")
  wel <- utils::modifyList(ok, list(dyn_welfare = TRUE, dyn_welfare_pct = 30,
    dyn_incl_error_pct = 10, dyn_excl_error_pct = 10))
  expect_null(.sp_shock_problem(wel))
  for (bad in list(NA_real_, 0, 100)) {
    expect_match(.sp_shock_problem(utils::modifyList(wel, list(dyn_welfare_pct = bad))), "cutoff")
  }
  expect_match(.sp_shock_problem(utils::modifyList(wel, list(dyn_excl_error_pct = 120))), "error rates")
  # Without the layer the welfare fields are not read
  expect_null(.sp_shock_problem(utils::modifyList(ok, list(dyn_welfare = FALSE, dyn_welfare_pct = NA))))
})

# P2-5: impact-based triggers ----------------------------------------------

shk_impact_spec <- function(metric = "loss", level = "national", ...) {
  utils::modifyList(
    shk_spec(trigger_metric = metric, trigger_level = level, trigger_variable = NA_character_,
      trigger_direction = "above", trigger_value = 0),
    list(...)
  )
}

shk_impact_plan <- function(fx, spec, pov_line = 1.5) {
  sp_shock_plan(spec, fx$pipeline$weather_exposure$table,
    fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh",
    hist_pipeline = fx$pipeline, is_log = FALSE, seed = 5L,
    hist_exposure_map = fx$pipeline$weather_exposure, pov_line = pov_line)
}

shk_impact_cfg <- function(metric, level = "national", pov_line = 1.5) {
  list(metric = metric, level = level, pov_line = pov_line, is_log = FALSE)
}

test_that("impact metrics match a hand calculation (national)", {
  fx <- shk_fixture("none", shk_impact_spec())
  p <- fx$pipeline
  ref <- tapply(p$y_point, p$svy_row_id, mean)[as.character(p$svy_row_id)]
  ref_full <- rep(NA_real_, 12)
  ref_full[as.integer(names(tapply(p$y_point, p$svy_row_id, mean)))] <- tapply(p$y_point, p$svy_row_id, mean)
  w <- p$weight
  for (yr in unique(p$sim_year)) {
    i <- p$sim_year == yr
    exp_loss <- -100 * sum(w[i] * (p$y_point[i] / ref[i] - 1)) / sum(w[i])
    exp_pov <- 100 * sum(w[i] * (p$y_point[i] < 1.5)) / sum(w[i])
    exp_cnt <- sum(w[i] * (p$y_point[i] < 1.5))
    cell <- p$weather_exposure$row_index[i]
    col <- function(m) .sp_impact_column(shk_impact_cfg(m), p, p$weather_exposure, ref_full)
    expect_equal(unique(col("loss")[cell]), exp_loss, info = yr)
    expect_equal(unique(col("poverty_rate")[cell]), exp_pov, info = yr)
    expect_equal(unique(col("poor_count")[cell]), exp_cnt, info = yr)
  }
  # Table rows no prediction row refers to stay missing
  q <- p
  q$weather_exposure$table <- rbind(p$weather_exposure$table, p$weather_exposure$table[1, ])
  expect_true(is.na(.sp_impact_column(shk_impact_cfg("loss"), q, q$weather_exposure, ref_full)[11]))
})

test_that("location-level loss is per survey location, poverty needs enough households", {
  fx <- shk_fixture("none", shk_impact_spec(level = "location"))
  p <- fx$pipeline
  tbl <- p$weather_exposure$table
  ref_full <- rep(NA_real_, 12)
  m <- tapply(p$y_point, p$svy_row_id, mean)
  ref_full[as.integer(names(m))] <- m
  col <- .sp_impact_column(shk_impact_cfg("loss", "location"), p, p$weather_exposure, ref_full)
  loc <- paste(tbl$code, tbl$year, tbl$survname, tbl$loc_id)
  for (u in unique(loc)) for (yr in unique(p$sim_year)) {
    i <- loc == u & p$sim_year == yr
    expect_equal(unique(col[i]), -100 * sum(p$weight[i] * (p$y_point[i] / ref_full[p$svy_row_id[i]] - 1)) / sum(p$weight[i]))
  }
  pov <- .sp_impact_column(shk_impact_cfg("poverty_rate", "location"), p, p$weather_exposure, ref_full)
  expect_true(all(is.na(pov)))
  expect_error(shk_impact_plan(fx, shk_impact_spec("poverty_rate", "location")),
    "sampled households", class = "sp_trigger_problem")
})

test_that("a poverty trigger is not available without the Step 2 line", {
  fx <- shk_fixture("none", shk_impact_spec("poverty_rate"))
  expect_error(shk_impact_plan(fx, fx$spec, pov_line = NULL), "poverty line", class = "sp_trigger_problem")
  expect_error(shk_impact_plan(fx, fx$spec, pov_line = NA), "poverty line", class = "sp_trigger_problem")
  expect_equal(shk_impact_plan(fx, shk_impact_spec("loss"), pov_line = NULL)$impact$metric, "loss")
})

test_that("never-fires and always-fires hold for every metric", {
  for (metric in c("loss", "poverty_rate", "poor_count")) {
    fx <- shk_fixture("none", shk_impact_spec(metric, trigger_value = 1e12))
    p <- fx$pipeline
    never <- shk_impact_plan(fx, fx$spec)
    st <- sp_shock_pipeline_state(never, p, p$weather_exposure)
    expect_false(any(st$in_scope), info = metric)
    expect_true(all(sp_shock_pipeline_transfer(never, p, p$weather_exposure, st) == 0), info = metric)
    always <- shk_impact_plan(fx, utils::modifyList(fx$spec, list(trigger_value = -1e12)))
    st <- sp_shock_pipeline_state(always, p, p$weather_exposure)
    expect_true(all(st$in_scope), info = metric)
    # An always-on impact trigger pays what an always-on weather trigger pays
    fw <- shk_fixture("none", shk_spec(trigger_value = -1e9))
    pw <- fw$pipeline
    w_plan <- sp_shock_plan(fw$spec, pw$weather_exposure$table, fw$shock_policy[[SP_ELIGIBLE_COL]],
      fw$shock_policy, "hh")
    expect_equal(
      sp_shock_pipeline_transfer(always, p, p$weather_exposure, st),
      sp_shock_pipeline_transfer(w_plan, pw, pw$weather_exposure), info = metric
    )
  }
})

test_that("a 1-in-2 trigger on national loss fires in the worse year; rate and count agree", {
  fx <- shk_fixture("none", shk_impact_spec(trigger_type = "return_period",
    trigger_return_period_years = 2, trigger_value = NA_real_))
  p <- fx$pipeline
  col <- .sp_impact_column(shk_impact_cfg("loss"), p, p$weather_exposure, {
    r <- rep(NA_real_, 12); m <- tapply(p$y_point, p$svy_row_id, mean); r[as.integer(names(m))] <- m; r
  })
  by_year <- tapply(col[p$weather_exposure$row_index], p$sim_year, unique)
  worse <- names(which.max(by_year))
  plan <- shk_impact_plan(fx, fx$spec)
  expect_identical(plan$impact$n_years, 2L)
  st <- sp_shock_pipeline_state(plan, p, p$weather_exposure)
  expect_identical(st$exceeds, as.character(p$sim_year) == worse)
  rate <- shk_impact_plan(fx, utils::modifyList(fx$spec, list(trigger_metric = "poverty_rate")))
  count <- shk_impact_plan(fx, utils::modifyList(fx$spec, list(trigger_metric = "poor_count")))
  expect_identical(
    sp_shock_pipeline_state(rate, p, p$weather_exposure)$exceeds,
    sp_shock_pipeline_state(count, p, p$weather_exposure)$exceeds
  )
})

test_that("the impact column does not depend on prediction row order", {
  fx <- shk_fixture("none", shk_impact_spec(trigger_value = 0, dyn_geo_pct = 100))
  p <- fx$pipeline
  plan <- shk_impact_plan(fx, fx$spec)
  ref <- sp_shock_pipeline_state(plan, p, p$weather_exposure)
  perm <- c(7L, 2L, 10L, 5L, 1L, 9L, 3L, 8L, 6L, 4L)
  q <- p
  for (f in c("y_point", "svy_row_id", "sim_year", "weight", "id_vec")) q[[f]] <- p[[f]][perm]
  q$weather_exposure$row_index <- p$weather_exposure$row_index[perm]
  expect_identical(sp_shock_pipeline_state(plan, q, q$weather_exposure)$in_scope, ref$in_scope[perm])
  expect_identical(sp_shock_pipeline_state(plan, p, p$weather_exposure)$in_scope, ref$in_scope)
})

test_that("preview equals run for an impact trigger", {
  spec <- shk_impact_spec("loss", trigger_type = "return_period",
    trigger_return_period_years = 2, trigger_value = NA_real_)
  fx <- shk_fixture("none", spec)
  p <- fx$pipeline
  hist <- list(pipeline = p, so = shk_so("none"),
    shared_context = list(train_aug = data.frame(id = 1:12, .resid = 0)), residuals = "none",
    pov_line = 1.5)
  run <- apply_policy_delta_to_baseline(fx$base, fx$shock_policy, fx$model, shk_so("none"),
    hist, list(), decomp_context = fx$ctx_shock, run_identity = "annual-run",
    sp = spec, analysis_unit = "hh", seed = 5L)$shock
  expect_gt(sum(run$rows$activated), 0)
  expect_lt(sum(run$rows$activated), nrow(run$rows))
  pre <- sp_shock_preview(spec, hist, fx$base, "hh", seed = 5L, exposure = p$weather_exposure)
  hist_rows <- run$rows[run$rows$scenario == "Historical", ]
  expect_equal(pre$annual$total_cost, hist_rows$total_cost)
  expect_identical(pre$annual$activated, hist_rows$activated)
  expect_identical(pre$n_record_years, 2L)
  # A poverty trigger without the line returns a message, not an error
  no_line <- sp_shock_preview(utils::modifyList(spec, list(trigger_metric = "poverty_rate")),
    utils::modifyList(hist, list(pov_line = NULL)), fx$base, "hh", seed = 5L,
    exposure = p$weather_exposure)
  expect_match(no_line$problem, "poverty line")
})

test_that("spec checks cover the impact metric and level", {
  ok <- shk_impact_spec()
  expect_null(.sp_shock_problem(ok))
  expect_null(.sp_shock_problem(utils::modifyList(ok, list(trigger_variable = NA_character_, trigger_direction = "either"))))
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(trigger_metric = "x"))), "measures")
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(trigger_level = "x"))), "nationally")
  expect_match(.sp_shock_problem(utils::modifyList(ok, list(trigger_value = NA_real_))), "threshold")
  # The weather trigger still needs its variable
  expect_match(.sp_shock_problem(shk_spec(trigger_variable = NA_character_)), "weather variable")
  expect_true(has_sp_change(ok))
})

test_that("administrative levels read the survey area codes and need them in the data", {
  fx <- shk_fixture("none", shk_impact_spec(level = "admin1"))
  area <- rep(c("A", "B", "A"), 4)
  fx$shock_policy$gaul1_code <- area
  p <- fx$pipeline
  plan <- sp_shock_plan(fx$spec, p$weather_exposure$table, fx$shock_policy[[SP_ELIGIBLE_COL]],
    fx$shock_policy, "hh", hist_pipeline = p, is_log = FALSE, seed = 5L,
    hist_exposure_map = p$weather_exposure, pov_line = 1.5)
  ref <- rep(NA_real_, 12)
  m <- tapply(p$y_point, p$svy_row_id, mean)
  ref[as.integer(names(m))] <- m
  col <- .sp_impact_column(plan$impact, p, p$weather_exposure, ref)
  for (a in c("A", "B")) for (yr in unique(p$sim_year)) {
    i <- area[p$svy_row_id] == a & p$sim_year == yr
    expect_equal(unique(col[p$weather_exposure$row_index[i]]),
      -100 * sum(p$weight[i] * (p$y_point[i] / ref[p$svy_row_id[i]] - 1)) / sum(p$weight[i]))
  }
  # No area column in the run, or too few households for a poverty rate
  bare <- shk_fixture("none", shk_impact_spec(level = "admin1"))
  expect_error(shk_impact_plan(bare, bare$spec), "gaul1_code", class = "sp_trigger_problem")
  expect_error(sp_shock_plan(utils::modifyList(fx$spec, list(trigger_metric = "poverty_rate")),
    p$weather_exposure$table, fx$shock_policy[[SP_ELIGIBLE_COL]], fx$shock_policy, "hh",
    hist_pipeline = p, is_log = FALSE, hist_exposure_map = p$weather_exposure, pov_line = 1.5),
    "sampled households", class = "sp_trigger_problem")
  expect_null(.sp_shock_problem(utils::modifyList(fx$spec, list(trigger_level = "admin2"))))
})


test_that("a threshold at the historical minimum gives a cell no adverse tail", {
  tbl <- data.frame(code = "X", year = "2020", survname = "s", loc_id = 1L, int_month = 1L,
    sim_year = 1:20, .impact_metric = c(rep(0, 18), 5, 8))
  spec <- list(trigger_metric = "poverty_rate", trigger_type = "return_period",
    trigger_return_period_years = 5, trigger_direction = "above")
  # The 1-in-5 level is 0, the historical minimum: it would fire every year
  expect_error(sp_trigger_thresholds(tbl, spec), "no adverse tail", class = "sp_trigger_problem")
  spec$trigger_return_period_years <- 20
  expect_equal(sp_trigger_thresholds(tbl, spec)$table$threshold, 5.15)
  # A continuous metric is unaffected
  tbl$.impact_metric <- seq(0.5, 10, by = 0.5)
  spec$trigger_return_period_years <- 5
  expect_gt(sp_trigger_thresholds(tbl, spec)$table$threshold, 0.5)
})

test_that("the impact level choices follow the survey columns", {
  expect_identical(unname(.sp_impact_level_choices(NULL)), c("national", "admin1", "admin2", "location"))
  svy <- data.frame(loc_id = 1:4, gaul1_code = c("a", "a", "b", "b"), gaul2_code = "x")
  expect_identical(unname(.sp_impact_level_choices(svy)), c("national", "admin1", "location"))
  svy$gaul2_code <- c("x", "y", "z", NA)
  expect_identical(unname(.sp_impact_level_choices(svy)), c("national", "admin1", "admin2", "location"))
  expect_identical(unname(.sp_impact_level_choices(svy["loc_id"])), c("national", "location"))
  expect_identical(unname(.sp_impact_level_choices(svy[0, ])), "national")
})
