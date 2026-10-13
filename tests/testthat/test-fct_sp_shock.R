# ============================================================================ #
# tests/testthat/test-fct_sp_shock.R                                           #
# Shock-responsive SP: spec validation (P1-1) and trigger thresholds (P1-2).   #
# ============================================================================ #

library(testthat)

shock_sp <- function(...) {
  utils::modifyList(
    list(sp_type = "shock", transfer_amount_usd = 30, payments_per_activation = 2L,
         trigger_type = "weather", trigger_variable = "temp",
         trigger_direction = "above", trigger_value = 35,
         trigger_return_period_years = NA_real_, payout_scope = "local",
         national_k_pct = 0),
    list(...)
  )
}

# Exposure: two locations x two interview months, `n` years each; temp is
# 1..n shifted per cell so quantiles are hand-checkable.
shock_exposure <- function(n = 30L) {
  g <- expand.grid(loc_id = c("a", "b"), int_month = c(3L, 9L),
                   sim_year = seq_len(n), stringsAsFactors = FALSE)
  g$code <- "BFA"
  g$year <- 2018L
  g$survname <- "EHCVM"
  g$timestamp <- as.Date("2000-01-01") + g$sim_year
  g$temp <- g$sim_year + ifelse(g$loc_id == "b", 100, 0) + g$int_month / 100
  g$cat <- factor(ifelse(g$sim_year > 10, "hot", "mild"))
  g$.policy_exposure_id <- seq_len(nrow(g))
  g
}

# Spec validation (P1-1) ----

test_that("a complete shock spec is valid and has_sp_change is TRUE", {
  expect_null(.sp_shock_problem(shock_sp()))
  expect_true(has_sp_change(shock_sp()))
  expect_null(.sp_shock_problem(shock_sp(
    trigger_type = "return_period", trigger_return_period_years = 10,
    trigger_value = NA_real_, payout_scope = "national_all", national_k_pct = 25
  )))
})

test_that("shock validation names what is missing", {
  expect_match(.sp_shock_problem(shock_sp(trigger_variable = NA_character_)), "variable")
  expect_match(.sp_shock_problem(shock_sp(trigger_value = NA_real_)), "threshold")
  expect_match(.sp_shock_problem(shock_sp(trigger_direction = "left")), "above, below or at either")
  expect_match(
    .sp_shock_problem(shock_sp(trigger_type = "return_period")), "return period"
  )
  expect_match(.sp_shock_problem(shock_sp(payout_scope = "everyone")), "scope")
  expect_match(
    .sp_shock_problem(shock_sp(payout_scope = "national_all", national_k_pct = 140)),
    "between 0 and 100"
  )
  # The gate share is not consulted for local payouts
  expect_null(.sp_shock_problem(shock_sp(national_k_pct = 140)))
})

test_that("shock mode needs amount, payments and a valid trigger", {
  expect_false(has_sp_change(shock_sp(transfer_amount_usd = 0)))
  expect_false(has_sp_change(shock_sp(payments_per_activation = 0L)))
  expect_false(has_sp_change(shock_sp(trigger_value = NA_real_)))
  # Budget mode is ignored in shock mode (decision 12)
  expect_true(has_sp_change(shock_sp(budget_mode = "budget_first", budget_fixed = 0)))
  # Regular programs are unchanged
  reg <- list(budget_mode = "transfer_first", transfer_amount_usd = 20,
              transfer_n_payments = 6L)
  expect_true(has_sp_change(reg))
  expect_false(has_sp_change(utils::modifyList(reg, list(transfer_amount_usd = 0))))
})

test_that("the SP module spec carries the shock fields with safe defaults", {
  svy <- data.frame(welfare = 1:4, weight = 1)
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_shock", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"),
    hist_sim = shiny::reactiveVal(NULL)
  ), {
    spec <- sp_scenario_spec()
    expect_identical(spec$trigger_type, "weather")
    expect_true(is.na(spec$trigger_variable))
    expect_identical(spec$trigger_direction, "above")
    expect_true(is.na(spec$trigger_value))
    expect_true(is.na(spec$trigger_return_period_years))
    expect_identical(spec$payout_scope, "local")
    expect_equal(spec$national_k_pct, 0)
    expect_equal(spec$payments_per_activation, 1L)
    # Shock mode: a fixed amount per activation, whatever the budget mode was
    session$setInputs(
      sp_type = "shock", budget_mode = "budget_first", transfer_n_payments = 12,
      payments_per_activation = 3, trigger_type = "return_period",
      trigger_variable = "t", trigger_direction = "below",
      trigger_return_period_years = "10", payout_scope = "national_all",
      national_k_pct = 40
    )
    spec <- sp_scenario_spec()
    expect_identical(spec$sp_type, "shock")
    expect_identical(spec$budget_mode, "transfer_first")
    expect_equal(spec$transfer_n_payments, 3)
    expect_equal(spec$payments_per_activation, 3)
    expect_identical(spec$trigger_type, "return_period")
    expect_identical(spec$trigger_direction, "below")
    expect_equal(spec$trigger_return_period_years, 10)
    expect_identical(spec$payout_scope, "national_all")
    expect_equal(spec$national_k_pct, 40)
    expect_null(.sp_shock_problem(spec))
    # A regular program keeps its own budget mode and payments per year
    session$setInputs(sp_type = "regular")
    spec <- sp_scenario_spec()
    expect_identical(spec$budget_mode, "budget_first")
    expect_equal(spec$transfer_n_payments, 12)
  })
})

test_that("the trigger flyout offers continuous weather variables only", {
  svy <- data.frame(welfare = 1:4, weight = 1, t = c(1, 2, 3, 4),
    rain = factor(c("a", "b", "a", "b")))
  hs <- list(
    svy = svy,
    pipeline = list(weather_exposure = list(weather_columns = c("t", "rain", "gone")))
  )
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_trigger", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame(
      name = c("t", "rain"), label = c("Temperature", "Rainfall")
    )),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(hs)
  ), {
    expect_identical(unname(trigger_candidates()), "t")
    expect_identical(names(trigger_candidates()), "Temperature")
    html <- as.character(output$sp_trigger_ui$html)
    expect_match(html, "Trigger settings")
    expect_match(html, "Weather variable")
    expect_match(html, "Payments per activation")
    expect_no_match(html, "Rainfall")
  })
  # No exposure yet: the flyout says so instead of offering nothing
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_trigger_none", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(NULL)
  ), {
    expect_length(trigger_candidates(), 0L)
    expect_match(as.character(output$sp_trigger_ui$html), "No continuous weather variable")
  })
})

test_that("changing any shock field changes the policy signature input", {
  base <- shock_sp()
  variants <- list(
    base, shock_sp(trigger_type = "return_period"), shock_sp(trigger_variable = "rain"),
    shock_sp(trigger_direction = "below"), shock_sp(trigger_value = 36),
    shock_sp(trigger_return_period_years = 10), shock_sp(payout_scope = "national_all"),
    shock_sp(national_k_pct = 10), shock_sp(payments_per_activation = 3L)
  )
  sigs <- vapply(variants, function(sp) rlang::hash(.sig_plain(list(sp = sp))), character(1))
  expect_equal(length(unique(sigs)), length(variants))
})

# Trigger thresholds (P1-2) ----

test_that("only continuous weather columns are trigger variables", {
  expect_identical(.sp_trigger_variables(shock_exposure()), "temp")
  expect_identical(.sp_trigger_variables(NULL), character())
  expect_error(
    sp_trigger_thresholds(shock_exposure(), shock_sp(trigger_variable = "cat")),
    "continuous weather variable"
  )
})

test_that("a weather trigger returns the common value", {
  th <- sp_trigger_thresholds(shock_exposure(), shock_sp(trigger_value = 41.5))
  expect_identical(th$type, "weather")
  expect_equal(th$value, 41.5)
  expect_identical(th$direction, "above")
})

test_that("return-period thresholds equal the hand-computed quantile per cell", {
  ex <- shock_exposure(30L)
  th <- sp_trigger_thresholds(ex, shock_sp(
    trigger_type = "return_period", trigger_return_period_years = 10,
    trigger_value = NA_real_
  ))
  tbl <- th$table
  expect_equal(nrow(tbl), 4L)
  expect_true(all(tbl$n_years == 30L & tbl$supported))
  # Cell a / month 3 holds temp = 1.03 .. 30.03, so the 0.9 quantile is 27.13
  row <- tbl[tbl$loc_id == "a" & tbl$int_month == 3L, ]
  expect_equal(row$threshold, unname(stats::quantile(1:30 + 0.03, 0.9)))
  expect_equal(row$threshold, 27.13)
  row_b <- tbl[tbl$loc_id == "b" & tbl$int_month == 9L, ]
  expect_equal(row_b$threshold, unname(stats::quantile(1:30 + 100.09, 0.9)))
  # Same activation probability everywhere: 3 of 30 years sit at or above it
  for (i in seq_len(nrow(tbl))) {
    cell <- ex[ex$loc_id == tbl$loc_id[i] & ex$int_month == tbl$int_month[i], ]
    expect_equal(sum(cell$temp >= tbl$threshold[i]), 3L)
  }
})

test_that("direction below mirrors above", {
  ex <- shock_exposure(40L)
  sp <- shock_sp(trigger_type = "return_period", trigger_return_period_years = 20,
                 trigger_value = NA_real_)
  up <- sp_trigger_thresholds(ex, sp)$table
  ex_neg <- ex
  ex_neg$temp <- -ex$temp
  down <- sp_trigger_thresholds(ex_neg, utils::modifyList(sp, list(trigger_direction = "below")))
  expect_identical(down$direction, "below")
  expect_equal(down$table$threshold, -up$threshold)
  # Low tail: at or below the threshold in 1 in 20 years of a 40-year record
  cell <- ex[ex$loc_id == "a" & ex$int_month == 3L, ]
  low <- sp_trigger_thresholds(ex, utils::modifyList(sp, list(trigger_direction = "below")))$table
  thr <- low$threshold[low$loc_id == "a" & low$int_month == 3L]
  expect_equal(sum(cell$temp <= thr), 2L)
})

test_that("a 1-in-N trigger needs N finite historical years", {
  ex <- shock_exposure(15L)
  sp <- shock_sp(trigger_type = "return_period", trigger_return_period_years = 20,
                 trigger_value = NA_real_)
  expect_error(sp_trigger_thresholds(ex, sp), "needs at least 20 historical years")
  # N equal to the record length is allowed
  ok <- sp_trigger_thresholds(ex, utils::modifyList(sp, list(trigger_return_period_years = 15)))
  expect_true(all(ok$table$supported))
  # Non-finite years do not count; short cells get NA and never fire
  ex$temp[ex$loc_id == "a" & ex$int_month == 3L & ex$sim_year <= 5L] <- NA
  mixed <- sp_trigger_thresholds(ex, utils::modifyList(sp, list(trigger_return_period_years = 15)))$table
  short <- mixed[mixed$loc_id == "a" & mixed$int_month == 3L, ]
  expect_equal(short$n_years, 10L)
  expect_false(short$supported)
  expect_true(is.na(short$threshold))
  expect_equal(sum(mixed$supported), 3L)
})

test_that("an invalid spec is refused with its message", {
  expect_error(
    sp_trigger_thresholds(shock_exposure(), shock_sp(trigger_value = NA_real_)),
    "threshold value"
  )
})

test_that("thresholds do not depend on row order", {
  ex <- shock_exposure(30L)
  sp <- shock_sp(trigger_type = "return_period", trigger_return_period_years = 10,
                 trigger_value = NA_real_)
  a <- sp_trigger_thresholds(ex, sp)$table
  b <- sp_trigger_thresholds(ex[rev(seq_len(nrow(ex))), ], sp)$table
  key <- function(t) t[order(t$loc_id, t$int_month), c("loc_id", "int_month", "threshold", "n_years")]
  expect_equal(key(a), key(b), ignore_attr = TRUE)
})

# Static eligibility in shock mode (P1-4) ----

shock_svy <- function(n = 600, seed = 21) {
  set.seed(seed)
  data.frame(
    welfare = rlnorm(n, 0.6, 0.7), hhsize = sample(1:8, n, replace = TRUE),
    weight = runif(n, 50, 150), proxy = rnorm(n)
  )
}

shock_targeting <- function(...) {
  shock_sp(targeting = "exante_poor", targeting_threshold = 25,
           inclusion_error_pct = 15, exclusion_error_pct = 10,
           transfer_n_payments = 6L, ...)
}

test_that("shock mode writes eligibility and no transfer", {
  svy <- shock_svy()
  out <- apply_policy_to_svy(svy, sp = shock_targeting(), seed = 5L)
  expect_false(SP_TRANSFER_COL %in% names(out))
  expect_type(out[[SP_ELIGIBLE_COL]], "logical")
  expect_equal(length(out[[SP_ELIGIBLE_COL]]), nrow(svy))
  expect_true(any(out[[SP_ELIGIBLE_COL]]) && !all(out[[SP_ELIGIBLE_COL]]))
})

test_that("shock eligibility equals the regular program's for the same seed", {
  svy <- shock_svy()
  reg <- apply_policy_to_svy(
    svy, sp = shock_targeting(sp_type = "regular", budget_mode = "transfer_first"),
    seed = 5L
  )
  shk <- apply_policy_to_svy(svy, sp = shock_targeting(), seed = 5L)
  expect_identical(shk[[SP_ELIGIBLE_COL]], reg[[SP_TRANSFER_COL]] > 0)
  # Deterministic per seed, and the seed matters
  again <- apply_policy_to_svy(svy, sp = shock_targeting(), seed = 5L)
  other <- apply_policy_to_svy(svy, sp = shock_targeting(), seed = 6L)
  expect_identical(shk[[SP_ELIGIBLE_COL]], again[[SP_ELIGIBLE_COL]])
  expect_false(identical(shk[[SP_ELIGIBLE_COL]], other[[SP_ELIGIBLE_COL]]))
})

test_that("an invalid shock spec writes nothing, and regular mode is unchanged", {
  svy <- shock_svy()
  bad <- apply_policy_to_svy(svy, sp = shock_targeting(trigger_value = NA_real_), seed = 5L)
  expect_false(any(c(SP_TRANSFER_COL, SP_ELIGIBLE_COL) %in% names(bad)))
  reg <- apply_policy_to_svy(
    svy, sp = shock_targeting(sp_type = "regular", budget_mode = "transfer_first"),
    seed = 5L
  )
  expect_true(SP_TRANSFER_COL %in% names(reg))
  expect_false(SP_ELIGIBLE_COL %in% names(reg))
})

test_that("a shock program with eligible households counts as an effect", {
  svy <- shock_svy()
  out <- apply_policy_to_svy(svy, sp = shock_targeting(), seed = 5L)
  expect_true(.scenario_has_effect(svy, out))
  expect_false(.scenario_has_effect(svy, svy))
})

# Trigger state (P1-3) ----

# Three locations, two years; temps and weights chosen so the exposed share is
# 0.3 in year 1 (only a) and 0.7 in year 2 (a and b).
state_exposure <- function() {
  data.frame(
    loc_id = rep(c("a", "b", "c"), 2), sim_year = rep(1:2, each = 3),
    temp = c(36, 30, 30, 36, 40, 20), weight = rep(c(30, 40, 30), 2)
  )
}

state_run <- function(scope, k = 0, ex = state_exposure()) {
  spec <- shock_sp(payout_scope = scope, national_k_pct = k)
  th <- sp_trigger_thresholds(ex[rep(1, 30), ], spec) # weather type: value only
  sp_trigger_state(ex, th, ex$weight, spec)
}

test_that("exposed share is weighted and by simulation year", {
  st <- state_run("local")
  expect_equal(unname(st$share), c(0.3, 0.7))
  expect_identical(names(st$share), c("1", "2"))
  expect_identical(st$exceeds, c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
  # Equal weights when none are given
  spec <- shock_sp()
  th <- sp_trigger_thresholds(state_exposure()[rep(1, 30), ], spec)
  eq <- sp_trigger_state(state_exposure(), th, NULL, spec)
  expect_equal(unname(eq$share), c(1 / 3, 2 / 3))
})

test_that("payout scope truth table, gate fired or not by row exceeds or not", {
  # k = 50: gate fires in year 2 only
  local <- state_run("local", 50)
  expect_identical(local$in_scope, c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_false(any(local$gate))
  trig <- state_run("national_triggered", 50)
  expect_identical(unname(trig$gate), c(FALSE, TRUE))
  expect_identical(trig$in_scope, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  all_hh <- state_run("national_all", 50)
  expect_identical(all_hh$in_scope, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  # A gate above the exposed share never fires
  expect_false(any(state_run("national_all", 80)$in_scope))
})

test_that("k = 0 makes national_triggered identical to local", {
  expect_identical(
    state_run("national_triggered", 0)$in_scope, state_run("local")$in_scope
  )
  # The gate needs some exposure, so a year with none stays unpaid
  ex <- state_exposure()
  ex$temp[ex$sim_year == 1] <- 10
  expect_identical(unname(state_run("national_triggered", 0, ex)$gate), c(FALSE, TRUE))
  expect_match(
    .sp_shock_problem(shock_sp(payout_scope = "national_all", national_k_pct = 0)),
    "above 0"
  )
})

test_that("below mirrors above and non-finite values never exceed", {
  ex <- state_exposure()
  ex$temp[2] <- NA
  spec <- shock_sp(trigger_direction = "below", trigger_value = 30)
  th <- sp_trigger_thresholds(ex[rep(1, 30), ], spec)
  st <- sp_trigger_state(ex, th, ex$weight, spec)
  expect_identical(st$exceeds, c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("return-period state fires in 1 of N years per cell and ignores row order", {
  ex <- shock_exposure(30L)
  ex$weight <- 100
  spec <- shock_sp(trigger_type = "return_period", trigger_return_period_years = 10,
                   trigger_value = NA_real_)
  th <- sp_trigger_thresholds(ex, spec)
  st <- sp_trigger_state(ex, th, ex$weight, spec)
  # 4 cells x 3 of 30 years
  expect_equal(sum(st$exceeds), 12L)
  cells <- paste(ex$loc_id, ex$int_month)
  expect_true(all(tapply(st$exceeds, cells, sum) == 3L))
  # Cells without a supported threshold never exceed
  th2 <- th
  th2$table$threshold[th2$table$loc_id == "a" & th2$table$int_month == 3L] <- NA
  st2 <- sp_trigger_state(ex, th2, ex$weight, spec)
  expect_false(any(st2$exceeds[ex$loc_id == "a" & ex$int_month == 3L]))
  # Row order does not change which rows exceed
  ord <- sample(nrow(ex))
  st3 <- sp_trigger_state(ex[ord, ], th, ex$weight[ord], spec)
  expect_identical(st3$exceeds, st$exceeds[ord])
  expect_equal(st3$share, st$share)
})

test_that("trigger state refuses an exposure without what it needs", {
  spec <- shock_sp()
  th <- sp_trigger_thresholds(shock_exposure(), spec)
  expect_error(sp_trigger_state(data.frame(temp = 1), th, NULL, spec), "sim_year")
  rp <- shock_sp(trigger_type = "return_period", trigger_return_period_years = 10,
                 trigger_value = NA_real_)
  th_rp <- sp_trigger_thresholds(shock_exposure(), rp)
  expect_error(
    sp_trigger_state(data.frame(sim_year = 1, temp = 1), th_rp, NULL, rp), "key columns"
  )
  expect_error(
    sp_trigger_state(state_exposure(), th, NULL, shock_sp(payout_scope = "x")), "payout scope"
  )
})

# Dynamic transfer (P1-5) ----

# State for `n_years` years over `n` survey rows, every row in scope or none.
dyn_state <- function(n, n_years = 1L, on = TRUE) {
  list(in_scope = rep(on, n * n_years))
}

dyn_reference <- function(svy, sp, unit = "hh", seed = 5L) {
  reg <- apply_policy_to_svy(
    svy, sp = utils::modifyList(sp, list(sp_type = "regular", budget_mode = "transfer_first",
      transfer_n_payments = sp$payments_per_activation)),
    analysis_unit = unit, seed = seed
  )
  shk <- apply_policy_to_svy(svy, sp = sp, analysis_unit = unit, seed = seed)
  list(transfer = reg[[SP_TRANSFER_COL]], eligible = shk[[SP_ELIGIBLE_COL]])
}

test_that("a trigger that never fires pays nothing", {
  svy <- shock_svy()
  ref <- dyn_reference(svy, shock_targeting())
  out <- sp_dynamic_transfer(dyn_state(nrow(svy), 3L, on = FALSE), ref$eligible,
    shock_targeting(), svy, "hh", rep(seq_len(nrow(svy)), 3L))
  expect_true(all(out == 0))
  expect_equal(length(out), 3L * nrow(svy))
})

test_that("an always-on trigger reproduces the regular program's transfer", {
  svy <- shock_svy()
  svy$ppp2021 <- runif(nrow(svy), 0.8, 1.2)
  variants <- list(
    shock_targeting(),
    shock_targeting(payments_per_activation = 4L, transfer_amount_usd = 55),
    shock_targeting(amount_basis = "per_capita"),
    shock_targeting(currency = "LCU", transfer_amount_usd = 20000)
  )
  for (sp in variants) {
    ref <- dyn_reference(svy, sp)
    ids <- rep(seq_len(nrow(svy)), 2L)
    out <- sp_dynamic_transfer(dyn_state(nrow(svy), 2L), ref$eligible, sp, svy, "hh", ids)
    expect_equal(out, rep(ref$transfer, 2L))
  }
  # Individual analysis units
  sp <- shock_targeting()
  ref <- dyn_reference(svy, sp, unit = "ind")
  out <- sp_dynamic_transfer(dyn_state(nrow(svy)), ref$eligible, sp, svy, "ind",
    seq_len(nrow(svy)))
  expect_equal(out, ref$transfer)
})

test_that("only in-scope rows of eligible households are paid", {
  svy <- shock_svy(n = 6)
  elig <- c(TRUE, TRUE, FALSE, TRUE, NA, FALSE)
  sp <- shock_sp(transfer_amount_usd = 36.5, payments_per_activation = 10L,
                 amount_basis = "per_capita")
  state <- list(in_scope = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))
  # Prediction rows 1..7 map to survey rows 1,2,3,4,5,6,1
  out <- sp_dynamic_transfer(state, elig, sp, svy, "hh", c(1:6, 1L))
  # 36.5 * 10 / 365 = 1 per person per day
  expect_equal(out, c(1, 0, 0, 1, 0, 0, 1))
  # Per household divides by hhsize
  sp2 <- utils::modifyList(sp, list(amount_basis = "per_household"))
  out2 <- sp_dynamic_transfer(state, elig, sp2, svy, "hh", c(1:6, 1L))
  expect_equal(out2[1], 1 / svy$hhsize[1])
})

test_that("the dynamic transfer rejects misaligned inputs", {
  svy <- shock_svy(n = 5)
  sp <- shock_sp()
  expect_error(sp_dynamic_transfer(dyn_state(5), rep(TRUE, 4), sp, svy, "hh", 1:5),
    "one value per survey row")
  expect_error(sp_dynamic_transfer(dyn_state(5), rep(TRUE, 5), sp, svy, "hh", 1:4),
    "map to survey rows")
  expect_error(sp_dynamic_transfer(dyn_state(5), rep(TRUE, 5), sp, svy, "hh", c(1:4, 9L)),
    "map to survey rows")
})

test_that("the dynamic effect uses the predicted level for log outcomes", {
  y <- log(c(100, 50, NA, NA))
  tr <- c(20, 0, 10, 0)
  eff <- sp_dynamic_effect(y, tr, is_log = TRUE)
  expect_equal(eff[1], log(120) - log(100))
  expect_equal(eff[2], 0)
  expect_true(is.na(eff[3]))
  expect_equal(eff[4], 0)
  # A smaller predicted level gains more in logs for the same transfer
  expect_gt(sp_dynamic_effect(log(50), 20, TRUE), sp_dynamic_effect(log(100), 20, TRUE))
  # Identity outcomes add the transfer directly
  expect_equal(sp_dynamic_effect(c(100, 50), c(20, 0), is_log = FALSE), c(20, 0))
})

test_that("shock transfers equal the regular program's welfare effect (identity outcome)", {
  svy <- shock_svy()
  sp <- shock_targeting()
  ref <- dyn_reference(svy, sp)
  out <- sp_dynamic_transfer(dyn_state(nrow(svy)), ref$eligible, sp, svy, "hh", seq_len(nrow(svy)))
  expect_equal(
    sp_dynamic_effect(svy$welfare, out, FALSE),
    sp_dynamic_effect(svy$welfare, ref$transfer, FALSE)
  )
})

# Shock preview (P1-8) ----

# Six one-person households in two locations (a: 1-3, b: 4-6), four years. a is
# hot in years 2 and 4, b in years 3 and 4.
preview_fixture <- function() {
  svy <- data.frame(welfare = 1:6, hhsize = 1L, weight = 100)
  table <- expand.grid(loc_id = c("a", "b"), sim_year = 2001:2004, stringsAsFactors = FALSE)
  table$code <- "BFA"; table$year <- "2021"; table$survname <- "EHCVM"; table$int_month <- 1L
  table$temp <- ifelse(
    (table$loc_id == "a" & table$sim_year %in% c(2002L, 2004L)) |
      (table$loc_id == "b" & table$sim_year %in% c(2003L, 2004L)), 30, 10
  )
  hh <- expand.grid(svy_row_id = 1:6, sim_year = 2001:2004)
  loc <- ifelse(hh$svy_row_id <= 3, "a", "b")
  row_index <- match(paste(loc, hh$sim_year), paste(table$loc_id, table$sim_year))
  pipe <- list(y_point = rep(1, nrow(hh)), svy_row_id = hh$svy_row_id, sim_year = hh$sim_year,
    weight = rep(100, nrow(hh)))
  pipe$weather_exposure <- list(status = "ok", table = table, row_index = row_index,
    prediction_row_id = seq_len(nrow(hh)))
  list(svy = svy, hist = list(pipeline = pipe, svy = svy))
}

preview_spec <- function(...) {
  utils::modifyList(
    shock_sp(targeting = "universal", inclusion_error_pct = 0, exclusion_error_pct = 0,
      transfer_amount_usd = 36.5, payments_per_activation = 10L, trigger_variable = "temp",
      trigger_value = 20),
    list(...)
  )
}

test_that("the preview reproduces a hand calculation", {
  fx <- preview_fixture()
  p <- sp_shock_preview(preview_spec(), fx$hist, fx$svy, "hh", seed = 5L)
  # One household is paid 36.5 * 10 = 365 a year; 3 households per location
  per_location <- 3 * 365 * 100
  expect_equal(p$annual$total_cost, c(0, per_location, per_location, 2 * per_location))
  expect_identical(p$annual$activated, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(p$annual$exposed_share, c(0, 0.5, 0.5, 1))
  expect_equal(p$activation_freq, 0.75)
  expect_equal(p$mean_cost, mean(p$annual$total_cost))
  expect_true(is.na(p$cost_1_in_20)) # four years cannot support a 1-in-20 cost
  expect_equal(p$mean_exposed_share, 0.5)
  expect_equal(p$n_record_years, 4L)
})

test_that("the preview follows scope, direction and administration", {
  fx <- preview_fixture()
  all_hh <- sp_shock_preview(preview_spec(payout_scope = "national_all", national_k_pct = 60),
    fx$hist, fx$svy, "hh", seed = 5L)
  # Only year 4 reaches 60 percent; then everyone is paid
  expect_equal(all_hh$annual$total_cost, c(0, 0, 0, 6 * 365 * 100))
  expect_equal(all_hh$activation_freq, 0.25)
  below <- sp_shock_preview(preview_spec(trigger_direction = "below"), fx$hist, fx$svy, "hh", seed = 5L)
  expect_equal(below$annual$activated, c(TRUE, TRUE, TRUE, FALSE))
  admin <- sp_shock_preview(preview_spec(admin_cost_pct = 20), fx$hist, fx$svy, "hh", seed = 5L)
  base <- sp_shock_preview(preview_spec(), fx$hist, fx$svy, "hh", seed = 5L)
  expect_equal(admin$annual$total_cost, base$annual$total_cost / 0.8)
  expect_equal(admin$annual$admin_cost, base$annual$total_cost / 0.8 * 0.2)
})

test_that("an always-on trigger costs what the regular program costs", {
  fx <- preview_fixture()
  sp <- preview_spec(trigger_value = -1e9)
  p <- sp_shock_preview(sp, fx$hist, fx$svy, "hh", seed = 5L)
  regular <- apply_policy_to_svy(fx$svy, sp = utils::modifyList(sp, list(
    sp_type = "regular", budget_mode = "transfer_first", transfer_n_payments = 10L)),
    seed = 5L)
  expect_equal(p$annual$total_cost, rep(.sp_transfer_totals(regular, "hh")$total_cost, 4L))
  expect_equal(p$activation_freq, 1)
})

test_that("the preview reports problems instead of failing", {
  fx <- preview_fixture()
  expect_match(
    sp_shock_preview(preview_spec(trigger_value = NA_real_), fx$hist, fx$svy)$problem,
    "threshold"
  )
  expect_match(
    sp_shock_preview(preview_spec(trigger_type = "return_period",
      trigger_return_period_years = 10, trigger_value = NA_real_),
      fx$hist, fx$svy, seed = 5L)$problem,
    "needs at least 10 historical years; the longest record has 4"
  )
  expect_match(
    sp_shock_preview(preview_spec(trigger_variable = "nope"), fx$hist, fx$svy, seed = 5L)$problem,
    "not a continuous weather variable"
  )
  expect_match(
    sp_shock_preview(preview_spec(), list(pipeline = list()), fx$svy)$problem, "not available"
  )
  # Other errors are not swallowed as user messages
  bad <- fx$svy
  bad$welfare <- NULL
  expect_error(sp_shock_preview(preview_spec(), fx$hist, bad, seed = 5L))
})

test_that("a 1-in-20 cost needs twenty years and is the 95th percentile", {
  fx <- preview_fixture()
  years <- 2001:2024
  table <- expand.grid(loc_id = c("a", "b"), sim_year = years, stringsAsFactors = FALSE)
  table$code <- "BFA"; table$year <- "2021"; table$survname <- "EHCVM"; table$int_month <- 1L
  table$temp <- ifelse(table$sim_year > 2019L, 30, 10) # 5 hot years of 24
  hh <- expand.grid(svy_row_id = 1:6, sim_year = years)
  loc <- ifelse(hh$svy_row_id <= 3, "a", "b")
  pipe <- list(y_point = rep(1, nrow(hh)), svy_row_id = hh$svy_row_id, sim_year = hh$sim_year,
    weight = rep(100, nrow(hh)))
  pipe$weather_exposure <- list(status = "ok", table = table,
    row_index = match(paste(loc, hh$sim_year), paste(table$loc_id, table$sim_year)),
    prediction_row_id = seq_len(nrow(hh)))
  p <- sp_shock_preview(preview_spec(), list(pipeline = pipe, svy = fx$svy), fx$svy, "hh", seed = 5L)
  expect_equal(p$n_years, 24L)
  expect_equal(p$cost_1_in_20, unname(stats::quantile(p$annual$total_cost, 0.95)))
  expect_equal(p$cost_1_in_20, 6 * 365 * 100)
})

test_that("the summary card shows expected activation and cost for a shock program", {
  fx <- preview_fixture()
  fx$svy$temp <- 1
  fx$hist$svy <- fx$svy
  fx$hist$pipeline$weather_exposure$weather_columns <- "temp"
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_card", survey_weather = shiny::reactiveVal(fx$svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(fx$hist)
  ), {
    session$setInputs(
      sp_type = "shock", targeting = "universal", transfer_amount_usd = 36.5,
      payments_per_activation = 10, trigger_type = "weather", trigger_variable = "temp",
      trigger_direction = "above", trigger_value = 20, payout_scope = "local"
    )
    session$elapse(500)
    html <- as.character(output$sp_reach_ui$html)
    expect_match(html, "Shock-responsive")
    expect_match(html, "Years with payments")
    expect_match(html, "75")
    expect_match(html, "1-in-20 year cost")
    expect_match(html, "Needs 20 years")
    # An unfinished trigger says what is missing instead of showing numbers
    session$setInputs(trigger_value = NA)
    session$elapse(500)
    expect_match(as.character(output$sp_reach_ui$html), "Enter a threshold value")
  })
})

# Diagnostics display and export (P1-7) ----

shock_summary_fixture <- function() {
  list(
    rows = data.frame(scenario = c("Historical", "SSP"), member = "m", sim_year = 1:2,
      activated = c(TRUE, FALSE), exposed_share = c(0.5, 0), transfer_cost = c(90, 0),
      admin_cost = c(10, 0), total_cost = c(100, 0), pop_tp = 1, pop_fp = 1, pop_fn = 1,
      pop_tn = 1, spend_total = 5, spend_no_event = 1),
    summary = data.frame(scenario = c("Historical", "SSP"), n_member_years = c(30, 176),
      activation_freq = c(0.2, 0.5), mean_total_cost = c(100, 200),
      median_total_cost = c(90, 180), cost_1_in_20 = c(300, NA), mean_transfer_cost = c(90, 180),
      mean_admin_cost = c(10, 20), false_positive_rate = c(0.1, 0.2),
      false_negative_rate = c(0.3, NA), leakage_share = c(0.05, 0.1))
  )
}

test_that("the shock summary is shown as a table and a chart, with NA as not available", {
  d <- .sp_shock_display(shock_summary_fixture()$summary)
  expect_identical(names(d), c("Measure", "Historical", "SSP"))
  expect_identical(d$SSP[d$Measure == "1-in-20 year cost"], "Not available")
  expect_identical(d$Historical[d$Measure == "Years with payments"], "20.0%")
  expect_s3_class(echart_shock_cost_distribution(shock_summary_fixture()$summary), "echarts4r")
  expect_s3_class(echart_shock_cost_distribution(NULL), "echarts4r")
  expect_equal(nrow(.sp_shock_display(NULL)), 0L)
})

test_that("the diagnostics tab renders the shock section only for a shock run", {
  shock <- shiny::reactiveVal(NULL)
  testServer(
    mod_3_08_diagnostics_server,
    args = list(
      id = "diag_shock", baseline_svy = reactive(NULL), policy_svy = reactive(NULL),
      shock_summary = shock, tabset_id = "tabs"
    ),
    {
      expect_null(output$shock_section_ui$html)
      shock(shock_summary_fixture())
      session$flushReact()
      expect_match(as.character(output$shock_section_ui$html), "Shock-responsive program")
      expect_match(as.character(output$shock_section_ui$html), "shock_cost_plot")
    }
  )
})

test_that("the effectiveness table reports a shock program's design figures (P2-2)", {
  base <- data.frame(welfare = c(1, 2, 4, 6), hhsize = 2, weight = 100)
  pol <- base
  # Ever-paid marker the run publishes; the design figures use the amount per
  # eligible household instead
  pol[[SP_TRANSFER_COL]] <- c(0, 0, 0, 0.1)
  sp <- reactiveVal(list(sp_type = "shock"))
  testServer(
    mod_3_08_diagnostics_server,
    args = list(
      id = "diag_shock_eff", baseline_svy = reactive(base), policy_svy = reactive(pol),
      diagnostic_summary = reactive(list(analysis_unit = "hh", transfer_currency = "PPP",
        admin_share = 0)),
      shock_summary = reactive(list(per_household = c(0.5, 1, 1, 0))),
      sp_scenario = sp, poverty_line = reactive(3), tabset_id = "tabs"
    ),
    {
      df <- sp_effectiveness_display()
      expect_false(is.null(df))
      row <- df[df$Metric == "Annual cost per person in the population (year with payments)", ]
      expect_match(row$Value, "^\\$228\\.1")
      expect_true("Leakage to the non-poor (if paid)" %in% df$Metric)
      expect_no_match(paste(df$Note, collapse = " "), "Not reported")
    }
  )
  # A per-household vector of the wrong length is not used
  testServer(
    mod_3_08_diagnostics_server,
    args = list(
      id = "diag_shock_eff2", baseline_svy = reactive(base), policy_svy = reactive(pol),
      diagnostic_summary = reactive(list(analysis_unit = "hh", transfer_currency = "PPP")),
      shock_summary = reactive(list(per_household = 1:2)),
      sp_scenario = sp, poverty_line = reactive(3), tabset_id = "tabs"
    ),
    {
      expect_null(sp_effectiveness_display())
    }
  )
})

# Run plumbing, diagnostics and export (P1-9) ----

test_that("shock and loss-event inputs travel in the config and the flyout toggle does not", {
  keep <- paste0("step3-sp-", c(
    "trigger_type", "trigger_variable", "trigger_direction", "trigger_value",
    "trigger_return_period_years", "payout_scope", "national_k_pct",
    "payments_per_activation", "loss_event_pct"
  ))
  expect_true(all(.export_keep_input(keep)))
  expect_false(.export_keep_input("step3-sp-trigger_toggle"))
})

test_that("the loss-event share enters the policy signature input", {
  a <- rlang::hash(.sig_plain(list(sp = shock_sp(loss_event_pct = 10))))
  b <- rlang::hash(.sig_plain(list(sp = shock_sp(loss_event_pct = 20))))
  expect_false(identical(a, b))
})

test_that("diagnostics treat a shock program as paid in at least one historical year", {
  svy <- shock_svy(n = 40)
  spec <- shock_targeting()
  policy <- apply_policy_to_svy(svy, sp = spec, seed = 5L)
  per_household <- .sp_dynamic_per_household(policy[[SP_ELIGIBLE_COL]], spec, policy, "hh")
  paid <- policy[[SP_ELIGIBLE_COL]] & seq_len(nrow(svy)) %% 2 == 0
  summary <- data.frame(scenario = "Historical", n_member_years = 30, activation_freq = 0.3,
    mean_total_cost = 1234, median_total_cost = 1000, cost_1_in_20 = NA_real_,
    mean_transfer_cost = 1000, mean_admin_cost = 234, false_positive_rate = NA_real_,
    false_negative_rate = NA_real_, leakage_share = NA_real_)
  snap <- .policy_diagnostics_snapshot(svy, policy, "welfare", "hh", sp = spec,
    shock = list(per_household = per_household, paid_historical = paid, summary = summary))
  expect_true(snap$is_shock)
  # Treated is paid at least once; the cost is the expected annual cost
  treated <- snap$treatment_matrix
  expect_equal(sum(treated$n[treated$treated_policy]), sum(paid))
  expect_equal(snap$total_cost_sum, 1234)
  expect_equal(snap$transfer_sum, 1000)
  expect_equal(snap$admin_sum, 234)
  expect_equal(snap$component_matrix$realized_cost[snap$component_matrix$component == "Social protection"], 1234)
  # A regular run carries no shock marker
  reg <- .policy_diagnostics_snapshot(
    svy, apply_policy_to_svy(svy, sp = utils::modifyList(spec, list(
      sp_type = "regular", budget_mode = "transfer_first", transfer_n_payments = 6L)), seed = 5L),
    "welfare", "hh", sp = spec
  )
  expect_null(reg$is_shock)
  expect_match(.policy_treatment_explanation(spec), "paid in at least one historical")
  expect_no_match(
    .policy_treatment_explanation(utils::modifyList(spec, list(sp_type = "regular"))),
    "paid in at least one historical"
  )
})

# Either extreme (trigger direction) ----

test_that("either extreme needs a low and a high threshold, in order", {
  base <- shock_sp(trigger_direction = "either", trigger_value = 30, trigger_value_low = 10)
  expect_null(.sp_shock_problem(base))
  expect_true(has_sp_change(base))
  expect_match(.sp_shock_problem(utils::modifyList(base, list(trigger_value_low = NA_real_))),
    "low and a high")
  expect_match(.sp_shock_problem(utils::modifyList(base, list(trigger_value = NA_real_))),
    "low and a high")
  expect_match(.sp_shock_problem(utils::modifyList(base, list(trigger_value_low = 40))),
    "must not exceed")
  # A return period needs no thresholds
  expect_null(.sp_shock_problem(shock_sp(
    trigger_direction = "either", trigger_type = "return_period",
    trigger_return_period_years = 10, trigger_value = NA_real_
  )))
  # The low threshold is not consulted for one-sided triggers
  expect_null(.sp_shock_problem(shock_sp(trigger_direction = "above")))
})

test_that("a weather-level trigger at either extreme fires beyond both thresholds", {
  ex <- data.frame(sim_year = 1:6, temp = c(5, 10, 15, 30, 31, NA))
  spec <- shock_sp(trigger_direction = "either", trigger_value = 30, trigger_value_low = 10)
  th <- sp_trigger_thresholds(ex[rep(1, 30), ], spec)
  expect_identical(th$direction, "either")
  expect_equal(c(th$value_low, th$value), c(10, 30))
  st <- sp_trigger_state(ex, th, NULL, spec)
  expect_identical(st$exceeds, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("a return period at either extreme splits the chance over two tails", {
  ex <- shock_exposure(40L)
  ex$temp <- ex$sim_year # 1..40 in every cell
  spec <- shock_sp(trigger_direction = "either", trigger_type = "return_period",
    trigger_return_period_years = 10, trigger_value = NA_real_)
  th <- sp_trigger_thresholds(ex, spec)
  tbl <- th$table
  expect_true(all(tbl$supported))
  # 1 in 10 shared over two tails: the 95th and 5th percentiles
  expect_equal(tbl$threshold[1], unname(stats::quantile(1:40, 0.95)))
  expect_equal(tbl$threshold_low[1], unname(stats::quantile(1:40, 0.05)))
  fires <- sp_trigger_state(ex, th, NULL, spec)$exceeds
  # 38.05 and 2.95: years 39, 40 and 1, 2 fire, 4 of 40 = 1 in 10 in every cell
  expect_true(all(tapply(fires, paste(ex$loc_id, ex$int_month), sum) == 4L))
  # The one-sided triggers keep their own probability
  one <- sp_trigger_thresholds(ex, shock_sp(trigger_type = "return_period",
    trigger_return_period_years = 10, trigger_value = NA_real_))
  expect_equal(one$table$threshold[1], unname(stats::quantile(1:40, 0.9)))
  expect_false("threshold_low" %in% names(one$table))
})

test_that("either extreme needs 2N years because each tail is a 1-in-2N level", {
  ex <- shock_exposure(15L)
  spec <- shock_sp(trigger_direction = "either", trigger_type = "return_period",
    trigger_return_period_years = 10, trigger_value = NA_real_)
  expect_error(sp_trigger_thresholds(ex, spec),
    "1-in-10 trigger at either extreme needs at least 20 historical years")
  ok <- sp_trigger_thresholds(ex, utils::modifyList(spec, list(trigger_return_period_years = 7)))
  expect_true(all(ok$table$supported)) # 14 years needed, 15 available
})

test_that("the module spec and flyout carry the either-extreme fields", {
  svy <- data.frame(welfare = 1:4, weight = 1)
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_either", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(NULL)
  ), {
    expect_true(is.na(sp_scenario_spec()$trigger_value_low))
    session$setInputs(sp_type = "shock", trigger_variable = "t",
      trigger_direction = "either", trigger_value = 30, trigger_value_low = 10,
      payments_per_activation = 2, transfer_amount_usd = 10)
    spec <- sp_scenario_spec()
    expect_identical(spec$trigger_direction, "either")
    expect_equal(c(spec$trigger_value_low, spec$trigger_value), c(10, 30))
    expect_null(.sp_shock_problem(spec))
    expect_match(output$trigger_summary, "beyond 10 or 30")
    session$setInputs(trigger_type = "return_period", trigger_return_period_years = 10)
    expect_match(output$trigger_summary, "1-in-10-year level at either end")
    expect_identical(output$trigger_value_hint, "(high threshold)")
  })
})

test_that("the preview reports the historical basis risk", {
  fx <- preview_fixture()
  p <- sp_shock_preview(preview_spec(loss_event_pct = 10), fx$hist, fx$svy, "hh", seed = 5L)
  expect_true(all(c("false_positive_rate", "false_negative_rate", "leakage_share") %in% names(p)))
  # y_point is constant, so no cell has a loss event: every paid cell is a false positive
  expect_gt(p$false_positive_rate, 0)
  expect_true(is.na(p$false_negative_rate)) # no loss events at all
  expect_equal(p$leakage_share, 1)
})

test_that("changing only the loss-event share does not mark results stale", {
  sp <- shiny::reactiveVal(list(transfer_amount_usd = 5, loss_event_pct = 10))
  ver <- shiny::reactiveVal(1L)
  testServer(mod_3_06_policy_sim_server, args = list(
    id = "ps_loss", survey_weather = shiny::reactiveVal(data.frame(welfare = runif(20))),
    sp_scenario = sp, infra_scenario = reactive(NULL), digital_scenario = reactive(NULL),
    labor_scenario = reactive(NULL), education_scenario = reactive(NULL),
    hist_sim = shiny::reactiveVal(NULL), survey_version = ver
  ), {
    baseline_hist_sim_rv(list(.sig = .policy_sig_from_live()))
    session$flushReact()
    policy_stale(FALSE)
    sp(list(transfer_amount_usd = 5, loss_event_pct = 25))
    session$flushReact()
    expect_false(policy_stale())
    sp(list(transfer_amount_usd = 6, loss_event_pct = 25))
    session$flushReact()
    expect_true(policy_stale())
  })
})

test_that("the trigger unit follows the Step 2 transformation (P1-16)", {
  sw <- data.frame(
    name = c("t", "p", "s", "x"), units = c("C", "mm", "mm", NA),
    transformation = c("None", "Deviation from mean", "Standardized anomaly", "None"),
    stringsAsFactors = FALSE
  )
  expect_identical(.sp_trigger_unit("t", sw), "C")
  expect_identical(.sp_trigger_unit("p", sw), "deviation from mean, mm")
  expect_identical(.sp_trigger_unit("s", sw), "standardized anomaly (z-score)")
  expect_identical(.sp_trigger_unit("x", sw), "")
  expect_identical(.sp_trigger_unit("zz", sw), "")
  expect_identical(.sp_trigger_unit("t", NULL), "")
  expect_identical(.sp_trigger_unit("t", data.frame(name = "t")), "")
})

test_that("the trigger flyout shows the unit of the chosen variable (P1-16)", {
  svy <- data.frame(welfare = 1:4, weight = 1, p = c(1, 2, 3, 4))
  hs <- list(svy = svy, pipeline = list(weather_exposure = list(weather_columns = "p")))
  sw <- data.frame(name = "p", units = "mm", transformation = "Standardized anomaly")
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_unit", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(hs),
    selected_weather = shiny::reactiveVal(sw)
  ), {
    session$setInputs(trigger_variable = "p")
    expect_match(output$trigger_unit, "standardized anomaly \\(z-score\\)")
  })
})

# Response check (P2-1) ----

# Small panel: one row per household-year. `exposure$table` holds the weather
# variable `t`; `row_index` maps prediction rows to it.
response_panel <- function(value, level, hh, weight = rep(1, length(value)), is_log = FALSE) {
  list(
    pipeline = list(
      y_point = if (is_log) log(level) else level, svy_row_id = hh, weight = weight,
      sim_year = rep(1L, length(value))
    ),
    exposure = list(table = data.frame(t = value), row_index = seq_along(value))
  )
}

# Two households, five years each: levels 8..12 and 40..60 (own mean 10 and 50),
# so the relative loss is -0.2, -0.1, 0, 0.1, 0.2 in both
response_base <- function(...) {
  response_panel(
    value = c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10),
    level = c(8, 9, 10, 11, 12, 40, 45, 50, 55, 60),
    hh = rep(1:2, each = 5L), ...
  )
}

test_that("response deciles are hand-calculated on the level scale with weights", {
  p <- response_base(weight = rep(c(1, 3), each = 5L))
  out <- sp_response_check(p$pipeline, p$exposure, "t", n_bins = 4L)
  expect_null(out$problem)
  # Cumulative weight shares .05 .2 .25 .4 .45 .6 .65 .8 .85 1 -> bins of
  # values {1,2,3}, {4,5}, {6,7}, {8,9,10}
  expect_equal(out$deciles$decile, 1:4)
  expect_equal(out$deciles$pop, c(5, 4, 4, 7))
  expect_equal(out$deciles$mean_value, c(10 / 5, (3 * 4 + 5) / 4, (3 * 6 + 7) / 4, 63 / 7))
  expect_equal(out$deciles$min_value, c(1, 4, 6, 8))
  expect_equal(out$deciles$max_value, c(3, 5, 7, 10))
  expect_equal(out$deciles$mean_loss, c(-0.9 / 5, -0.3 / 4, 0.1 / 4, 1.1 / 7))
  expect_identical(out$direction, "below")
})

test_that("a log outcome is back-transformed before averaging the loss", {
  lvl <- response_base()
  lg <- response_base(is_log = TRUE)
  a <- sp_response_check(lvl$pipeline, lvl$exposure, "t")
  b <- sp_response_check(lg$pipeline, lg$exposure, "t", is_log = TRUE)
  expect_equal(b$deciles, a$deciles)
  expect_equal(a$deciles$mean_loss, c(-.2, -.2, -.1, -.1, 0, 0, .1, .1, .2, .2))
  # Reading the logs as levels would give a different answer
  wrong <- sp_response_check(lg$pipeline, lg$exposure, "t", is_log = FALSE)
  expect_false(isTRUE(all.equal(wrong$deciles$mean_loss, a$deciles$mean_loss)))
})

test_that("direction: one-sided response gives the loss end, two-sided gives either", {
  p <- response_base()
  expect_identical(sp_response_check(p$pipeline, p$exposure, "t")$direction, "below")
  p$exposure$table$t <- -p$exposure$table$t
  high <- sp_response_check(p$pipeline, p$exposure, "t")
  expect_identical(high$direction, "above")
  expect_match(high$hint, "high t")

  u <- response_panel(1:10, c(80, rep(100, 8), 80), rep(1L, 10L))
  expect_identical(sp_response_check(u$pipeline, u$exposure, "t")$direction, "either")
  flat <- response_panel(1:10, rep(100, 10), rep(1L, 10L))
  none <- sp_response_check(flat$pipeline, flat$exposure, "t")
  expect_identical(none$direction, "none")
  expect_match(none$hint, "No clear response")
})

test_that("the hint names wet years for a standardized precipitation index", {
  p <- response_base()
  p$exposure$table$spei6 <- -p$exposure$table$t
  out <- sp_response_check(p$pipeline, p$exposure, "spei6")
  expect_identical(out$direction, "above")
  expect_match(out$hint, "wet years, high spei6", fixed = TRUE)
  expect_match(
    sp_response_check(p$pipeline, p$exposure, "t")$hint, "cold years, low t",
    fixed = TRUE
  )
})

test_that("an unusable variable returns a message, not an error", {
  p <- response_base()
  p$exposure$table$f <- factor(rep(c("a", "b"), 5L))
  p$exposure$table$const <- 1
  expect_match(sp_response_check(p$pipeline, p$exposure, "gone")$problem, "continuous")
  expect_match(sp_response_check(p$pipeline, p$exposure, "f")$problem, "continuous")
  expect_match(sp_response_check(p$pipeline, p$exposure, "const")$problem, "too few")
  expect_match(sp_response_check(p$pipeline, list(status = "unavailable"), "t")$problem, "continuous")
})

test_that("the response check is deterministic and ignores row order", {
  p <- response_base(weight = rep(c(1, 3), each = 5L))
  a <- sp_response_check(p$pipeline, p$exposure, "t")
  expect_identical(a, sp_response_check(p$pipeline, p$exposure, "t"))
  perm <- c(7L, 2L, 10L, 4L, 1L, 9L, 3L, 8L, 5L, 6L)
  q <- response_panel(
    p$exposure$table$t[perm], p$pipeline$y_point[perm], p$pipeline$svy_row_id[perm],
    p$pipeline$weight[perm]
  )
  expect_equal(sp_response_check(q$pipeline, q$exposure, "t"), a)
})

test_that("the trigger flyout shows the response check for the chosen variable", {
  p <- response_base()
  p$exposure$table$spei6 <- -p$exposure$table$t
  hs <- list(
    svy = data.frame(welfare = 1:2, weight = 1, spei6 = c(1, 2)),
    pipeline = c(p$pipeline, list(weather_exposure = c(
      list(schema = 1L, status = "ok", weather_columns = "spei6"), p$exposure
    )))
  )
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_resp", survey_weather = shiny::reactiveVal(hs$svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(hs)
  ), {
    session$setInputs(trigger_variable = "spei6")
    html <- as.character(output$trigger_response$html)
    expect_match(html, "wet years, high spei6", fixed = TRUE)
    session$setInputs(trigger_variable = "nope")
    expect_match(as.character(output$trigger_response$html), "continuous weather variable")
  })
})
