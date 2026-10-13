# Effect per cost and the shock cost pairing (P2-2). Targeting and cost metrics
# of the static program are covered in test-sp-design-options.R.

ce_effects <- function(...) {
  data.frame(
    scenario = c("Historical", "SSP"),
    effect_avg = c(-0.02, -0.01), effect_avg_sd = c(0.005, 0.002),
    effect_tail = c(-0.03, -0.015), stringsAsFactors = FALSE
  )
}
ce_costs <- function() {
  data.frame(
    scenario = c("Historical", "SSP"),
    cost_avg = c(4e6, 5e6), cost_tail = c(9e6, 12e6), stringsAsFactors = FALSE
  )
}
ce_row <- function(res, scenario, basis) {
  res[res$scenario == scenario & res$basis == basis, , drop = FALSE]
}

test_that("cost per person lifted matches a hand calculation, average and 1-in-20 year", {
  res <- sp_cost_effectiveness(ce_effects(), ce_costs(), "headcount_ratio",
    pop = 1e6, poverty_line = 3)
  expect_equal(nrow(res), 4L)
  a <- ce_row(res, "Historical", "Average year")
  # Rate falls 2 pp of 1 million people: 20,000 people lifted; cost 4 million
  expect_equal(a$amount, 20000)
  expect_equal(a$value, 4e6 / 20000)
  expect_identical(a$status, "ok")
  # 95% interval from the coefficient SD, applied to the effect endpoints
  half <- stats::qnorm(0.975) * 0.005
  expect_equal(a$lo, 4e6 / ((0.02 + half) * 1e6))
  expect_equal(a$hi, 4e6 / ((0.02 - half) * 1e6))
  expect_identical(a$interval, "95% interval")
  # Tail year: 30,000 lifted at the 1-in-20 cost of 9 million, no interval
  t <- ce_row(res, "Historical", "1-in-20 year")
  expect_equal(t$value, 9e6 / 30000)
  expect_identical(t$interval, "none")
  expect_true(is.na(t$lo))
  expect_match(a$amount_kind, "Net people lifted")
})

test_that("near-zero, wrong-signed and unbounded effects never give an extreme ratio", {
  run <- function(effect, sd = 0.001, cost = 4e6) {
    sp_cost_effectiveness(
      data.frame(scenario = "S", effect_avg = effect, effect_avg_sd = sd, effect_tail = effect),
      data.frame(scenario = "S", cost_avg = cost, cost_tail = cost),
      "headcount_ratio", pop = 1e6, poverty_line = 3
    )
  }
  # 0.001 pp of 1 million = 10 people, under the 100-person threshold
  tiny <- run(-1e-5)
  expect_true(all(tiny$status == "not_applicable") && all(is.na(tiny$value)))
  wrong <- run(0.01)
  expect_true(all(wrong$status == "not_applicable") && all(is.na(wrong$value)))
  expect_match(wrong$note[[1L]], "meaningful")
  # Interval reaches zero: the estimate stays, the interval is not bounded
  wide <- run(-0.004, sd = 0.005)
  a <- wide[wide$basis == "Average year", ]
  expect_identical(a$status, "ok")
  expect_equal(a$value, 4e6 / 4000)
  expect_identical(a$interval, "not bounded")
  expect_true(is.na(a$lo) && is.na(a$hi))
  # A year without spending, and a missing effect
  free <- run(-0.02, cost = 0)
  expect_true(all(free$status == "not_applicable"))
  expect_match(free$note[[1L]], "No spending")
  missing <- sp_cost_effectiveness(
    data.frame(scenario = "S", effect_avg = -0.02, effect_avg_sd = NA_real_, effect_tail = NA_real_),
    data.frame(scenario = "S", cost_avg = 4e6, cost_tail = 4e6),
    "headcount_ratio", pop = 1e6, poverty_line = 3
  )
  expect_identical(missing$status, c("ok", "unavailable"))
})

test_that("money metrics give a gain per $1, and unsupported metrics say so", {
  one <- function(method, effect, ...) {
    sp_cost_effectiveness(
      data.frame(scenario = "S", effect_avg = effect, effect_avg_sd = NA_real_, effect_tail = effect),
      data.frame(scenario = "S", cost_avg = 4e6, cost_tail = 4e6),
      method, pop = 1e6, ...
    )
  }
  # Poverty gap: -1 pp of the line (3 per day) over 1 million people for 365 days
  gap <- one("gap", -0.01, poverty_line = 3)
  expect_equal(gap$amount[[1L]], 0.01 * 3 * 1e6 * 365)
  expect_equal(gap$value[[1L]], 0.01 * 3 * 1e6 * 365 / 4e6)
  # Mean welfare per day and the population sum
  mean_res <- one("mean", 0.1, level_unit = "$ per day")
  expect_equal(mean_res$value[[1L]], 0.1 * 1e6 * 365 / 4e6)
  total <- one("total", 1e5, level_unit = "$ per day")
  expect_equal(total$value[[1L]], 1e5 * 365 / 4e6)
  # A worse metric is not applicable
  expect_identical(one("mean", -0.1, level_unit = "$ per day")$status[[1L]], "not_applicable")
  # Not money per day, or a metric without a per-dollar reading
  expect_identical(one("mean", 0.1, level_unit = "index")$status[[1L]], "not_applicable")
  expect_match(one("gini", -0.01)$note[[1L]], "no per-dollar reading")
  # Without a poverty line the poverty figures are not available
  expect_identical(one("gap", -0.01)$status[[1L]], "unavailable")
  expect_identical(one("headcount_ratio", -0.01, poverty_line = NA)$status[[1L]], "unavailable")
})

test_that("another active lever makes the figures not applicable", {
  res <- sp_cost_effectiveness(ce_effects(), ce_costs(), "headcount_ratio",
    pop = 1e6, poverty_line = 3, sp_only = FALSE)
  expect_true(all(res$status == "not_applicable"))
  expect_match(res$note[[1L]], "other than the cash transfer")
})

test_that("the cost-effectiveness display prints one string per figure", {
  res <- sp_cost_effectiveness(ce_effects(), ce_costs(), "headcount_ratio",
    pop = 1e6, poverty_line = 3)
  d <- .sp_ce_display(res, "PPP")
  expect_equal(nrow(d), 4L)
  expect_identical(d[["Cost per person lifted out of poverty (net)"]][[1L]], "$200.00")
  expect_identical(d[["Net people lifted"]][[1L]], "20,000")
  expect_identical(d[["Annual cost"]][[1L]], "$4,000,000.00")
  expect_match(d[["95% interval"]][[1L]], "^\\$[0-9,.]+ to \\$[0-9,.]+$")
  expect_identical(d[["95% interval"]][[2L]], "")
  na <- sp_cost_effectiveness(ce_effects(), ce_costs(), "headcount_ratio", pop = 1e6)
  expect_true(all(.sp_ce_display(na, "LCU")[["Cost per person lifted out of poverty (net)"]] == "Not available"))
  expect_equal(nrow(.sp_ce_display(NULL)), 0L)
})

test_that("effects join the headline summary and the 1-in-20 tail", {
  summary <- data.frame(scenario = c("Hist", "SSP"), value = c(-0.02, -0.01), coef_sd = c(0.005, NA))
  tails <- data.frame(
    scenario = c("Hist", "Hist", "SSP", "SSP"),
    scope = "baseline_anchored", return_period = c(10, 20, 10, 20),
    total = c(-0.025, -0.03, -0.012, NA), status = c("ok", "ok", "ok", "unavailable")
  )
  out <- sp_effect_by_scenario(summary, tails)
  expect_equal(out$effect_avg, c(-0.02, -0.01))
  expect_equal(out$effect_avg_sd, c(0.005, NA))
  expect_equal(out$effect_tail, c(-0.03, NA))
  expect_equal(sp_effect_by_scenario(summary, NULL)$effect_tail, c(NA_real_, NA_real_))
  expect_equal(nrow(sp_effect_by_scenario(NULL)), 0L)
})

# Shock cost pairing ----

cost_rows <- function() {
  data.frame(
    scenario = rep(c("Run hist", "SSP", "SSP"), each = 4L),
    member = rep(c("Historical", "m1", "m2"), each = 4L),
    sim_year = rep(1:4, 3L),
    total_cost = c(10, 20, 30, 40, 10, 20, 30, 40, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}
cost_support <- function(member, baseline, scenario = "SSP") {
  s <- adverse_year_support(baseline, 1:4, 0.5, "high")
  as.data.frame(c(list(scenario = scenario, model_id = member), s), stringsAsFactors = FALSE)
}

test_that("shock cost is an equal-member mean on the effect's own support", {
  sup <- rbind(
    cost_support("m1", c(5, 1, 3, 2)),
    cost_support("m2", c(1, 2, 3, 4))
  )
  out <- sp_shock_cost_by_scenario(cost_rows(), supports = sup, historical = "Hist",
    probability = 0.5)
  expect_equal(out$scenario, c("Hist", "SSP"))
  # SSP: members average 25 and 1, equally weighted
  expect_equal(out$cost_avg[out$scenario == "SSP"], (25 + 1) / 2)
  expect_equal(out$cost_avg[out$scenario == "Hist"], 25)
  # m1 baseline sorted 1,2,3,5 -> rank 2.5 = half of year 4 (40) and year 3 (30)
  # m2 baseline sorted 1,2,3,4 -> half of year 3 and year 2 (1 each = 1)
  expect_equal(out$cost_tail[out$scenario == "SSP"], (35 + 1) / 2)
  expect_true(is.na(out$cost_tail[out$scenario == "Hist"]))
})

test_that("shock cost keeps only the matched member-years, and a missing support year gives NA", {
  matched <- data.frame(
    scenario = c(rep("Hist", 3), rep("SSP", 3), rep("SSP", 4)),
    model_id = c(rep("Historical", 3), rep("m1", 3), rep("m2", 4)),
    sim_year = c(1:3, 1:3, 1:4)
  )
  sup <- rbind(cost_support("m1", c(5, 1, 3, 2)), cost_support("m2", c(1, 2, 3, 4)))
  out <- sp_shock_cost_by_scenario(cost_rows(), matched = matched, supports = sup,
    historical = "Hist", probability = 0.5)
  # Year 4 of the historical arm and of m1 is dropped from the average
  expect_equal(out$cost_avg[out$scenario == "Hist"], 20)
  expect_equal(out$cost_avg[out$scenario == "SSP"], (20 + 1) / 2)
  # m1's support needs year 4, which is not in its matched years
  expect_true(is.na(out$cost_tail[out$scenario == "SSP"]))
  expect_equal(nrow(sp_shock_cost_by_scenario(NULL)), 0L)
})

test_that("shock cost does not depend on row order", {
  sup <- rbind(cost_support("m1", c(5, 1, 3, 2)), cost_support("m2", c(1, 2, 3, 4)))
  rows <- cost_rows()
  a <- sp_shock_cost_by_scenario(rows, supports = sup, historical = "Hist", probability = 0.5)
  b <- sp_shock_cost_by_scenario(rows[c(9:12, 5:8, 4:1), ], supports = sup,
    historical = "Hist", probability = 0.5)
  expect_equal(b[order(b$scenario), ], a[order(a$scenario), ], ignore_attr = TRUE)
})

test_that("climate-adjusted cost compares each scenario with the reference", {
  summary <- data.frame(
    scenario = c("Historical", "SSP"), activation_freq = c(0.10, 0.25),
    mean_total_cost = c(100, 150), cost_1_in_20 = c(300, 500)
  )
  out <- sp_shock_climate_cost(summary)
  expect_equal(out$activation_change, c(0, 0.15))
  expect_equal(out$cost_change, c(0, 0.5))
  expect_equal(out$is_reference, c(TRUE, FALSE))
  expect_equal(sp_shock_climate_cost(summary, "SSP")$cost_change, c(100 / 150 - 1, 0))
  zero <- summary
  zero$mean_total_cost[[1L]] <- 0
  expect_true(all(is.na(sp_shock_climate_cost(zero)$cost_change)))
  expect_equal(nrow(sp_shock_climate_cost(NULL)), 0L)
})

# Targeting figures of a shock program ----

test_that("a shock program reports who can be paid and the cost of a year with payments", {
  base <- data.frame(welfare = c(1, 2, 4, 6), hhsize = 2, weight = 100)
  pol <- base
  # Daily per-capita amount of eligible households (hh 1, 2 and 3)
  pol[[SP_TRANSFER_COL]] <- c(0.5, 1, 1, 0)
  res <- sp_effectiveness(base, pol, 3, shock = TRUE,
    eligibility = c(TRUE, TRUE, FALSE, TRUE))
  m <- stats::setNames(res$metrics$value, res$metrics$metric)
  reg <- sp_effectiveness(base, pol, 3, eligibility = c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(m[["Annual cost per person in the population (year with payments)"]], 250 * 365 / 400)
  expect_equal(m[["Leakage to the non-poor (if paid)"]], 100 / 250)
  expect_equal(m[["Adequacy for poor recipients (if paid)"]], 150 / 300)
  expect_equal(m[["Coverage of the poor"]], 1)
  expect_false(any(grepl("year with payments|if paid", reg$metrics$metric)))
  expect_match(res$metrics$note[[1L]], "every eligible household is paid")
})

# Coverage and adequacy of the shock-affected poor ----

test_that("reach rows count loss-event household-years, payments and losses per survey row", {
  plan <- list(loss_event_pct = 10)
  pipeline <- list(svy_row_id = c(1L, 1L, 2L, 2L))
  level <- c(80, 100, 50, 60)
  ref <- c(100, 100, 60, 60)
  rel <- level / ref - 1
  m <- .sp_shock_reach_rows(plan, pipeline, level, ref, rel, rep(TRUE, 4L), c(5, 0, 0, 0))
  # Loss events: row 1 (-20%) and row 3 (-16.7%); only row 1 was paid
  expect_equal(unname(m["1", ]), c(1, 1, 20, 5))
  expect_equal(unname(m["2", ]), c(1, 0, 10, 0))
  expect_identical(colnames(m), .sp_shock_reach_cols)
  # An 18% event share leaves only the first
  strict <- .sp_shock_reach_rows(list(loss_event_pct = 18), pipeline, level, ref, rel,
    rep(TRUE, 4L), c(5, 0, 0, 0))
  expect_equal(rownames(strict), "1")
  # No event gives an empty matrix
  none <- .sp_shock_reach_rows(plan, pipeline, level, ref, rep(0, 4L), rep(TRUE, 4L), rep(0, 4L))
  expect_equal(dim(none), c(0L, 4L))
})

test_that("reach adds up across members and keeps scenarios apart", {
  a <- matrix(c(1, 1, 20, 5), 1, dimnames = list("2", .sp_shock_reach_cols))
  acc <- .sp_shock_reach_add(list(), "S", a, 3L)
  acc <- .sp_shock_reach_add(acc, "S", a, 3L)
  acc <- .sp_shock_reach_add(acc, "T", a, 3L)
  expect_equal(unname(acc$S[2, ]), c(2, 2, 40, 10))
  expect_equal(sum(acc$S[-2, ]), 0)
  expect_equal(unname(acc$T[2, ]), c(1, 1, 20, 5))
  expect_identical(.sp_shock_reach_add(acc, "U", NULL, 3L), acc)
})

test_that("coverage and adequacy of the shock-affected poor follow the display line", {
  svy <- data.frame(welfare = c(1, 2, 5), weight = c(100, 300, 100))
  m <- matrix(c(
    2, 1, 40, 10, # poor, paid in 1 of 2 loss-event years
    4, 4, 80, 60, # poor, always paid
    9, 0, 90, 0 # not poor
  ), 3, byrow = TRUE, dimnames = list(NULL, .sp_shock_reach_cols))
  out <- sp_shock_reach(list(S = m), svy, poverty_line = 3)
  # Weighted by people: 100 * 2 + 300 * 4 loss-event years, 100 * 1 + 300 * 4 covered
  expect_equal(out$coverage, (100 * 1 + 300 * 4) / (100 * 2 + 300 * 4))
  expect_equal(out$adequacy, (100 * 10 + 300 * 60) / (100 * 40 + 300 * 80))
  expect_equal(out$poor_affected_years, 1400)
  # A lower line removes the second household from the poor
  low <- sp_shock_reach(list(S = m), svy, poverty_line = 1.5)
  expect_equal(low$coverage, 0.5)
  expect_equal(low$adequacy, 10 / 40)
  # No line, no reach, or a mismatched frame give no rows
  expect_equal(nrow(sp_shock_reach(list(S = m), svy, NULL)), 0L)
  expect_equal(nrow(sp_shock_reach(NULL, svy, 3)), 0L)
  expect_equal(nrow(sp_shock_reach(list(S = m[1:2, ]), svy, 3)), 0L)
  # Nobody poor is affected: not available, not zero
  rich <- sp_shock_reach(list(S = m), svy, poverty_line = 0.5)
  expect_true(is.na(rich$coverage) && is.na(rich$adequacy))
})
