# Social protection cost and targeting effectiveness ----
#
# Pure functions (no Shiny) behind the Step 3 diagnostics "Cost and targeting"
# table (plan: review/step3_cost_effectiveness_plan.md, sections 4.1 and 4.2;
# task P0-5 in review/sp_shock_responsive_tasks.md). They need only the baseline
# and policy survey frames, so every figure is exact for the run's single seeded
# targeting draw. The effect-per-cost functions further down (P2-2) join a Step 3
# effect to the cost; decisions in section 10 of that plan and in
# review/sp_shock_responsive_tasks.md settle their definitions.

#' Cost and targeting effectiveness of a social-protection transfer
#'
#' Conventions follow `.sp_transfer_totals()`: survey weights represent people;
#' in household analysis the transfer column is per capita, so a household's
#' spending is `transfer * weight` (or `transfer * hhsize` without weights).
#' "Poor" means baseline welfare below `poverty_line`. The line is entered in
#' the outcome's currency and converted to the stored 2021 PPP scale per row
#' (`currency = "LCU"`), the same way the Results poverty line is.
#'
#' @param baseline_svy,policy_svy Survey frames before and after
#'   `apply_policy_to_svy()`; same rows. `policy_svy` carries `SP_TRANSFER_COL`.
#' @param poverty_line Poverty line in the outcome's currency, or NULL/NA when
#'   unavailable (the targeting metrics are then reported as not available).
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @param currency `"PPP"` or `"LCU"`; see `sp$currency`.
#' @param admin_share Administration as a share of total cost, see
#'   `.sp_admin_share()`.
#' @param eligibility Optional logical vector, the ideal eligibility before
#'   targeting errors (as kept by the diagnostics snapshot). Enables the
#'   realised inclusion and exclusion error rates.
#' @param weight_col Name of the survey weight column.
#' @param shock For a shock-responsive program: `policy_svy` then carries the
#'   per-household transfer paid in an activated year (not a transfer paid every
#'   year), so cost, leakage and adequacy describe a year in which every
#'   eligible household is paid, and the names say so. Coverage and the
#'   targeting errors describe who can be paid.
#'
#' @return A list: `status` (`"ok"` or `"no_transfer"`), `has_line` (logical)
#'   and `metrics`, a data frame with `metric`, `value` (numeric, NA when not
#'   available), `kind` (`"money"`, `"percent"` or `"text"`) and `note`.
#' @keywords internal
sp_effectiveness <- function(baseline_svy, policy_svy, poverty_line = NULL,
                             analysis_unit = "hh", currency = "PPP",
                             admin_share = 0, eligibility = NULL,
                             weight_col = "weight", shock = FALSE) {
  empty <- list(
    status = "no_transfer", has_line = FALSE,
    metrics = data.frame(
      metric = character(), value = numeric(), kind = character(),
      note = character(), stringsAsFactors = FALSE
    )
  )
  if (!is.data.frame(baseline_svy) || !is.data.frame(policy_svy) ||
    nrow(baseline_svy) != nrow(policy_svy) || nrow(baseline_svy) == 0L ||
    !SP_TRANSFER_COL %in% names(policy_svy) ||
    !"welfare" %in% names(baseline_svy)) {
    return(empty)
  }

  n <- nrow(baseline_svy)
  v <- suppressWarnings(as.numeric(policy_svy[[SP_TRANSFER_COL]]))
  has_w <- weight_col %in% names(baseline_svy)
  w <- if (has_w) suppressWarnings(as.numeric(baseline_svy[[weight_col]])) else rep(1, n)
  w[!is.finite(w) | w < 0] <- 0
  hh <- if (identical(analysis_unit, "hh") && "hhsize" %in% names(baseline_svy)) {
    h <- suppressWarnings(as.numeric(baseline_svy$hhsize))
    h[!is.finite(h) | h <= 0] <- 1
    h
  } else {
    rep(1, n)
  }
  # Persons (or units) each row stands for, and spending per row per day.
  persons <- if (identical(analysis_unit, "hh") && !has_w) hh else w
  spend <- ifelse(is.finite(v), v, 0) * persons
  recip <- is.finite(v) & v > 0

  totals <- .sp_transfer_totals(policy_svy, analysis_unit, currency, admin_share)
  if (!any(recip) || !is.finite(totals$total_cost) || totals$total_cost <= 0) {
    return(empty)
  }

  pop <- sum(persons)
  rows <- list()
  add <- function(metric, value, kind, note = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      metric = metric, value = as.numeric(value), kind = kind, note = note,
      stringsAsFactors = FALSE
    )
  }

  sfx <- if (isTRUE(shock)) " (year with payments)" else ""
  add(paste0("Annual cost per person in the population", sfx),
    if (pop > 0) totals$total_cost / pop else NA_real_, "money",
    if (isTRUE(shock)) {
      paste(
        "Cost of a year in which every eligible household is paid, including",
        "administration, over all represented people: the most the program",
        "costs in one year."
      )
    } else {
      "Total annual cost (including administration) over all represented people."
    })
  add("Administration share of cost", admin_share, "percent",
    "Set in Payment settings; 0 means no administration cost is counted.")

  # Targeting accuracy against the ideal rule (before inclusion/exclusion errors)
  if (!is.null(eligibility) && length(eligibility) == n) {
    elig <- as.logical(eligibility)
    elig[is.na(elig)] <- FALSE
    add("Inclusion error (realised)",
      if (sum(persons[recip]) > 0) sum(persons[recip & !elig]) / sum(persons[recip]) else NA_real_,
      "percent", "Recipients who are not eligible under the rule, as a share of recipients.")
    add("Exclusion error (realised)",
      if (sum(persons[elig]) > 0) sum(persons[elig & !recip]) / sum(persons[elig]) else NA_real_,
      "percent", "Eligible units without a transfer, as a share of eligible units.")
  }

  # Poverty-line metrics
  pl <- suppressWarnings(as.numeric(poverty_line)[1L])
  has_line <- is.finite(pl) && pl > 0
  line_note <- "Needs a positive poverty line."
  if (has_line) {
    line <- .povline_to_ppp(rep(pl, n), baseline_svy, identical(.sp_currency(currency), "LCU"))
    welfare <- suppressWarnings(as.numeric(baseline_svy$welfare))
    poor <- is.finite(welfare) & is.finite(line) & welfare < line
    poor_pop <- sum(persons[poor])
    add("Coverage of the poor",
      if (poor_pop > 0) sum(persons[poor & recip]) / poor_pop else NA_real_,
      "percent", if (isTRUE(shock)) {
        "Poor people (baseline welfare below the line) who are eligible to be paid."
      } else {
        "Poor people (baseline welfare below the line) who receive a transfer."
      })
    add(paste0("Leakage to the non-poor", if (isTRUE(shock)) " (if paid)" else ""),
      sum(spend[recip & !poor]) / sum(spend[recip]),
      "percent", "Share of transfer spending that reaches people above the line.")
    pr <- recip & poor
    gap_total <- sum(persons[pr] * (line[pr] - welfare[pr]))
    add(paste0("Adequacy for poor recipients", if (isTRUE(shock)) " (if paid)" else ""),
      if (gap_total > 0) sum(spend[pr]) / gap_total else NA_real_,
      "percent",
      "Transfers to poor recipients as a share of their poverty gap (100% closes it).")
  } else {
    for (m in c("Coverage of the poor",
                paste0("Leakage to the non-poor", if (isTRUE(shock)) " (if paid)" else ""),
                paste0("Adequacy for poor recipients", if (isTRUE(shock)) " (if paid)" else ""))) {
      add(m, NA_real_, "percent", line_note)
    }
  }

  list(
    status = "ok", has_line = has_line,
    metrics = do.call(rbind, rows)
  )
}

# Display table for `sp_effectiveness()`: strings, one rule for every figure.
# Money uses the entry-currency prefix; percentages one decimal place.
.sp_effectiveness_display <- function(res, currency = "PPP") {
  m <- res$metrics
  if (is.null(m) || nrow(m) == 0L) {
    return(data.frame(Metric = character(), Value = character(), Note = character()))
  }
  value <- vapply(seq_len(nrow(m)), function(i) {
    if (identical(m$kind[[i]], "money")) {
      fmt_num(m$value[[i]], digits = 2, prefix = .sp_currency_prefix(currency))
    } else {
      fmt_num(100 * m$value[[i]], digits = 1, suffix = "%", na = "Not available")
    }
  }, character(1))
  data.frame(Metric = m$metric, Value = value, Note = m$note, stringsAsFactors = FALSE)
}

# Effect per cost (P2-2) ----

# An effect smaller than this share of the population (0.01 percentage points of
# the poverty rate) is treated as zero: a cost per person lifted would be an
# extreme number, never a finding (decision 3).
SP_CE_MIN_EFFECT <- 1e-4

#' People represented by a survey frame
#'
#' The denominator of "people lifted": the sum of survey weights, or, for
#' unweighted household rows, the household members (the convention of
#' `sp_effectiveness()`).
#'
#' @param svy Survey frame.
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @return One number, `NA` without rows.
#' @keywords internal
.sp_ce_population <- function(svy, analysis_unit = "hh") {
  if (!is.data.frame(svy) || !nrow(svy)) {
    return(NA_real_)
  }
  if ("weight" %in% names(svy)) {
    w <- suppressWarnings(as.numeric(svy$weight))
    return(sum(w[is.finite(w) & w >= 0]))
  }
  if (identical(analysis_unit, "hh") && "hhsize" %in% names(svy)) {
    h <- suppressWarnings(as.numeric(svy$hhsize))
    h[!is.finite(h) | h <= 0] <- 1
    return(sum(h))
  }
  as.numeric(nrow(svy))
}

#' Policy effect by scenario, for the cost-effectiveness figures
#'
#' @param paired_summary Expected paired policy effect per scenario (`scenario`,
#'   `value`, optionally `coef_sd`): the headline summary of Results.
#' @param tails Tail rows of the metric decomposition (`scenario`, `scope`,
#'   `return_period`, `total`, `status`).
#' @param return_period Adverse return period in years.
#' @return Data frame: `scenario`, `effect_avg`, `effect_avg_sd` (coefficient SD
#'   of the paired contrast) and `effect_tail` (baseline-anchored adverse-year
#'   effect; `NA` when unavailable).
#' @keywords internal
sp_effect_by_scenario <- function(paired_summary, tails = NULL, return_period = 20) {
  if (!is.data.frame(paired_summary) || !nrow(paired_summary) ||
    !all(c("scenario", "value") %in% names(paired_summary))) {
    return(data.frame(
      scenario = character(), effect_avg = numeric(), effect_avg_sd = numeric(),
      effect_tail = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    scenario = as.character(paired_summary$scenario),
    effect_avg = as.numeric(paired_summary$value),
    effect_avg_sd = if ("coef_sd" %in% names(paired_summary)) {
      as.numeric(paired_summary$coef_sd)
    } else {
      NA_real_
    },
    effect_tail = NA_real_, stringsAsFactors = FALSE
  )
  if (is.data.frame(tails) && nrow(tails) &&
    all(c("scenario", "scope", "return_period", "total", "status") %in% names(tails))) {
    use <- tails$scope == "baseline_anchored" &
      abs(tails$return_period - return_period) < 1e-8 & tails$status == "ok"
    use[is.na(use)] <- FALSE
    out$effect_tail <- as.numeric(tails$total[use])[match(
      out$scenario, as.character(tails$scenario[use])
    )]
  }
  out
}

#' Cost-effectiveness of a cash transfer in the average and a 1-in-20 year
#'
#' Joins the Results effect on the selected metric to the program's annual cost
#' (transfers plus administration, in the entry currency). Which ratio leads
#' depends on the metric (decision 3):
#' \itemize{
#'   \item Poverty rate: cost per person lifted out of poverty, from the net
#'     change in the number of poor (`-effect * pop`). Gross movements are not
#'     built, so a household that falls into poverty offsets one that leaves it.
#'   \item Poverty gap: the reduction in the aggregate gap in money terms
#'     (`-effect * line * pop * 365`) per unit of cost.
#'   \item Mean and population sum of a money outcome per day: the welfare
#'     change in money terms per unit of cost.
#'   \item Anything else has no per-dollar reading.
#' }
#' A ratio is never an extreme number: a poverty-rate effect under `min_effect`
#' of the population, an effect with the wrong sign, or a year without spending
#' is `"not_applicable"`. The average-year result carries a 95 percent interval
#' from the coefficient SD of the effect; for cost per person lifted the
#' interval is `"not bounded"` when the effect interval reaches zero. The
#' adverse-year result has no interval (component uncertainty is not
#' estimated).
#'
#' @param effects Result of `sp_effect_by_scenario()`.
#' @param costs Data frame (`scenario`, `cost_avg`, `cost_tail`): annual cost in
#'   the entry currency per scenario.
#' @param method Results aggregation method (`"headcount_ratio"`, `"gap"`,
#'   `"mean"`, `"total"`, ...).
#' @param pop People represented (sum of survey weights).
#' @param poverty_line Results poverty line in the entry currency per day, or
#'   `NULL`; poverty-based figures are `"unavailable"` without it.
#' @param level_unit Unit of the metric's level (`metric_metadata()$level_unit`),
#'   to confirm a money-per-day outcome for `"mean"` and `"total"`.
#' @param sp_only Whether the cash transfer is the only lever with an effect. If
#'   not, the effect cannot be credited to the transfer and every row is
#'   `"not_applicable"`.
#' @param min_effect Near-zero threshold, a share of `pop`.
#' @return Data frame, one row per scenario and weather basis: `scenario`,
#'   `basis`, `measure`, `cost`, `amount` (net people lifted, or annual value
#'   in money), `amount_kind`, `value` (the ratio), `lo`, `hi`, `interval`
#'   (`"95% interval"`, `"not bounded"` or `"none"`), `status` (`"ok"`,
#'   `"not_applicable"` or `"unavailable"`) and `note`.
#' @keywords internal
sp_cost_effectiveness <- function(effects, costs, method, pop, poverty_line = NULL,
                                  level_unit = NULL, sp_only = TRUE,
                                  min_effect = SP_CE_MIN_EFFECT) {
  cols <- c(
    "scenario", "basis", "measure", "cost", "amount", "amount_kind", "value",
    "lo", "hi", "interval", "status", "note"
  )
  if (!is.data.frame(effects) || !nrow(effects)) {
    return(stats::setNames(
      data.frame(character(), character(), character(), numeric(), numeric(),
        character(), numeric(), numeric(), numeric(), character(), character(),
        character(), stringsAsFactors = FALSE), cols
    ))
  }
  method <- as.character(method %||% "")[1L]
  pop <- suppressWarnings(as.numeric(pop))[1L]
  line <- suppressWarnings(as.numeric(poverty_line))[1L]
  has_line <- is.finite(line) && line > 0
  per_day <- grepl("per day$", as.character(level_unit %||% ""))[1L]
  z <- stats::qnorm(0.975)

  measure <- switch(method,
    headcount_ratio = "Cost per person lifted out of poverty (net)",
    gap = "Poverty gap closed per $1 of cost",
    mean = "Welfare gain per $1 of cost",
    total = "Welfare gain per $1 of cost",
    "No per-dollar reading"
  )
  per_person <- identical(method, "headcount_ratio")
  # Annual value of a change `e` of the metric, in the entry currency.
  money <- function(e) {
    switch(method,
      gap = -e * line * pop * 365,
      mean = e * pop * 365,
      total = e * 365,
      NA_real_
    )
  }
  # Why a method has no usable reading, or NULL when it has one.
  method_problem <- if (!method %in% c("headcount_ratio", "gap", "mean", "total")) {
    list(status = "not_applicable", note = paste(
      "This metric has no per-dollar reading. Choose the poverty rate, poverty",
      "gap, mean or population sum in Results."
    ))
  } else if (!is.finite(pop) || pop <= 0) {
    list(status = "unavailable", note = "The represented population is not available.")
  } else if (method %in% c("headcount_ratio", "gap") && !has_line) {
    list(status = "unavailable", note = "Needs a positive poverty line.")
  } else if (method %in% c("mean", "total") && !per_day) {
    list(status = "not_applicable", note = paste(
      "The outcome is not a money value per day, so a per-dollar reading is",
      "not defined."
    ))
  } else if (!isTRUE(sp_only)) {
    list(status = "not_applicable", note = paste(
      "The policy effect includes levers other than the cash transfer, so it",
      "cannot be credited to the transfer's cost."
    ))
  }

  one <- function(effect, sd, cost) {
    res <- list(
      amount = NA_real_, value = NA_real_, lo = NA_real_, hi = NA_real_,
      interval = "none", status = "unavailable", note = ""
    )
    if (!is.null(method_problem)) {
      res$status <- method_problem$status
      res$note <- method_problem$note
      return(res)
    }
    if (!is.finite(effect) || !is.finite(cost)) {
      res$note <- "The effect or the cost is not available."
      return(res)
    }
    amount <- if (per_person) -effect * pop else money(effect)
    res$amount <- amount
    if (cost <= 0) {
      res$status <- "not_applicable"
      res$note <- "No spending in this year."
      return(res)
    }
    near_zero <- if (per_person) amount < min_effect * pop else amount <= 0
    if (near_zero) {
      res$status <- "not_applicable"
      res$note <- if (per_person) {
        "The modelled effect does not reduce the number of poor by a meaningful amount."
      } else {
        "The modelled effect does not improve the metric."
      }
      return(res)
    }
    res$status <- "ok"
    res$value <- if (per_person) cost / amount else amount / cost
    if (is.finite(sd) && sd > 0) {
      ends <- effect + c(-1, 1) * z * sd
      if (per_person) {
        lifted <- sort(-ends * pop)
        if (lifted[[1L]] >= min_effect * pop) {
          res$lo <- cost / lifted[[2L]]
          res$hi <- cost / lifted[[1L]]
          res$interval <- "95% interval"
        } else {
          res$interval <- "not bounded"
        }
      } else {
        r <- sort(money(ends) / cost)
        res$lo <- r[[1L]]
        res$hi <- r[[2L]]
        res$interval <- "95% interval"
      }
    }
    res
  }

  pairs <- expand.grid(
    scenario = as.character(effects$scenario),
    basis = c("Average year", "1-in-20 year"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    s <- pairs$scenario[[i]]
    avg <- pairs$basis[[i]] == "Average year"
    e <- effects[match(s, effects$scenario), , drop = FALSE]
    k <- if (is.data.frame(costs)) costs[match(s, as.character(costs$scenario)), , drop = FALSE]
    cost <- if (!is.null(k) && nrow(k)) as.numeric(if (avg) k$cost_avg else k$cost_tail) else NA_real_
    r <- one(
      effect = as.numeric(if (avg) e$effect_avg else e$effect_tail),
      sd = if (avg) as.numeric(e$effect_avg_sd) else NA_real_,
      cost = cost
    )
    data.frame(
      scenario = s, basis = pairs$basis[[i]], measure = measure, cost = cost,
      amount = r$amount,
      amount_kind = if (per_person) "Net people lifted" else "Annual value of the change",
      value = r$value, lo = r$lo, hi = r$hi, interval = r$interval,
      status = r$status, note = r$note, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(match(out$scenario, effects$scenario), out$basis != "Average year"), cols]
}

# Display table for `sp_cost_effectiveness()`: strings, one rule for every
# figure. `prefix` is the entry-currency prefix (`.sp_currency_prefix()`).
.sp_ce_display <- function(res, currency = "PPP") {
  empty <- data.frame(
    Scenario = character(), `Weather year` = character(), `Annual cost` = character(),
    Change = character(), Result = character(), `95% interval` = character(),
    Note = character(), check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!is.data.frame(res) || !nrow(res)) {
    return(empty)
  }
  prefix <- .sp_currency_prefix(currency)
  per_person <- grepl("^Cost per person", res$measure[[1L]])
  state <- function(status) {
    ifelse(status == "unavailable", "Not available", "Not applicable")
  }
  money <- function(x) fmt_num(x, digits = 2, prefix = prefix, na = "Not available")
  ratio <- function(x) fmt_num(x, digits = 2, na = "Not available")
  fmt_ratio <- if (per_person) money else ratio
  ok <- res$status == "ok"
  result <- ifelse(ok, fmt_ratio(res$value), state(res$status))
  interval <- ifelse(
    res$interval == "95% interval",
    paste0(fmt_ratio(res$lo), " to ", fmt_ratio(res$hi)),
    ifelse(res$interval == "not bounded", "Not bounded", "")
  )
  change <- if (per_person) {
    fmt_num(res$amount, digits = 0, na = "Not available")
  } else {
    money(res$amount)
  }
  out <- data.frame(
    Scenario = res$scenario, `Weather year` = res$basis,
    `Annual cost` = money(res$cost), Change = change, Result = result,
    `95% interval` = interval, Note = res$note,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(out)[names(out) == "Change"] <- res$amount_kind[[1L]]
  names(out)[names(out) == "Result"] <- res$measure[[1L]]
  out
}
