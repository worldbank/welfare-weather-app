# Shock-responsive social protection: spec checks and trigger thresholds ----
#
# Pure functions (no Shiny) for the shock-responsive transfer (plan:
# review/sp_shock_responsive_plan.md, decisions 2, 7, 8 and 9; tasks P1-1 and
# P1-2 in review/sp_shock_responsive_tasks.md). Thresholds are computed from the
# historical exposure table of the run and applied unchanged to every climate
# member, as Step 2 does for its return periods.

SP_TRIGGER_TYPES <- c("weather", "return_period")
SP_TRIGGER_DIRECTIONS <- c("above", "below", "either")
SP_PAYOUT_SCOPES <- c("local", "national_triggered", "national_all")
SP_BUDGET_RULES <- c("fixed", "loss_share")
SP_DYN_GEO <- c("all", "worst_share")
SP_TIMINGS <- c("expost", "anticipatory")
# What the trigger measures: a weather variable, or an impact computed from the
# central predictions (P2-5). Impact metrics are read at national level or per
# survey location (interview months pooled) and always fire on high values:
# `loss` is percent of the households' own typical welfare (positive = loss),
# `poverty_rate` percent, `poor_count` survey-weight people.
SP_TRIGGER_METRICS <- c("weather", "loss", "poverty_rate", "poor_count")
SP_TRIGGER_LEVELS <- c("location", "national", "admin1", "admin2")
# Survey columns behind the administrative levels (GAUL 2024 codes)
SP_IMPACT_GEO_COLS <- c(admin1 = "gaul1_code", admin2 = "gaul2_code")
SP_IMPACT_COL <- ".impact_metric"
# A poverty metric at survey-location level needs this many sampled households
# in the location-year, or the rate is a step function (P2-0b)
SP_IMPACT_MIN_HH <- 30L

# A trigger is evaluated per survey round, survey location and interview month.
.sp_trigger_key_cols <- c("code", "year", "survname", "loc_id", "int_month")

# A trigger problem the user can act on (choose another variable or return
# period). Classed so callers can show its message and treat other errors as
# unexpected.
.sp_trigger_abort <- function(message) {
  stop(structure(
    class = c("sp_trigger_problem", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

#' Why a shock-responsive spec cannot run, or NULL when it can
#'
#' Checks the trigger and payout fields of a scenario list from
#' `mod_3_01_sp_server()`. Shock mode pays a fixed amount per activation, so the
#' budget mode is not consulted (decision 12). The budget rule (`"fixed"` or
#' `"loss_share"`, P2-6) and the optional annual cap are checked here too.
#'
#' @param sp Scenario list.
#' @return `NULL` when valid, otherwise one short message.
#' @keywords internal
.sp_shock_problem <- function(sp) {
  num <- function(x) suppressWarnings(as.numeric(x %||% NA_real_))[1L]
  type <- sp$trigger_type %||% "weather"
  if (!type %in% SP_TRIGGER_TYPES) {
    return("Choose a trigger type.")
  }
  metric <- sp[["trigger_metric"]] %||% "weather"
  if (!metric %in% SP_TRIGGER_METRICS) {
    return("Choose what the trigger measures.")
  }
  impact <- !identical(metric, "weather")
  if (impact && !(sp[["trigger_level"]] %||% "location") %in% SP_TRIGGER_LEVELS) {
    return("Choose whether the impact is read nationally or per survey location.")
  }
  var <- sp$trigger_variable %||% NA_character_
  if (!impact && (length(var) != 1L || is.na(var) || !nzchar(var))) {
    return("Choose a weather variable for the trigger.")
  }
  direction <- if (impact) "above" else sp$trigger_direction %||% "above"
  if (!direction %in% SP_TRIGGER_DIRECTIONS) {
    return("Choose whether the trigger fires above, below or at either extreme.")
  }
  if (identical(type, "weather")) {
    if (identical(direction, "either")) {
      if (!is.finite(num(sp$trigger_value)) || !is.finite(num(sp$trigger_value_low))) {
        return("Enter a low and a high threshold value for the trigger.")
      }
      if (num(sp$trigger_value_low) > num(sp$trigger_value)) {
        return("The low threshold must not exceed the high threshold.")
      }
    } else if (!is.finite(num(sp$trigger_value))) {
      return("Enter a threshold value for the trigger.")
    }
  } else {
    n <- num(sp$trigger_return_period_years)
    if (!is.finite(n) || n < 1) {
      return("Enter a return period of at least 1 year.")
    }
  }
  if (!(sp$payout_scope %||% "local") %in% SP_PAYOUT_SCOPES) {
    return("Choose a payout scope.")
  }
  scope <- sp$payout_scope %||% "local"
  if (!identical(scope, "local")) {
    k <- num(sp$national_k_pct %||% 0)
    if (!is.finite(k) || k < 0 || k > 100) {
      return("The national gate share must be between 0 and 100 percent.")
    }
    # "Any location fires, pay everyone" is not offered (decision 10)
    if (identical(scope, "national_all") && k <= 0) {
      return("Paying all targeted households needs a national gate share above 0.")
    }
  }
  rule <- sp$budget_rule %||% "fixed"
  if (!rule %in% SP_BUDGET_RULES) {
    return("Choose a budget rule.")
  }
  if (identical(rule, "loss_share")) {
    s <- num(sp$loss_share_pct)
    if (!is.finite(s) || s <= 0 || s > 100) {
      return("Enter a share of modelled loss between 0 and 100 percent.")
    }
  }
  # A blank annual cap means no cap
  cap <- num(sp$annual_cap)
  if (!is.na(cap) && (!is.finite(cap) || cap <= 0)) {
    return("The annual cap must be above 0, or left blank for no cap.")
  }
  .sp_dyn_problem(sp, scope) %||% .sp_timing_problem(sp)
}

# Timing fields of a shock spec (P2-7). `transfer_timing` is "expost" (default,
# also for a missing value) or "anticipatory"; the forecast skill (`forecast_pod_pct`,
# `forecast_far_pct`) and the multiplier `effect_multiplier` belong to the
# anticipatory design, the delay (`timeliness_weeks`, `delay_loss_pct_per_week`)
# to the ex-post design. Absent fields take the neutral values.
.sp_timing_problem <- function(sp) {
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x %||% default))[1L]
    if (is.na(v)) default else v
  }
  timing <- sp[["transfer_timing"]] %||% "expost"
  if (is.na(timing)) timing <- "expost"
  if (!timing %in% SP_TIMINGS) {
    return("Choose ex post or anticipatory payments.")
  }
  pod <- num(sp[["forecast_pod_pct"]], 100)
  far <- num(sp[["forecast_far_pct"]], 0)
  if (!is.finite(pod) || pod < 0 || pod > 100 || !is.finite(far) || far < 0 || far > 100) {
    return("Forecast detection and false-alarm rates must be between 0 and 100 percent.")
  }
  m <- num(sp[["effect_multiplier"]], 1)
  if (!is.finite(m) || m <= 0 || m > 10) {
    return("The effectiveness multiplier must be above 0 and at most 10.")
  }
  weeks <- num(sp[["timeliness_weeks"]], 0)
  loss <- num(sp[["delay_loss_pct_per_week"]], 0)
  if (!is.finite(weeks) || weeks < 0 || weeks > 52 || !is.finite(loss) || loss < 0 || loss > 100) {
    return("The delay must be 0 to 52 weeks and the loss per week 0 to 100 percent.")
  }
  NULL
}

# Checks of the dynamic targeting layers (P2-8); NULL when valid or both off.
.sp_dyn_problem <- function(sp, scope) {
  num <- function(x) suppressWarnings(as.numeric(x %||% NA_real_))[1L]
  geo <- sp[["dyn_geo"]] %||% "all"
  if (!geo %in% SP_DYN_GEO) {
    return("Choose where payments go within the triggered locations.")
  }
  if (identical(geo, "worst_share")) {
    p <- num(sp$dyn_geo_pct)
    if (!is.finite(p) || p <= 0 || p > 100) {
      return("Enter a worst-affected share between 0 and 100 percent.")
    }
    if (identical(scope, "national_all")) {
      return("Paying all targeted households cannot be limited to the worst-affected locations.")
    }
  }
  if (isTRUE(sp[["dyn_welfare"]])) {
    p <- num(sp$dyn_welfare_pct)
    if (!is.finite(p) || p <= 0 || p >= 100) {
      return("Enter a predicted-welfare cutoff between 0 and 100 percent.")
    }
    for (f in c("dyn_incl_error_pct", "dyn_excl_error_pct")) {
      e <- num(sp[[f]] %||% 0)
      if (!is.finite(e) || e < 0 || e > 100) {
        return("Dynamic targeting error rates must be between 0 and 100 percent.")
      }
    }
  }
  NULL
}

#' Weather columns that can drive a trigger
#'
#' A trigger needs a continuous value. Binned variables reach the exposure table
#' as bin labels (character or factor), which do not qualify; only numeric
#' columns do, among them the continuous companion `<var>_cont` that Step 2 keeps
#' for each binned variable (P2-4). The unit is whatever the variable's
#' transformation implies (physical units, deviation or standardized anomaly).
#'
#' @param exposure Exposure table (`.policy_exposure_table()` layout).
#' @return Character vector of column names.
#' @keywords internal
.sp_trigger_variables <- function(exposure) {
  if (!is.data.frame(exposure)) {
    return(character())
  }
  cols <- setdiff(names(exposure), .policy_exposure_key_cols)
  cols[vapply(cols, function(v) is.numeric(exposure[[v]]), logical(1))]
}

#' Unit of a trigger variable as the model sees it
#'
#' The exposure table holds the value after the Step 2 transformation, so a
#' threshold is read in that unit: the physical unit, a deviation from the
#' reference mean (same unit) or a standardized anomaly (z-score).
#'
#' @param variable Weather variable name.
#' @param selected_weather Data frame with `name`, and optionally `units` and
#'   `transformation` (as `build_selected_weather()`).
#' @return One string, or `""` when nothing is known about the variable.
#' @keywords internal
.sp_trigger_unit <- function(variable, selected_weather) {
  if (!is.data.frame(selected_weather) || !length(variable) ||
    !"name" %in% names(selected_weather)) {
    return("")
  }
  row <- which(selected_weather$name == variable)[1L]
  if (is.na(row) && grepl("_cont$", variable)) {
    # The continuous companion of a binned variable has the variable's unit
    row <- which(selected_weather$name == sub("_cont$", "", variable))[1L]
  }
  if (is.na(row)) {
    return("")
  }
  one <- function(col) {
    x <- if (col %in% names(selected_weather)) selected_weather[[col]][row] else NA
    x <- as.character(x)
    if (is.na(x) || !nzchar(x)) "" else x
  }
  units <- one("units")
  tf <- one("transformation")
  if (identical(tf, "Standardized anomaly")) {
    "standardized anomaly (z-score)"
  } else if (identical(tf, "Deviation from mean")) {
    paste0("deviation from mean", if (nzchar(units)) paste0(", ", units) else "")
  } else {
    units
  }
}

#' Trigger thresholds from the historical exposure
#'
#' `"weather"` returns the common value the user entered. `"return_period"`
#' returns one threshold per (survey round, location, interview month) at the
#' 1-in-N level of that cell's historical record, on the adverse side given by
#' `trigger_direction`: `above` uses the `1 - 1/N` quantile and fires at or above
#' it, `below` uses the `1/N` quantile and fires at or below it (the rule of
#' `step2_adverse_return_period()`). Following Step 2, a 1-in-N trigger is
#' offered only when the historical record has at least N finite years; cells
#' with a shorter record get no threshold (`NA`, never fires) and the call stops
#' when no cell supports N.
#'
#' The exposure table holds one value per simulation year in each cell, so the
#' number of finite values is the record length behind the threshold.
#'
#' @param hist_exposure Exposure table of the historical pipeline.
#' @param spec Scenario list with the trigger fields.
#' @return A list with `type`, `variable`, `direction`, and either `value`
#'   (weather) or `table` (return period: key columns, `threshold`, `n_years`,
#'   `supported`) plus `return_period_years`.
#' @keywords internal
sp_trigger_thresholds <- function(hist_exposure, spec) {
  problem <- .sp_shock_problem(spec)
  if (!is.null(problem)) {
    .sp_trigger_abort(problem)
  }
  type <- spec$trigger_type %||% "weather"
  trig <- .sp_spec_trigger(spec)
  var <- trig$variable
  direction <- trig$direction
  if (!var %in% .sp_trigger_variables(hist_exposure)) {
    .sp_trigger_abort(paste0(
      "The trigger variable '", var, "' is not a continuous weather variable ",
      "in the exposure table (a binned variable is used through its continuous ",
      "companion; re-run Step 2 if it is missing)."
    ))
  }
  if (identical(type, "weather")) {
    return(list(
      type = type, variable = var, direction = direction,
      value = as.numeric(spec$trigger_value),
      value_low = if (identical(direction, "either")) as.numeric(spec$trigger_value_low)
    ))
  }

  n_req <- as.numeric(spec$trigger_return_period_years)
  # "Either extreme" splits the 1-in-N probability over two tails, so each tail
  # is a 1-in-2N level and needs a record of 2N years (the Step 2 rule applied
  # to the tail level).
  either <- identical(direction, "either")
  n_tail <- if (either) 2 * n_req else n_req
  keys <- intersect(.sp_trigger_key_cols, names(hist_exposure))
  if (!all(c("loc_id", "int_month") %in% keys)) {
    stop("The exposure table has no location or interview month.", call. = FALSE)
  }
  grp <- do.call(paste, c(unname(as.list(hist_exposure[keys])), sep = "\r"))
  first <- !duplicated(grp)
  vals <- split(as.numeric(hist_exposure[[var]]), factor(grp, levels = grp[first]))
  n_years <- vapply(vals, function(v) sum(is.finite(v)), integer(1))
  tbl <- hist_exposure[first, keys, drop = FALSE]
  rownames(tbl) <- NULL
  tbl$n_years <- unname(n_years)
  tbl$supported <- tbl$n_years >= ceiling(n_tail)
  tbl$threshold <- NA_real_
  if (either) tbl$threshold_low <- NA_real_
  ok <- which(tbl$supported)
  quant <- function(v, p) unname(stats::quantile(v[is.finite(v)], probs = p, names = FALSE))
  if (!identical(direction, "below")) {
    tbl$threshold[ok] <- vapply(vals[ok], quant, numeric(1), p = 1 - 1 / n_tail)
  }
  if (identical(direction, "below")) {
    tbl$threshold[ok] <- vapply(vals[ok], quant, numeric(1), p = 1 / n_tail)
  }
  if (either) {
    tbl$threshold_low[ok] <- vapply(vals[ok], quant, numeric(1), p = 1 / n_tail)
  }
  if (identical(var, SP_IMPACT_COL)) {
    # A discrete impact (a poverty rate of 0 in most years) can have a threshold
    # equal to its historical minimum, which would fire in every year: such a
    # cell has no adverse tail and never fires
    lowest <- vapply(vals, function(v) suppressWarnings(min(v[is.finite(v)])), numeric(1))
    flat <- tbl$supported & tbl$threshold <= unname(lowest)
    tbl$supported[flat] <- FALSE
    tbl$threshold[flat] <- NA_real_
    if (any(flat) && !any(tbl$supported)) {
      .sp_trigger_abort(paste0(
        "The 1-in-", format(n_req), " level of the impact metric equals its lowest ",
        "historical value everywhere, so there is no adverse tail to trigger on. ",
        "Use a longer return period or a threshold value."
      ))
    }
  }
  if (!any(tbl$supported)) {
    .sp_trigger_abort(paste0(
      "A 1-in-", format(n_req), if (either) " trigger at either extreme" else " trigger",
      " needs at least ", ceiling(n_tail), " historical years; the longest record has ",
      max(tbl$n_years, 0L), "."
    ))
  }
  list(
    type = type, variable = var, direction = direction,
    return_period_years = n_req, table = tbl
  )
}

#' Trigger state of one pipeline
#'
#' One pre-pass per pipeline (a climate member or the historical arm), before
#' any chunk loop: which prediction rows exceed the trigger, the survey-weighted
#' share of the population exposed in each simulation year, whether the
#' national gate fires, and which rows fall inside the payout scope. Rows are
#' evaluated at their own (survey round, location, interview month), so two
#' households of one location interviewed in different months can differ in a
#' year (decision Q4). Rows with no finite value or no supported threshold do
#' not exceed.
#'
#' Scopes: `local` pays rows that exceed; `national_triggered` pays rows that
#' exceed in a year where the gate fired; `national_all` pays every row in a
#' year where the gate fired. The gate fires in a year when the exposed
#' population share is above 0 and at least `national_k_pct` percent, so
#' `k = 0` makes `national_triggered` identical to `local`.
#'
#' @param exposure Data frame with one row per prediction row: `sim_year`, the
#'   trigger variable and, for return-period triggers, the key columns of
#'   `thresholds$table`.
#' @param thresholds Result of `sp_trigger_thresholds()`.
#' @param weights Numeric weights per row (people), or NULL for equal weights.
#'   Missing or negative weights count as 0.
#' @param spec Scenario list with `payout_scope` and `national_k_pct`.
#' @return A list: `exceeds` and `in_scope` (logical per row), `share`
#'   (exposed population share by simulation year, named by year) and `gate`
#'   (logical by simulation year; always FALSE for `local`).
#' @keywords internal
sp_trigger_state <- function(exposure, thresholds, weights = NULL, spec) {
  if (!"sim_year" %in% names(exposure)) {
    stop("The exposure needs `sim_year` and the trigger variable.", call. = FALSE)
  }
  .sp_trigger_scope(
    .sp_trigger_exceeds(exposure, thresholds), exposure$sim_year, weights, spec
  )
}

# Trigger limits per exposure row: the common value (weather level) or the
# cell's threshold (return period), and the low limit for "either end".
.sp_trigger_limits <- function(exposure, thresholds) {
  n <- nrow(exposure)
  either <- identical(thresholds$direction, "either")
  low <- NULL
  limit <- if (identical(thresholds$type, "weather")) {
    if (either) low <- rep(thresholds$value_low, n)
    rep(thresholds$value, n)
  } else {
    keys <- intersect(.sp_trigger_key_cols, names(thresholds$table))
    if (!all(keys %in% names(exposure))) {
      stop("The exposure lacks the threshold key columns.", call. = FALSE)
    }
    th <- .sp_key_chr(thresholds$table, keys)
    th$threshold <- thresholds$table$threshold
    if (either) th$threshold_low <- thresholds$table$threshold_low
    joined <- dplyr::left_join(.sp_key_chr(exposure, keys), th, by = keys)
    if (either) low <- joined$threshold_low
    joined$threshold
  }
  list(limit = limit, low = low)
}

# Key columns as character, for joins between exposure and threshold tables.
.sp_key_chr <- function(df, keys) {
  df <- as.data.frame(df)[keys]
  df[] <- lapply(df, as.character)
  df
}

# Which exposure rows pass the trigger. Works on any frame that carries the
# trigger variable (and the threshold keys), so a pipeline can evaluate its
# small exposure table once and index the result by prediction row.
.sp_trigger_exceeds <- function(exposure, thresholds) {
  if (!thresholds$variable %in% names(exposure)) {
    stop("The exposure needs `sim_year` and the trigger variable.", call. = FALSE)
  }
  value <- as.numeric(exposure[[thresholds$variable]])
  lim <- .sp_trigger_limits(exposure, thresholds)
  exceeds <- if (identical(thresholds$direction, "either")) {
    value >= lim$limit | value <= lim$low
  } else if (identical(thresholds$direction, "above")) {
    value >= lim$limit
  } else {
    value <= lim$limit
  }
  exceeds[is.na(exceeds)] <- FALSE
  exceeds
}

# Exposed share, national gate and payout scope from per-row exceedance.
.sp_trigger_scope <- function(exceeds, year, weights, spec) {
  scope <- spec$payout_scope %||% "local"
  if (!scope %in% SP_PAYOUT_SCOPES) {
    stop("Unknown payout scope '", scope, "'.", call. = FALSE)
  }
  w <- if (is.null(weights)) rep(1, length(exceeds)) else as.numeric(weights)
  w[!is.finite(w) | w < 0] <- 0
  total <- rowsum(w, year, reorder = TRUE)[, 1L]
  hit <- rowsum(w * exceeds, year, reorder = TRUE)[, 1L]
  share <- ifelse(total > 0, hit / total, 0)

  k <- as.numeric(spec$national_k_pct %||% 0) / 100
  gate <- if (identical(scope, "local")) {
    stats::setNames(rep(FALSE, length(share)), names(share))
  } else {
    share > 0 & share >= k
  }
  gate_row <- unname(gate[as.character(year)])
  in_scope <- switch(scope,
    local = exceeds,
    national_triggered = gate_row & exceeds,
    national_all = gate_row
  )
  list(exceeds = exceeds, in_scope = in_scope, share = share, gate = gate)
}

#' Dynamic transfer per prediction row
#'
#' The daily-equivalent transfer a prediction row receives: the static
#' program's per-household amount (`transfer_amount_usd` times
#' `payments_per_activation`, over 365) for statically eligible households,
#' multiplied by the trigger's payout scope. The per-household arithmetic is
#' `.sp_transfer_values()` itself (household size, amount basis, currency), so a
#' trigger that always fires reproduces the regular program's transfer exactly.
#' Administration cost is not part of the transfer; it is a cost only.
#'
#' The result is on the stored welfare scale (2021 PPP), like `SP_TRANSFER_COL`.
#' Convert it to the outcome's scale with `outcome_level_scale()` before adding
#' it to a prediction.
#'
#' @param state Result of `sp_trigger_state()` for the pipeline.
#' @param eligibility Logical per survey row (`SP_ELIGIBLE_COL`).
#' @param spec Scenario list.
#' @param svy Survey frame the eligibility refers to (`welfare`, `hhsize`,
#'   `ppp2021` as the regular program needs them).
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @param svy_row_id Integer survey row of each prediction row, aligned with
#'   `state$in_scope`.
#' @return Numeric vector, one value per prediction row.
#' @keywords internal
sp_dynamic_transfer <- function(state, eligibility, spec, svy, analysis_unit = "hh",
                                svy_row_id) {
  if (length(eligibility) != nrow(svy)) {
    stop("Eligibility must have one value per survey row.", call. = FALSE)
  }
  ids <- as.integer(svy_row_id)
  if (length(ids) != length(state$in_scope) || anyNA(ids) ||
    any(ids < 1L | ids > nrow(svy))) {
    stop("Prediction rows must map to survey rows.", call. = FALSE)
  }
  .sp_dynamic_per_household(eligibility, spec, svy, analysis_unit)[ids] * state$in_scope
}

# Daily transfer each survey row receives when the program pays it: the regular
# program's transfer-first arithmetic with payments per activation. Zero for
# rows that are not statically eligible.
.sp_dynamic_per_household <- function(eligibility, spec, svy, analysis_unit) {
  if (length(eligibility) != nrow(svy)) {
    stop("Eligibility must have one value per survey row.", call. = FALSE)
  }
  per_household <- .sp_transfer_values(
    svy,
    utils::modifyList(spec, list(
      budget_mode = "transfer_first",
      transfer_n_payments = spec$payments_per_activation %||% 1L
    )),
    analysis_unit, !is.na(eligibility) & eligibility
  )
  if (is.null(per_household)) {
    stop("The survey cannot support a transfer (no welfare column).", call. = FALSE)
  }
  per_household
}

#' Run-level plan for a shock-responsive program
#'
#' Everything a pipeline needs to evaluate the program, computed once per run:
#' the trigger thresholds from the historical exposure and the per-survey-row
#' daily transfer of eligible households. Plain data, so it can be captured by
#' any process that runs the correction.
#'
#' @param spec Scenario list with `sp_type == "shock"`.
#' @param hist_exposure Exposure table of the historical pipeline.
#' @param eligibility Logical per survey row (`SP_ELIGIBLE_COL`).
#' @param svy Survey frame of the run (see `sp_dynamic_transfer()`).
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @param hist_pipeline,is_log,level_factor Historical pipeline, whether the
#'   outcome is logged and the outcome level scale (modelled-loss reference).
#' @param seed Base seed of the run (dynamic targeting draws).
#' @param hist_exposure_map,pov_line Resolved exposure of the historical
#'   pipeline (`table`, `row_index`) and the Step 2 poverty line; needed by an
#'   impact trigger (`trigger_metric` other than `"weather"`).
#' @return A list with `spec` (trigger and scope fields), `thresholds` and
#'   `per_household`.
#' @keywords internal
sp_shock_plan <- function(spec, hist_exposure, eligibility, svy, analysis_unit = "hh",
                          hist_pipeline = NULL, is_log = FALSE, level_factor = NULL,
                          seed = WISEAPP_DEFAULT_SEED, hist_exposure_map = NULL,
                          pov_line = NULL) {
  loss_pct <- suppressWarnings(as.numeric(spec$loss_event_pct %||% 10))[1L]
  admin_share <- .sp_admin_share(spec)
  cost_frame <- as.data.frame(svy)[intersect(c("weight", "hhsize", "ppp2021"), names(svy))]
  cap <- suppressWarnings(as.numeric(spec$annual_cap %||% NA_real_))[1L]
  rule <- spec$budget_rule %||% "fixed"
  reference <- if (is.null(hist_pipeline)) NULL else {
    sp_shock_reference(hist_pipeline, nrow(svy), is_log)
  }
  # Impact trigger (P2-5): the metric of the historical pipeline becomes a
  # column of the exposure table, so thresholds, record rule, state and the
  # severity ranking are the weather trigger's own code
  impact <- .sp_impact_plan(
    spec, hist_pipeline, hist_exposure_map, reference, is_log, pov_line, svy
  )
  if (!is.null(impact)) {
    hist_exposure <- as.data.frame(hist_exposure)
    hist_exposure[[SP_IMPACT_COL]] <- impact$hist_column
    impact$n_years <- length(unique(
      hist_pipeline$sim_year[is.finite(impact$hist_column[hist_exposure_map$row_index])]
    ))
    impact$hist_column <- NULL
  }
  thresholds <- sp_trigger_thresholds(hist_exposure, spec)
  list(
    spec = spec[intersect(names(spec), c(
      "trigger_type", "trigger_variable", "trigger_direction", "trigger_value",
      "trigger_value_low",
      "trigger_return_period_years", "payout_scope", "national_k_pct"
    ))],
    thresholds = thresholds,
    impact = impact,
    # Layered dynamic targeting (P2-8); NULL leaves the Phase 1 payout scope
    dyn = .sp_dyn_plan(spec, hist_exposure, thresholds, reference, cost_frame, seed),
    # Timing (P2-7); NULL for an ex-post design without delay
    timing = .sp_timing_plan(spec, hist_exposure, seed),
    per_household = .sp_dynamic_per_household(eligibility, spec, svy, analysis_unit),
    # Run outputs (P1-7): cost arithmetic and the modelled-loss reference
    cost_frame = cost_frame,
    analysis_unit = analysis_unit,
    currency = spec$currency %||% "PPP",
    admin_share = admin_share,
    # Budget (P2-6). Under "loss_share" a triggered cell's budget is
    # `loss_share` times the modelled loss of its paid households, split by the
    # amount basis (`amount_scale`); the fixed amount is then not used. The
    # annual cap is on total cost, so the transfer part is `cap * (1 - admin)`.
    budget_rule = rule,
    loss_share = suppressWarnings(as.numeric(spec$loss_share_pct %||% NA_real_))[1L] / 100,
    cap_transfer = if (is.finite(cap) && cap > 0) cap * (1 - admin_share) else NA_real_,
    cost_weight = .sp_shock_cost_weights(cost_frame, analysis_unit, spec$currency %||% "PPP"),
    eligible = !is.na(eligibility) & eligibility,
    amount_scale = .sp_shock_amount_scale(svy, spec, analysis_unit),
    level_factor = level_factor,
    loss_event_pct = if (is.finite(loss_pct) && loss_pct > 0) loss_pct else 10,
    is_log = isTRUE(is_log),
    reference = reference
  )
}

# Impact levels a survey frame supports, as select choices: national always;
# an administrative level when its GAUL column has at least two distinct
# non-missing codes; the survey location when `loc_id` is present. Without a
# frame, every level is offered (the plan reports a missing column).
.sp_impact_level_choices <- function(svy) {
  all <- c(
    "National" = "national", "Administrative level 1" = "admin1",
    "Administrative level 2" = "admin2", "Survey location" = "location"
  )
  if (!is.data.frame(svy)) {
    return(all)
  }
  varies <- function(col) col %in% names(svy) && length(unique(stats::na.omit(svy[[col]]))) > 1L
  keep <- c(
    TRUE, varies(SP_IMPACT_GEO_COLS[["admin1"]]), varies(SP_IMPACT_GEO_COLS[["admin2"]]),
    varies("loc_id")
  )
  all[keep]
}

# Trigger variable and direction a spec evaluates: the chosen weather variable
# and direction, or the impact column, which always fires on high values.
.sp_spec_trigger <- function(spec) {
  if (!identical(spec[["trigger_metric"]] %||% "weather", "weather")) {
    list(variable = SP_IMPACT_COL, direction = "above")
  } else {
    list(variable = spec$trigger_variable, direction = spec$trigger_direction %||% "above")
  }
}

# Plan part of an impact trigger (P2-5); NULL for a weather trigger. Everything
# the metric needs besides the pipeline: the household reference (rule 7), the
# Step 2 poverty line (rule 15: fixed per run, "not available" without one) and
# the level. `hist_column` is the metric of the historical pipeline, one value
# per exposure table row, for the thresholds.
.sp_impact_plan <- function(spec, hist_pipeline, hist_exposure_map, reference, is_log, pov_line,
                            svy = NULL) {
  metric <- spec[["trigger_metric"]] %||% "weather"
  if (identical(metric, "weather")) {
    return(NULL)
  }
  poverty <- metric %in% c("poverty_rate", "poor_count")
  line <- suppressWarnings(as.numeric(pov_line))[1L]
  if (poverty && !(is.finite(line) && line > 0)) {
    .sp_trigger_abort("A trigger on poverty is not available: Step 2 has no poverty line.")
  }
  if (is.null(reference) || is.null(hist_exposure_map)) {
    .sp_trigger_abort("An impact trigger needs the historical predictions and exposure.")
  }
  level <- spec[["trigger_level"]] %||% "location"
  geo <- NULL
  if (level %in% names(SP_IMPACT_GEO_COLS)) {
    col_name <- SP_IMPACT_GEO_COLS[[level]]
    if (!is.data.frame(svy) || !col_name %in% names(svy)) {
      .sp_trigger_abort(paste0(
        "The survey data of this run has no '", col_name, "' (",
        sub("admin", "administrative level ", level),
        "). Use another level."
      ))
    }
    geo <- as.character(svy[[col_name]])
  }
  cfg <- list(
    metric = metric, level = level, geo = geo,
    pov_line = if (poverty) line, is_log = isTRUE(is_log)
  )
  col <- .sp_impact_column(cfg, hist_pipeline, hist_exposure_map, reference)
  if (!any(is.finite(col))) {
    .sp_trigger_abort(if (poverty && !identical(level, "national")) {
      paste0(
        "No ", if (identical(level, "location")) "survey location" else "area",
        " has ", SP_IMPACT_MIN_HH, " sampled households, ",
        "too few to read a poverty rate. Use a larger area or the national level."
      )
    } else {
      "The impact metric has no finite value in the historical record."
    })
  }
  c(cfg, list(hist_column = col))
}

# The impact metric of one pipeline, one value per exposure table row (`NA` for
# rows no prediction row refers to). Per (unit, simulation year), where the unit
# is the whole sample, a survey location (interview months pooled) or an
# administrative area (`gaul1_code`, `gaul2_code`): `loss` is minus the survey-weighted mean of level / reference - 1 in percent (rule 7: level
# scale, no residual draws), `poverty_rate` the weighted percent of households
# below the line, `poor_count` their summed weight. Poverty below national level is
# `NA` below `SP_IMPACT_MIN_HH` sampled households. Sums are keyed by unit and
# year, so the result does not depend on row order.
.sp_impact_column <- function(cfg, pipeline, exposure, reference) {
  tbl <- exposure$table
  g <- as.integer(exposure$row_index)
  ids <- as.integer(pipeline$svy_row_id)
  w <- if (is.null(pipeline$weight)) rep(1, length(ids)) else as.numeric(pipeline$weight)
  w[!is.finite(w) | w < 0] <- 0
  y <- as.numeric(pipeline$y_point)
  level <- if (isTRUE(cfg$is_log)) exp(y) else y
  rel <- level / reference[ids] - 1
  ok <- is.finite(rel) & w > 0
  poverty <- cfg$metric %in% c("poverty_rate", "poor_count")
  if (poverty) {
    ok <- ok & is.finite(level)
  }
  unit <- if (identical(cfg$level, "national")) {
    rep("all", length(g))
  } else if (!is.null(cfg$geo)) {
    cfg$geo[ids]
  } else {
    keys <- intersect(c("code", "year", "survname", "loc_id"), names(tbl))
    do.call(paste, c(unname(lapply(tbl[keys], as.character)), list(sep = "\r")))[g]
  }
  key <- paste(unit, as.character(pipeline$sim_year), sep = "\n")
  val <- cbind(
    w = w, wr = w * ifelse(ok, rel, 0),
    wp = if (poverty) w * ifelse(ok & level < cfg$pov_line, 1, 0) else 0,
    n = 1
  )
  val[!ok, ] <- 0
  sums <- rowsum(val, key)
  metric <- switch(cfg$metric,
    loss = -100 * sums[, "wr"] / sums[, "w"],
    poverty_rate = 100 * sums[, "wp"] / sums[, "w"],
    poor_count = sums[, "wp"]
  )
  metric[!is.finite(metric) | sums[, "w"] <= 0] <- NA_real_
  if (poverty && !identical(cfg$level, "national")) {
    metric[sums[, "n"] < SP_IMPACT_MIN_HH] <- NA_real_
  }
  out <- rep(NA_real_, nrow(tbl))
  rows <- unique(g)
  first <- match(rows, g)
  out[rows] <- unname(metric[match(key[first], rownames(sums))])
  out
}

# Plan part of the timing design (P2-7); NULL when the design is ex post with no
# delay, so the Phase 1 state is untouched.
#
# Anticipatory: the program pays on a forecast of the trigger. In a cell-year
# where the realised weather passes the trigger (the event) the forecast fires
# with probability `pod`; where it does not, with probability `far` (false
# alarm). `m` multiplies the welfare effect of a payment made before an event
# that occurs (a hit); a false alarm pays cash at m = 1; cost is never scaled.
# Ex post: `delay` is the effectiveness left after `timeliness_weeks` of delay at
# `delay_loss_pct_per_week` lost each week, applied to welfare only. The
# forecast draw is keyed by (cell, simulation year) and shared by all members.
.sp_timing_plan <- function(spec, hist_exposure, seed) {
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x %||% default))[1L]
    if (is.na(v)) default else v
  }
  timing <- spec[["transfer_timing"]] %||% "expost"
  if (is.na(timing)) timing <- "expost"
  anticipatory <- identical(timing, "anticipatory")
  expost_delay <- max(
    0, 1 - num(spec[["timeliness_weeks"]], 0) * num(spec[["delay_loss_pct_per_week"]], 0) / 100
  )
  if (!anticipatory && expost_delay == 1) {
    return(NULL)
  }
  list(
    anticipatory = anticipatory, pod = num(spec[["forecast_pod_pct"]], 100) / 100,
    far = num(spec[["forecast_far_pct"]], 0) / 100,
    m = if (anticipatory) num(spec[["effect_multiplier"]], 1) else 1,
    # Effectiveness of the payments made, and of the ex-post comparator
    delay = if (anticipatory) 1 else expost_delay, expost_delay = expost_delay,
    seed = seed,
    cells = if (anticipatory) sort(unique(.sp_cell_key(hist_exposure)))
  )
}

# One string per exposure row naming its (survey round, location, interview
# month) cell.
.sp_cell_key <- function(tbl) {
  keys <- intersect(.sp_trigger_key_cols, names(tbl))
  do.call(paste, c(unname(lapply(as.data.frame(tbl)[keys], as.character)), list(sep = "\r")))
}

# Forecast of the trigger per exposure table row: `event` (realised trigger) is
# fired with probability pod, a non-event with probability far. One uniform per
# (cell, simulation year) from `wise_seed(seed, "policy", "sp-forecast", year)`;
# rows no prediction row refers to stay FALSE. A cell that is not in the
# historical table follows the realised trigger.
.sp_forecast_fire <- function(timing, tbl, g, year, event) {
  rows <- unique(g)
  first <- match(rows, g)
  cell <- match(.sp_cell_key(tbl[rows, , drop = FALSE]), timing$cells)
  yr <- as.character(year[first])
  u <- rep(NA_real_, length(rows))
  for (idx in split(seq_along(rows), yr)) {
    draw <- withr::with_seed(
      wise_seed(timing$seed, "policy", "sp-forecast", yr[idx[1L]]),
      stats::runif(length(timing$cells))
    )
    u[idx] <- draw[cell[idx]]
  }
  fire <- ifelse(event[rows], u < timing$pod, u < timing$far)
  fire[is.na(u)] <- event[rows][is.na(u)]
  out <- rep(FALSE, nrow(tbl))
  out[rows] <- fire
  out
}

# Welfare effectiveness of each prediction row's payment (P2-7): `m` on a hit of
# an anticipatory design, the delay multiplier of an ex-post design, 1 else.
# `NULL` when every multiplier is 1.
.sp_timing_effect <- function(timing, hit) {
  eff <- if (timing$anticipatory) {
    ifelse(hit, timing$m, 1)
  } else {
    rep(timing$delay, length(hit))
  }
  if (all(eff == 1)) NULL else eff
}

# Weighted value of the payments of an anticipatory design by simulation year:
# `hit_value` (the realised trigger passes in the payee's cell), `false_value`
# (it does not) and `expost_value` (what the same trigger, scope, layers and
# budget rule pay after the ex-post delay). Stored scale, weighted by the
# prediction weights.
.sp_timing_values <- function(plan, pipeline, exposure, state, transfer, years) {
  w <- if (is.null(pipeline$weight)) rep(1, length(transfer)) else as.numeric(pipeline$weight)
  w[!is.finite(w) | w < 0] <- 0
  pay <- ifelse(is.finite(transfer), transfer, 0)
  plan_ex <- plan
  plan_ex$timing$anticipatory <- FALSE
  plan_ex$timing$delay <- plan$timing$expost_delay
  state_ex <- sp_shock_pipeline_state(plan_ex, pipeline, exposure)
  pay_ex <- sp_shock_pipeline_transfer(plan_ex, pipeline, exposure, state_ex)
  pay_ex <- ifelse(is.finite(pay_ex), pay_ex, 0) * plan$timing$expost_delay
  by_year <- rowsum(cbind(
    hit_value = w * pay * state$hit, false_value = w * pay * !state$hit,
    expost_value = w * pay_ex
  ), pipeline$sim_year)
  by_year[match(as.character(years), rownames(by_year)), , drop = FALSE]
}

# Plan part of the layered dynamic targeting (P2-8): NULL when both layers are
# off, so the Phase 1 state is untouched. `geo_pct` is the worst-affected share
# of the triggered population; `scale` holds each cell's historical standard
# deviation of the trigger variable (the unit of the severity ranking).
# `welfare_cut` is the weighted `dyn_welfare_pct` quantile of the households'
# own historical mean outcome level, fixed per run.
.sp_dyn_plan <- function(spec, hist_exposure, thresholds, reference, cost_frame, seed) {
  num <- function(x) suppressWarnings(as.numeric(x))[1L]
  geo <- identical(spec[["dyn_geo"]] %||% "all", "worst_share")
  welfare <- isTRUE(spec[["dyn_welfare"]])
  if (!geo && !welfare) {
    return(NULL)
  }
  if (welfare && is.null(reference)) {
    .sp_trigger_abort("Predicted-welfare targeting needs the historical predictions.")
  }
  list(
    geo_pct = if (geo) num(spec$dyn_geo_pct) / 100,
    scale = if (geo) .sp_trigger_cell_sd(hist_exposure, thresholds$variable),
    welfare_cut = if (welfare) {
      .sp_welfare_quantile(reference, cost_frame$weight, num(spec$dyn_welfare_pct) / 100)
    },
    incl = if (welfare) num(spec$dyn_incl_error_pct %||% 0) / 100,
    excl = if (welfare) num(spec$dyn_excl_error_pct %||% 0) / 100,
    seed = seed
  )
}

# Historical standard deviation of the trigger variable per (survey round,
# location, interview month) cell; `NA` with fewer than two finite years.
.sp_trigger_cell_sd <- function(hist_exposure, variable) {
  keys <- intersect(.sp_trigger_key_cols, names(hist_exposure))
  grp <- do.call(paste, c(unname(as.list(hist_exposure[keys])), sep = "\r"))
  first <- !duplicated(grp)
  vals <- split(as.numeric(hist_exposure[[variable]]), factor(grp, levels = grp[first]))
  tbl <- hist_exposure[first, keys, drop = FALSE]
  rownames(tbl) <- NULL
  tbl$sd <- unname(vapply(vals, function(v) {
    v <- v[is.finite(v)]
    if (length(v) > 1L) stats::sd(v) else NA_real_
  }, numeric(1)))
  tbl
}

# Severity of a triggered exposure row: depth beyond the trigger limit in units
# of the cell's historical standard deviation, so cells with different
# variability rank on one scale. Zero when the cell has no spread.
.sp_trigger_severity <- function(exposure, thresholds, scale) {
  value <- as.numeric(exposure[[thresholds$variable]])
  lim <- .sp_trigger_limits(exposure, thresholds)
  depth <- if (identical(thresholds$direction, "either")) {
    pmax(value - lim$limit, lim$low - value)
  } else if (identical(thresholds$direction, "above")) {
    value - lim$limit
  } else {
    lim$limit - value
  }
  keys <- intersect(.sp_trigger_key_cols, names(scale))
  sc <- .sp_key_chr(scale, keys)
  sc$sd <- scale$sd
  sd <- dplyr::left_join(.sp_key_chr(exposure, keys), sc, by = keys)$sd
  out <- ifelse(is.finite(sd) & sd > 0, depth / sd, 0)
  out[!is.finite(out)] <- 0
  out
}

# Layered dynamic scope (P2-8): `static rule AND payout scope AND geography AND
# predicted welfare`. Returns the `in_scope` rows that survive both layers.
#
# Geography: in each simulation year the triggered cells are ranked from the
# most to the least severe (ties by cell key, so the order never depends on
# row order) and paid while the cumulative population before the cell is below
# `geo_pct` of the year's triggered population; a share of 100 % keeps all.
#
# Predicted welfare (an oracle: the model's own central prediction of the
# household's welfare in that year): a household is selected when that level is
# at or below the run's fixed cutoff. A selected household is dropped with
# probability `excl`, an unselected one added with probability `incl`. The
# uniform draw behind it is keyed by (survey row, simulation year) and shared
# by all members, so a household has the same luck in every scenario and the
# result never depends on row order or chunk size.
.sp_dyn_scope <- function(plan, pipeline, exposure, tbl, exceeds_t, in_scope) {
  dyn <- plan$dyn
  keep <- rep(TRUE, length(in_scope))
  if (!is.null(dyn$geo_pct)) {
    g <- as.integer(exposure$row_index)
    w <- if (is.null(pipeline$weight)) rep(1, length(g)) else as.numeric(pipeline$weight)
    w[!is.finite(w) | w < 0] <- 0
    pop <- numeric(nrow(tbl))
    by_cell <- rowsum(w, g)
    pop[as.integer(rownames(by_cell))] <- by_cell[, 1L]
    # A cell belongs to one simulation year; read it from its prediction rows
    year_t <- as.character(pipeline$sim_year[match(seq_len(nrow(tbl)), g)])
    cand <- which(exceeds_t & pop > 0)
    keep_t <- rep(FALSE, nrow(tbl))
    if (length(cand)) {
      sev <- .sp_trigger_severity(tbl[cand, , drop = FALSE], plan$thresholds, dyn$scale)
      key <- do.call(paste, c(
        unname(as.list(as.data.frame(tbl)[cand, intersect(.sp_trigger_key_cols, names(tbl)), drop = FALSE])),
        list(sep = "\r")
      ))
      yr <- year_t[cand]
      ord <- order(yr, -sev, key, cand, method = "radix")
      cand <- cand[ord]
      yr <- yr[ord]
      cum_before <- stats::ave(pop[cand], yr, FUN = cumsum) - pop[cand]
      total <- stats::ave(pop[cand], yr, FUN = sum)
      keep_t[cand] <- cum_before < dyn$geo_pct * total
    }
    keep <- keep & keep_t[g]
  }
  if (!is.null(dyn$welfare_cut)) {
    y <- as.numeric(pipeline$y_point)
    level <- if (plan$is_log) exp(y) else y
    ids <- as.integer(pipeline$svy_row_id)
    u <- .sp_dyn_uniform(dyn$seed, pipeline$sim_year, ids, length(plan$reference))
    poor <- level <= dyn$welfare_cut
    pick <- ifelse(poor, u >= dyn$excl, u < dyn$incl)
    keep <- keep & !is.na(pick) & pick & is.finite(level)
  }
  in_scope & keep
}

# One uniform per prediction row, keyed by (survey row, simulation year).
.sp_dyn_uniform <- function(seed, year, ids, n_svy) {
  u <- numeric(length(ids))
  for (idx in split(seq_along(ids), as.character(year))) {
    draw <- withr::with_seed(
      wise_seed(seed, "policy", "sp-dynamic", as.character(year[idx[1L]])),
      stats::runif(n_svy)
    )
    u[idx] <- draw[ids[idx]]
  }
  u
}

# Annual transfer cost, in the entry currency, of one unit of stored daily
# transfer in each survey row: the arithmetic of `.sp_transfer_totals_values()`
# written as a per-row multiplier, so a year's transfer cost is a plain sum.
.sp_shock_cost_weights <- function(cost_frame, analysis_unit, currency) {
  n <- nrow(cost_frame)
  has_w <- "weight" %in% names(cost_frame)
  w <- if (has_w) suppressWarnings(as.numeric(cost_frame$weight)) else rep(1, n)
  hh <- if (identical(analysis_unit, "hh") && "hhsize" %in% names(cost_frame)) {
    h <- suppressWarnings(as.numeric(cost_frame$hhsize))
    h[!is.finite(h) | h <= 0] <- 1
    h
  } else {
    rep(1, n)
  }
  scale <- if (identical(analysis_unit, "hh") && !has_w) hh else rep(1, n)
  ppp <- if (identical(.sp_currency(currency), "LCU") && "ppp2021" %in% names(cost_frame)) {
    suppressWarnings(as.numeric(cost_frame$ppp2021))
  } else {
    rep(1, n)
  }
  m <- w * scale * ppp * 365
  m[!is.finite(m)] <- 0
  m
}

# Household-size divisor of the per-capita gain: a per-household amount is shared
# by the members, a per-person amount is not (`.sp_transfer_values()`).
.sp_shock_amount_scale <- function(svy, spec, analysis_unit) {
  if (identical(analysis_unit, "hh") && "hhsize" %in% names(svy) &&
    !identical(.sp_amount_basis(spec), "per_capita")) {
    h <- suppressWarnings(as.numeric(svy$hhsize))
    h[!is.finite(h) | h <= 0] <- 1
    h
  } else {
    rep_len(1, nrow(svy))
  }
}

# Transfer under the "loss_share" rule, one value per prediction row (stored
# scale). The budget of a cell (survey round, location, interview month, year)
# is `plan$loss_share` times the weighted modelled loss (rule 7: level scale,
# no residual draws, gains count as zero) of the cell's households that are
# statically eligible and in the payout scope. It is shared so that every paid
# household in the cell gets the same amount on the plan's amount basis.
.sp_shock_loss_share_transfer <- function(plan, pipeline, exposure, state) {
  if (is.null(plan$reference)) {
    stop("A budget linked to modelled loss needs the historical reference.", call. = FALSE)
  }
  ids <- as.integer(pipeline$svy_row_id)
  g <- as.integer(exposure$row_index)
  w <- if (is.null(pipeline$weight)) rep(1, length(ids)) else as.numeric(pipeline$weight)
  w[!is.finite(w) | w < 0] <- 0
  y <- as.numeric(pipeline$y_point)
  level <- if (plan$is_log) exp(y) else y
  loss <- pmax(plan$reference[ids] - level, 0) / (plan$level_factor[ids] %||% 1)
  loss[!is.finite(loss)] <- 0
  scale <- plan$amount_scale[ids]
  sel <- state$in_scope & plan$eligible[ids] & w > 0
  out <- numeric(length(ids))
  if (!any(sel)) {
    return(out)
  }
  num <- rowsum(w[sel] * loss[sel], g[sel])
  den <- rowsum(w[sel] / scale[sel], g[sel])
  amount <- plan$loss_share * num[, 1L] / den[, 1L]
  out[sel] <- amount[match(g[sel], as.integer(rownames(num)))] / scale[sel]
  out
}

# Annual cap with pro-rata scale-down: in a year whose transfer cost would exceed
# the cap, every payment is multiplied by cap / cost. `cap_binds` (named by
# simulation year) is kept as an attribute for `sp_shock_annual()`.
.sp_shock_apply_cap <- function(transfer, plan, pipeline) {
  if (!is.finite(plan$cap_transfer %||% NA_real_)) {
    return(transfer)
  }
  ids <- as.integer(pipeline$svy_row_id)
  cost <- ifelse(is.finite(transfer), transfer, 0) * plan$cost_weight[ids]
  total <- rowsum(cost, pipeline$sim_year)
  factor <- ifelse(total[, 1L] > plan$cap_transfer, plan$cap_transfer / total[, 1L], 1)
  out <- transfer * factor[match(as.character(pipeline$sim_year), rownames(total))]
  attr(out, "cap_binds") <- stats::setNames(factor < 1, rownames(total))
  out
}

#' Each household's own historical mean outcome level
#'
#' The reference of the modelled weather loss (plan section 4.4): the mean over
#' the historical years of the back-transformed central prediction. The level is
#' averaged, not the log, since the loss is measured on levels.
#'
#' @param pipeline Historical Step 2 pipeline (`y_point`, `svy_row_id`).
#' @param n_svy Number of survey rows.
#' @param is_log Whether the outcome is log-transformed.
#' @return Numeric vector of length `n_svy`; `NA` for households without a
#'   finite prediction.
#' @keywords internal
sp_shock_reference <- function(pipeline, n_svy, is_log) {
  level <- if (is_log) exp(as.numeric(pipeline$y_point)) else as.numeric(pipeline$y_point)
  ok <- is.finite(level)
  ids <- as.integer(pipeline$svy_row_id)
  sums <- rowsum(level[ok], ids[ok])
  n <- rowsum(rep(1, sum(ok)), ids[ok])
  ref <- rep(NA_real_, n_svy)
  ref[as.integer(rownames(sums))] <- sums[, 1L] / n[, 1L]
  ref
}

#' Dynamic transfer of one pipeline, on the stored welfare scale
#'
#' Evaluates the trigger on the pipeline's own exposure table (one small join,
#' then indexed by prediction row), applies the payout scope with the
#' pipeline's weights, and multiplies by the per-household transfer. The same
#' pure function backs every consumer of the correction, so they cannot differ.
#'
#' @param plan Result of `sp_shock_plan()`.
#' @param pipeline Step 2 prediction pipeline (`svy_row_id`, `sim_year`,
#'   `weight`).
#' @param exposure Resolved exposure mapping of that pipeline (`table`,
#'   `row_index`).
#' @param state Optional result of `sp_shock_pipeline_state()`, when the caller
#'   has it already.
#' @return Numeric vector, one value per prediction row. With an annual cap it
#'   carries the attribute `cap_binds` (logical by simulation year).
#' @keywords internal
sp_shock_pipeline_transfer <- function(plan, pipeline, exposure, state = NULL) {
  state <- state %||% sp_shock_pipeline_state(plan, pipeline, exposure)
  raw <- if (identical(plan$budget_rule %||% "fixed", "loss_share")) {
    .sp_shock_loss_share_transfer(plan, pipeline, exposure, state)
  } else {
    plan$per_household[as.integer(pipeline$svy_row_id)] * state$in_scope
  }
  .sp_shock_apply_cap(raw, plan, pipeline)
}

#' Trigger state of one pipeline (see `sp_trigger_state()`)
#'
#' Evaluates the trigger on the pipeline's exposure table, then indexes the
#' result by prediction row.
#'
#' @inheritParams sp_shock_pipeline_transfer
#' @return The list returned by `sp_trigger_state()`.
#' @keywords internal
sp_shock_pipeline_state <- function(plan, pipeline, exposure) {
  tbl <- as.data.frame(exposure$table)
  if (!is.null(plan$impact)) {
    tbl[[SP_IMPACT_COL]] <- .sp_impact_column(plan$impact, pipeline, exposure, plan$reference)
  }
  exceeds_t <- .sp_trigger_exceeds(tbl, plan$thresholds)
  g <- exposure$row_index
  hit <- exceeds_t[g]
  # An anticipatory design pays on a forecast of the trigger (P2-7)
  if (isTRUE(plan$timing$anticipatory)) {
    exceeds_t <- .sp_forecast_fire(plan$timing, tbl, g, pipeline$sim_year, exceeds_t)
  }
  exceeds <- exceeds_t[g]
  state <- c(list(exceeds = exceeds), .sp_trigger_scope(
    exceeds, pipeline$sim_year, pipeline$weight, plan$spec
  )[c("in_scope", "share", "gate")])
  if (!is.null(plan$dyn)) {
    state$in_scope <- .sp_dyn_scope(plan, pipeline, exposure, tbl, exceeds_t, state$in_scope)
  }
  if (!is.null(plan$timing)) {
    state$hit <- hit
    state$effect <- .sp_timing_effect(plan$timing, hit)
  }
  state
}

#' Run outputs of one pipeline, one row per simulation year
#'
#' Annual cost and activation (`sp_shock_annual()`) plus the population-weighted
#' pieces of basis risk and leakage, so pipelines pool by plain sums. A cell is
#' a (survey round, location, interview month, year) exposure row. A cell has a
#' loss event when the mean modelled loss of its households, relative to each
#' household's own historical mean level, is at or below `-loss_event_pct`
#' percent (decision 6); the trigger "fires" in a cell when its weather exceeds
#' the threshold. The `pop_*` columns hold the population weight of cells that
#' are true or false positives and negatives; `spend_*` the spending reaching
#' all cells and cells without a loss event. Basis-risk columns are `NA` when
#' the plan has no historical reference.
#'
#' @param plan Result of `sp_shock_plan()`.
#' @param pipeline Baseline prediction pipeline (central `y_point`).
#' @param exposure Resolved exposure of that pipeline.
#' @param state Result of `sp_shock_pipeline_state()`.
#' @param transfer Result of `sp_shock_pipeline_transfer()` (stored scale).
#' @param transfer_model The same transfer on the outcome's level scale, for the
#'   `"reach"` attribute (per-survey-row loss-event counts, see
#'   `sp_shock_reach()`); defaults to `transfer`.
#' @return Data frame, see above, with attributes `cells` and `reach`.
#' @keywords internal
sp_shock_pipeline_rows <- function(plan, pipeline, exposure, state, transfer,
                                   transfer_model = NULL) {
  annual <- sp_shock_annual(
    transfer, pipeline, plan$cost_frame, state, plan$analysis_unit,
    plan$currency, plan$admin_share
  )
  cols <- c("pop_tp", "pop_fp", "pop_fn", "pop_tn", "spend_total", "spend_no_event")
  annual[cols] <- NA_real_
  # Modelled loss and payments in the same units and weights, for the share of
  # loss offset (P2-6)
  annual[c("loss_value", "paid_value")] <- NA_real_
  # Anticipatory design (P2-7): value of payments before events that occur
  # (hits) and before none (false alarms), and the value the same trigger pays
  # ex post after the delay, in the same units and weights
  annual[c("hit_value", "false_value", "expost_value")] <- NA_real_
  if (isTRUE(plan$timing$anticipatory)) {
    annual[c("hit_value", "false_value", "expost_value")] <-
      .sp_timing_values(plan, pipeline, exposure, state, transfer, annual$sim_year)
  }
  if (is.null(plan$reference)) {
    return(annual)
  }
  g <- as.integer(exposure$row_index)
  w <- if (is.null(pipeline$weight)) rep(1, length(g)) else as.numeric(pipeline$weight)
  w[!is.finite(w) | w < 0] <- 0
  y <- as.numeric(pipeline$y_point)
  level <- if (plan$is_log) exp(y) else y
  ref <- plan$reference[as.integer(pipeline$svy_row_id)]
  rel <- level / ref - 1
  ok <- is.finite(rel) & w > 0
  cell_w <- rowsum(w[ok], g[ok])
  cell <- as.integer(rownames(cell_w))
  first <- match(cell, g)
  spend <- rowsum(ifelse(is.finite(transfer), transfer, 0) * w, g)
  cells <- data.frame(
    sim_year = pipeline$sim_year[first], pop = cell_w[, 1L],
    rel = rowsum((w * rel)[ok], g[ok])[, 1L] / cell_w[, 1L],
    fired = state$exceeds[first],
    spend = spend[match(cell, as.integer(rownames(spend))), 1L]
  )
  basis <- .sp_shock_basis(cells, plan$loss_event_pct, as.character(cells$sim_year))
  annual[cols] <- basis[match(as.character(annual$sim_year), rownames(basis)), cols]
  annual[cols] <- lapply(annual[cols], function(x) ifelse(is.na(x), 0, x))
  loss_all <- pmax(ref - level, 0) / (plan$level_factor[as.integer(pipeline$svy_row_id)] %||% 1)
  loss_all[!is.finite(loss_all)] <- 0
  paid_all <- transfer
  paid_all[!is.finite(paid_all)] <- 0
  by_year <- rowsum(cbind(loss_value = w * loss_all, paid_value = w * paid_all), pipeline$sim_year)
  hit <- match(as.character(annual$sim_year), rownames(by_year))
  annual$loss_value <- by_year[hit, "loss_value"]
  annual$paid_value <- by_year[hit, "paid_value"]
  attr(annual, "cells") <- cells
  attr(annual, "reach") <- .sp_shock_reach_rows(
    plan, pipeline, level, ref, rel, ok, transfer_model %||% transfer
  )
  annual
}

# Per survey row, over the pipeline's years: household-years with a loss event
# (modelled loss of at least `loss_event_pct` of the household's own typical
# level), those among them in which the household was paid, the summed loss and
# the summed payment in those years. Line-free, so coverage and adequacy of the
# shock-affected poor can be evaluated at any poverty line afterwards
# (`sp_shock_reach()`). `transfer` is on the outcome's level scale, like `level`.
.sp_shock_reach_rows <- function(plan, pipeline, level, ref, rel, ok, transfer) {
  ids <- as.integer(pipeline$svy_row_id)
  event <- ok & rel <= -plan$loss_event_pct / 100
  if (!any(event)) {
    return(matrix(0, 0L, 4L, dimnames = list(NULL, .sp_shock_reach_cols)))
  }
  paid <- ifelse(is.finite(transfer[event]), transfer[event], 0)
  m <- cbind(
    n_event = 1, n_covered = paid > 0,
    loss = pmax(ref[event] - level[event], 0), paid = paid
  )
  rowsum(m, ids[event])
}

.sp_shock_reach_cols <- c("n_event", "n_covered", "loss", "paid")

# Add one pipeline's sparse reach rows into the per-scenario dense totals
# (`acc` is a named list of n_svy x 4 matrices).
.sp_shock_reach_add <- function(acc, scenario, reach, n_svy) {
  if (is.null(reach) || !nrow(reach)) {
    return(acc)
  }
  cur <- acc[[scenario]] %||% matrix(
    0, n_svy, length(.sp_shock_reach_cols), dimnames = list(NULL, .sp_shock_reach_cols)
  )
  i <- as.integer(rownames(reach))
  cur[i, ] <- cur[i, , drop = FALSE] + reach
  acc[[scenario]] <- cur
  acc
}

#' Coverage and adequacy of the shock-affected poor
#'
#' Over the household-years with a modelled loss event (the run's loss-event
#' share), for households poor at the baseline: the share that was paid
#' (coverage) and the payments as a share of the modelled loss (adequacy, in
#' the outcome's level units). People are weighted by the survey weight. The
#' poverty line is the display line (plan rule 15), so it can change without a
#' new run.
#'
#' @param reach Named list by scenario of n_svy x 4 matrices (`n_event`,
#'   `n_covered`, `loss`, `paid`), as published in `shock_summary()$reach`.
#' @param svy Baseline survey frame (`welfare`, optionally `weight`, `ppp2021`).
#' @param poverty_line Display poverty line in the entry currency per day.
#' @param currency `"PPP"` or `"LCU"`.
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @return Data frame: `scenario`, `coverage`, `adequacy`, `poor_affected_years`
#'   (weighted household-years of affected poor), `NA` when nothing qualifies.
#'   Zero rows without a usable line or reach.
#' @keywords internal
sp_shock_reach <- function(reach, svy, poverty_line, currency = "PPP",
                           analysis_unit = "hh") {
  empty <- data.frame(
    scenario = character(), coverage = numeric(), adequacy = numeric(),
    poor_affected_years = numeric(), stringsAsFactors = FALSE
  )
  pl <- suppressWarnings(as.numeric(poverty_line))[1L]
  if (!is.list(reach) || !length(reach) || !is.data.frame(svy) ||
    !"welfare" %in% names(svy) || !is.finite(pl) || pl <= 0) {
    return(empty)
  }
  n <- nrow(svy)
  line <- .povline_to_ppp(rep(pl, n), svy, identical(.sp_currency(currency), "LCU"))
  welfare <- suppressWarnings(as.numeric(svy$welfare))
  poor <- is.finite(welfare) & is.finite(line) & welfare < line
  w <- .sp_ce_population_weights(svy, analysis_unit) * poor
  out <- lapply(names(reach), function(s) {
    m <- reach[[s]]
    if (nrow(m) != n) {
      return(NULL)
    }
    den_n <- sum(w * m[, "n_event"])
    den_l <- sum(w * m[, "loss"])
    data.frame(
      scenario = s,
      coverage = if (den_n > 0) sum(w * m[, "n_covered"]) / den_n else NA_real_,
      adequacy = if (den_l > 0) sum(w * m[, "paid"]) / den_l else NA_real_,
      poor_affected_years = den_n, stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) empty else do.call(rbind, out)
}

# Population weights of true and false positives and negatives, and spending,
# summed by `key` (one value per cell). A cell has a loss event when its mean
# modelled loss is at or below minus `loss_event_pct` percent.
.sp_shock_basis <- function(cells, loss_event_pct, key) {
  event <- cells$rel <= -loss_event_pct / 100
  fired <- cells$fired
  m <- cbind(
    pop_tp = cells$pop * (fired & event), pop_fp = cells$pop * (fired & !event),
    pop_fn = cells$pop * (!fired & event), pop_tn = cells$pop * (!fired & !event),
    spend_total = cells$spend, spend_no_event = cells$spend * !event
  )
  rowsum(m, key)
}

#' Score a stored shock run again with another loss-event share
#'
#' The loss event only scores the trigger (basis risk and leakage); it changes
#' no payment and no welfare result. The run therefore keeps its per-cell loss
#' data (`cells`), and this recomputes the scoring columns and the summary for a
#' new share without running Step 3 again.
#'
#' @param shock List with `rows`, `cells` and `summary` as published by the run.
#' @param loss_event_pct Loss-event share in percent.
#' @return `shock` with `rows` and `summary` recomputed (a run without cells,
#'   or an invalid share, is returned unchanged).
#' @keywords internal
sp_shock_rescore <- function(shock, loss_event_pct) {
  pct <- suppressWarnings(as.numeric(loss_event_pct))[1L]
  if (is.null(shock$cells) || !is.finite(pct) || pct <= 0) {
    return(shock)
  }
  cols <- c("pop_tp", "pop_fp", "pop_fn", "pop_tn", "spend_total", "spend_no_event")
  key <- function(d) paste(d$scenario, d$member, d$sim_year, sep = "\r")
  basis <- .sp_shock_basis(shock$cells, pct, key(shock$cells))
  rows <- shock$rows
  hit <- match(key(rows), rownames(basis))
  rows[cols] <- lapply(cols, function(col) {
    ifelse(is.na(hit), 0, basis[hit, col])
  })
  shock$rows <- rows
  shock$summary <- sp_shock_summary(rows)
  shock
}

#' Summary of a shock program across members and years, by scenario
#'
#' Pools the rows of `sp_shock_pipeline_rows()` (columns `scenario`, `member`
#' added by the caller). Every (member, year) pair counts once for activation
#' and cost; basis risk and leakage pool population weights. Rates are `NA`
#' when their denominator is empty.
#'
#' @param rows Data frame of pipeline rows with `scenario` and `member`.
#' @return Data frame, one row per scenario: `n_member_years`,
#'   `activation_freq`, `mean_total_cost`, `median_total_cost`, `cost_1_in_20`
#'   (95th percentile, `NA` below 20 member-years), `mean_transfer_cost`,
#'   `mean_admin_cost`, `false_positive_rate`, `false_negative_rate`,
#'   `leakage_share`, `cost_cv` (coefficient of variation of annual total cost),
#'   `loss_offset_share` (payments over all modelled loss, same weights and
#'   scale; gains count as no loss) and `cap_exhaustion_share` (share of
#'   member-years in which the annual cap binds; 0 without a cap).
#' @keywords internal
sp_shock_summary <- function(rows) {
  ratio <- function(num, den) if (is.finite(den) && den > 0) num / den else NA_real_
  out <- lapply(split(rows, factor(rows$scenario, levels = unique(rows$scenario))), function(d) {
    n <- nrow(d)
    data.frame(
      scenario = d$scenario[[1L]], n_member_years = n,
      activation_freq = mean(d$activated),
      mean_total_cost = mean(d$total_cost),
      median_total_cost = stats::median(d$total_cost),
      cost_1_in_20 = if (n >= 20L) {
        unname(stats::quantile(d$total_cost, 0.95, names = FALSE))
      } else {
        NA_real_
      },
      mean_transfer_cost = mean(d$transfer_cost),
      mean_admin_cost = mean(d$admin_cost),
      false_positive_rate = ratio(sum(d$pop_fp), sum(d$pop_fp) + sum(d$pop_tn)),
      false_negative_rate = ratio(sum(d$pop_fn), sum(d$pop_fn) + sum(d$pop_tp)),
      leakage_share = ratio(sum(d$spend_no_event), sum(d$spend_total)),
      cost_cv = if (n >= 2L) ratio(stats::sd(d$total_cost), mean(d$total_cost)) else NA_real_,
      loss_offset_share = ratio(sum(d$paid_value %||% NA_real_), sum(d$loss_value %||% NA_real_)),
      cap_exhaustion_share = if (is.null(d$cap_binds)) NA_real_ else mean(d$cap_binds),
      # Anticipatory design (P2-7): payments before no event over all payments,
      # and the multiplier at which the effective value of the payments equals
      # what the same trigger pays ex post (hits x m + false alarms = ex post),
      # floored at 0 (false alarms alone already match it)
      false_alarm_cost_share = ratio(
        sum(d$false_value %||% NA_real_), sum(d$hit_value %||% NA_real_) + sum(d$false_value %||% NA_real_)
      ),
      breakeven_m = {
        be <- ratio(
          sum(d$expost_value %||% NA_real_) - sum(d$false_value %||% NA_real_),
          sum(d$hit_value %||% NA_real_)
        )
        if (is.finite(be)) max(be, 0) else NA_real_
      }
    )
  })
  do.call(rbind, c(unname(out), list(make.row.names = FALSE)))
}

#' Annual cost of a shock program by scenario, on the effect's own support
#'
#' The cost side of the cost-effectiveness figures (P2-2). The Results effect is
#' an equal-model mean over matched (member, year) cells, and its adverse-year
#' value interpolates two ranked baseline years per member
#' (`adverse_year_support()`). The cost is pooled the same way, so both sides of
#' a ratio describe the same member-years, not a ratio of unrelated means.
#'
#' `cost_avg` is the equal-member mean of each member's mean annual cost over the
#' matched years. `cost_tail` applies each member's adverse-year support to its
#' annual cost and averages the members equally; it is `NA` when any member in
#' `supports` has no usable support or no cost for a support year.
#'
#' @param rows Run rows (`scenario`, `member`, `sim_year`, `total_cost`), as
#'   published in `shock_summary()$rows`.
#' @param matched Optional data frame (`scenario`, `model_id`, `sim_year`) of the
#'   member-years behind the effect (`paired_annual_effects`). `NULL` keeps all
#'   rows.
#' @param supports Optional data frame of adverse-year supports (`scenario`,
#'   `model_id`, `probability` and the `adverse_year_support()` fields, as in
#'   `metric_decomposition()$adverse_support`).
#' @param historical Scenario label of the historical arm. Rows of member
#'   `"Historical"` take this label so they match the Results scenario names.
#' @param probability Exceedance probability of the adverse year (0.05 is the
#'   1-in-20 year).
#' @return Data frame: `scenario`, `cost_avg`, `cost_tail`, `n_models`.
#' @keywords internal
sp_shock_cost_by_scenario <- function(rows, matched = NULL, supports = NULL,
                                      historical = NULL, probability = 0.05) {
  empty <- data.frame(
    scenario = character(), cost_avg = numeric(), cost_tail = numeric(),
    n_models = integer(), stringsAsFactors = FALSE
  )
  need <- c("scenario", "member", "sim_year", "total_cost")
  if (!is.data.frame(rows) || !nrow(rows) || !all(need %in% names(rows))) {
    return(empty)
  }
  d <- data.frame(
    scenario = as.character(rows$scenario), member = as.character(rows$member),
    sim_year = as.numeric(rows$sim_year), cost = as.numeric(rows$total_cost),
    stringsAsFactors = FALSE
  )
  if (!is.null(historical)) {
    d$scenario[d$member == "Historical"] <- as.character(historical)[1L]
  }
  key <- function(s, m, y) paste(s, m, y, sep = "\r")
  if (is.data.frame(matched) && nrow(matched)) {
    d <- d[key(d$scenario, d$member, d$sim_year) %in%
      key(matched$scenario, matched$model_id, as.numeric(matched$sim_year)), , drop = FALSE]
  }
  d <- d[is.finite(d$cost), , drop = FALSE]
  if (!nrow(d)) {
    return(empty)
  }

  scen <- unique(d$scenario)
  out <- lapply(scen, function(s) {
    ds <- d[d$scenario == s, , drop = FALSE]
    members <- split(ds, ds$member)
    avg <- mean(vapply(members, function(m) mean(m$cost), numeric(1)))
    tail <- NA_real_
    if (is.data.frame(supports) && nrow(supports)) {
      sup <- supports[as.character(supports$scenario) == s &
        abs(as.numeric(supports$probability) - probability) < 1e-8, , drop = FALSE]
      if (nrow(sup)) {
        vals <- vapply(seq_len(nrow(sup)), function(i) {
          m <- members[[as.character(sup$model_id[[i]])]]
          if (is.null(m)) {
            return(NA_real_)
          }
          apply_adverse_year_support(m$cost, m$sim_year, sup[i, , drop = FALSE])$value
        }, numeric(1))
        if (all(is.finite(vals))) tail <- mean(vals)
      }
    }
    data.frame(
      scenario = s, cost_avg = avg, cost_tail = tail, n_models = length(members),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Climate-adjusted cost: the shock program under each scenario against history
#'
#' The trigger is fixed on the historical record, so under warming it fires more
#' (or less) often and the bill moves with it. This lays the run summary out as
#' that comparison (plan section 3.8): activation and mean annual cost per
#' scenario and their change against the reference scenario.
#'
#' @param summary Result of `sp_shock_summary()` (or `sp_shock_rescore()`).
#' @param reference Scenario label to compare against; defaults to the first
#'   row, which the run fills with the historical arm.
#' @return Data frame: `scenario`, `activation_freq`, `mean_total_cost`,
#'   `cost_1_in_20`, `activation_change` (percentage points against the
#'   reference, as a fraction), `cost_change` (relative change in mean cost;
#'   `NA` when the reference cost is 0), `is_reference`.
#' @keywords internal
sp_shock_climate_cost <- function(summary, reference = NULL) {
  if (!is.data.frame(summary) || !nrow(summary)) {
    return(data.frame())
  }
  ref <- if (!is.null(reference) && reference %in% summary$scenario) {
    match(reference, summary$scenario)
  } else {
    1L
  }
  base_cost <- summary$mean_total_cost[[ref]]
  data.frame(
    scenario = as.character(summary$scenario),
    activation_freq = summary$activation_freq,
    mean_total_cost = summary$mean_total_cost,
    cost_1_in_20 = summary$cost_1_in_20,
    activation_change = summary$activation_freq - summary$activation_freq[[ref]],
    cost_change = if (is.finite(base_cost) && base_cost > 0) {
      summary$mean_total_cost / base_cost - 1
    } else {
      rep(NA_real_, nrow(summary))
    },
    is_reference = seq_len(nrow(summary)) == ref,
    stringsAsFactors = FALSE
  )
}

# Display table for `sp_shock_climate_cost()`: strings, with signed changes.
.sp_climate_cost_display <- function(d, currency = "PPP") {
  if (!is.data.frame(d) || !nrow(d)) {
    return(data.frame(Scenario = character()))
  }
  prefix <- .sp_currency_prefix(currency)
  money <- function(x) fmt_num(x, digits = 0, prefix = prefix, na = "Not available")
  signed <- function(x, scale, suffix) {
    ifelse(is.finite(x), paste0(ifelse(x > 0, "+", ""), fmt_num(scale * x, 1, suffix = suffix)),
      "Not available")
  }
  out <- data.frame(
    Scenario = d$scenario,
    `Years with payments` = fmt_num(100 * d$activation_freq, 1, suffix = "%", na = "Not available"),
    `Change vs historical` = ifelse(d$is_reference, "Reference",
      signed(d$activation_change, 100, " pp")),
    `Mean annual cost` = money(d$mean_total_cost),
    `Cost change vs historical` = ifelse(d$is_reference, "Reference",
      signed(d$cost_change, 100, "%")),
    `1-in-20 year cost` = money(d$cost_1_in_20),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out
}

#' Annual cost and activation of one pipeline
#'
#' One row per simulation year. A year activates when any prediction row is
#' paid. Costs use the diagnostics' arithmetic (`.sp_transfer_totals_values()`),
#' in the entry currency and with administration on top of the net transfers.
#'
#' @param transfer Result of `sp_shock_pipeline_transfer()` (stored scale).
#' @param pipeline Step 2 prediction pipeline.
#' @param svy Survey frame the pipeline's `svy_row_id` refers to.
#' @param state Optional result of `sp_shock_pipeline_state()`; adds
#'   `exposed_share`.
#' @param analysis_unit,currency,admin_share As for `.sp_transfer_totals()`.
#' @return Data frame: `sim_year`, `activated`, `exposed_share`,
#'   `transfer_cost`, `admin_cost`, `total_cost`.
#' @keywords internal
sp_shock_annual <- function(transfer, pipeline, svy, state = NULL,
                            analysis_unit = "hh", currency = "PPP", admin_share = 0) {
  ids <- as.integer(pipeline$svy_row_id)
  year <- pipeline$sim_year
  years <- sort(unique(year))
  binds <- attr(transfer, "cap_binds")
  rows <- lapply(years, function(y) {
    idx <- which(year == y)
    v <- numeric(nrow(svy))
    # A survey row can appear more than once in a year; its transfers add up
    paid <- rowsum(transfer[idx], ids[idx])
    v[as.integer(rownames(paid))] <- paid[, 1L]
    totals <- .sp_transfer_totals_values(v, svy, analysis_unit, currency, admin_share)
    data.frame(
      sim_year = y, activated = any(transfer[idx] > 0, na.rm = TRUE),
      exposed_share = if (is.null(state)) NA_real_ else unname(state$share[as.character(y)]),
      transfer_cost = totals$total, admin_cost = totals$admin_cost,
      total_cost = totals$total_cost,
      cap_binds = isTRUE(unname(binds[as.character(y)]))
    )
  })
  do.call(rbind, rows)
}

#' Expected activation and annual cost of a shock program, before it is run
#'
#' The Step 3 summary-card preview: the historical pipeline's own years give the
#' share of years with payments, the mean annual cost and, when the record has
#' at least 20 years, the 1-in-20 annual cost (the 95th percentile of annual
#' cost). It uses the run's seeded eligibility, thresholds and arithmetic, so
#' the figures are what the run will produce for the historical arm.
#'
#' @param spec Scenario list.
#' @param hist_sim Step 2 `hist_sim` (`pipeline`, with its exposure).
#' @param svy Survey frame the run will use (`hist_sim$svy`).
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @param seed Base seed, as passed to `apply_policy_to_svy()`.
#' @param exposure Optional resolved exposure of the historical pipeline, when
#'   the caller has it already (the module caches it).
#' @return A list. On success: `annual` (see `sp_shock_annual()`),
#'   `activation_freq`, `mean_cost`, `cost_1_in_20` (`NA` below 20 years),
#'   `false_positive_rate`, `false_negative_rate`, `leakage_share` (scored with
#'   `spec$loss_event_pct`, historical arm), `mean_exposed_share`, `n_years`, `n_record_years` (finite historical
#'   years of the trigger variable). Otherwise `problem`, a message to show.
#' @keywords internal
sp_shock_preview <- function(spec, hist_sim, svy, analysis_unit = "hh",
                             seed = WISEAPP_DEFAULT_SEED, exposure = NULL) {
  problem <- .sp_shock_problem(spec)
  if (!is.null(problem)) {
    return(list(problem = problem))
  }
  pipeline <- hist_sim$pipeline
  exposure <- exposure %||% tryCatch(
    step2_exposure_resolve(pipeline, hist_sim), error = function(e) NULL
  )
  if (is.null(svy) || is.null(exposure) || !identical(exposure$status, "ok") ||
    !is.data.frame(exposure$table)) {
    return(list(problem = "The historical weather exposure is not available yet."))
  }
  tryCatch(
    {
      eligible <- withr::with_seed(
        wise_seed(seed, "policy", "sp"), .determine_sp_eligibility(svy, spec)
      )
      plan <- sp_shock_plan(
        spec, exposure$table, eligible, svy, analysis_unit,
        hist_pipeline = pipeline,
        is_log = identical(hist_sim$so$transform %||% "", "log"),
        level_factor = outcome_level_scale(rep(1, nrow(svy)), hist_sim$so, .outcome_ppp(svy)),
        seed = seed, hist_exposure_map = exposure, pov_line = hist_sim$pov_line
      )
      state <- sp_shock_pipeline_state(plan, pipeline, exposure)
      transfer <- sp_shock_pipeline_transfer(plan, pipeline, exposure, state)
      # The same rows the run reports for the historical arm
      annual <- sp_shock_pipeline_rows(plan, pipeline, exposure, state, transfer)
      attr(annual, "cells") <- NULL
      basis <- sp_shock_summary(cbind(scenario = "Historical", member = "Historical", annual))
      n_years <- nrow(annual)
      list(
        annual = annual,
        false_positive_rate = basis$false_positive_rate,
        false_negative_rate = basis$false_negative_rate,
        leakage_share = basis$leakage_share,
        cost_cv = basis$cost_cv,
        loss_offset_share = basis$loss_offset_share,
        cap_exhaustion_share = basis$cap_exhaustion_share,
        false_alarm_cost_share = basis$false_alarm_cost_share,
        breakeven_m = basis$breakeven_m,
        activation_freq = mean(annual$activated),
        mean_cost = mean(annual$total_cost),
        cost_1_in_20 = if (n_years >= 20L) {
          unname(stats::quantile(annual$total_cost, 0.95, names = FALSE))
        } else {
          NA_real_
        },
        mean_exposed_share = mean(annual$exposed_share),
        n_years = n_years,
        n_record_years = plan$impact$n_years %||% length(unique(exposure$table$sim_year[
          is.finite(as.numeric(exposure$table[[spec$trigger_variable]]))
        ]))
      )
    },
    sp_trigger_problem = function(e) list(problem = conditionMessage(e))
  )
}

#' Response check: where modelled loss concentrates along a weather variable
#'
#' A weather variable has no built-in adverse tail (on BFA the loss follows wet
#' years in `spei6`, not temperature), so before choosing a trigger the user can
#' see the mean modelled loss by decile of the variable. The loss is the rule 7
#' definition used by `sp_shock_pipeline_rows()`: level-scale central prediction
#' relative to the household's own historical mean level
#' (`sp_shock_reference()`), back-transformed before averaging, survey-weighted,
#' no residual draws. Negative values are losses. Deciles are weighted deciles of
#' the variable over household-years (prediction rows): equal values stay in one
#' decile, which is the one holding their cumulative weight share, so fewer than
#' `n_bins` deciles can result for a coarse variable.
#'
#' The direction suggestion compares the mean loss in the lowest and highest
#' fifth of the deciles with the middle ones. An end counts when its loss is at
#' least 1 percentage point deeper than the middle; both ends count when the
#' shallower one is at least half as deep as the deeper one (`"either"`);
#' otherwise the deeper end is suggested. With no end that deep the answer is
#' `"none"`.
#'
#' @param pipeline Historical Step 2 pipeline (`y_point`, `svy_row_id`,
#'   `weight`).
#' @param exposure Resolved exposure of that pipeline (`table`, `row_index`).
#' @param variable Weather variable (a column of `exposure$table`).
#' @param is_log Whether the outcome is log-transformed.
#' @param n_bins Number of weighted bins (deciles by default).
#' @return A list. On success: `deciles` (data frame: `decile`, `pop`,
#'   `mean_value`, `min_value`, `max_value`, `mean_loss`), `direction`
#'   (`"above"`, `"below"`, `"either"` or `"none"`), `depth` (named `low`,
#'   `high`: mean loss in the middle minus mean loss at that end), `hint` (one
#'   sentence), `variable`, `n_rows`. Otherwise `problem`, a message to show.
#' @keywords internal
sp_response_check <- function(pipeline, exposure, variable, is_log = FALSE, n_bins = 10L) {
  tbl <- if (is.list(exposure)) exposure$table
  if (!is.data.frame(tbl) || length(variable) != 1L || !variable %in% names(tbl) ||
    !is.numeric(tbl[[variable]])) {
    return(list(problem = paste0(
      "The response check needs a continuous weather variable; '",
      paste(variable, collapse = ""), "' is not one."
    )))
  }
  ids <- as.integer(pipeline$svy_row_id)
  value <- as.numeric(tbl[[variable]])[exposure$row_index]
  w <- if (is.null(pipeline$weight)) rep(1, length(ids)) else as.numeric(pipeline$weight)
  w[!is.finite(w) | w < 0] <- 0
  y <- as.numeric(pipeline$y_point)
  level <- if (isTRUE(is_log)) exp(y) else y
  rel <- level / sp_shock_reference(pipeline, max(ids), isTRUE(is_log))[ids] - 1
  ok <- is.finite(value) & is.finite(rel) & w > 0
  if (sum(ok) < 2L || length(unique(value[ok])) < 3L) {
    return(list(problem = paste0(
      "'", variable, "' has too few distinct historical values to show a response."
    )))
  }
  v <- value[ok]
  w <- w[ok]
  rel <- rel[ok]

  u <- sort(unique(v))
  g <- match(v, u)
  cum <- cumsum(rowsum(w, g, reorder = TRUE)[, 1L]) / sum(w)
  bin <- pmin(n_bins, pmax(1L, ceiling(cum * n_bins - 1e-9)))[g]
  pop <- rowsum(w, bin, reorder = TRUE)[, 1L]
  deciles <- data.frame(
    decile = as.integer(names(pop)), pop = unname(pop),
    mean_value = unname(rowsum(w * v, bin, reorder = TRUE)[, 1L] / pop),
    min_value = unname(tapply(v, bin, min)), max_value = unname(tapply(v, bin, max)),
    mean_loss = unname(rowsum(w * rel, bin, reorder = TRUE)[, 1L] / pop)
  )
  nb <- nrow(deciles)
  if (nb < 3L) {
    return(list(problem = paste0(
      "'", variable, "' has too few distinct historical values to show a response."
    )))
  }

  k <- max(1L, round(0.2 * nb))
  ends <- list(low = seq_len(k), high = seq(nb - k + 1L, nb))
  mid_i <- setdiff(seq_len(nb), c(ends$low, ends$high))
  if (!length(mid_i)) mid_i <- seq_len(nb)
  mean_loss <- function(i) sum(deciles$pop[i] * deciles$mean_loss[i]) / sum(deciles$pop[i])
  mid <- mean_loss(mid_i)
  depth <- c(low = mid - mean_loss(ends$low), high = mid - mean_loss(ends$high))
  min_depth <- 0.01
  direction <- if (max(depth) < min_depth) {
    "none"
  } else if (all(depth >= min_depth) && min(depth) >= 0.5 * max(depth)) {
    "either"
  } else if (depth[["high"]] >= depth[["low"]]) {
    "above"
  } else {
    "below"
  }
  list(
    deciles = deciles, direction = direction, depth = depth,
    hint = .sp_response_hint(direction, variable, depth),
    variable = variable, n_rows = length(v)
  )
}

# One sentence for the response check. Names the weather condition when the
# variable name says what its high and low ends mean (standardized indices and
# precipitation: wet / dry; temperature: hot / cold).
.sp_response_hint <- function(direction, variable, depth) {
  poles <- if (grepl("^(spei|spi|pr|precip|rain|ppt)", variable, ignore.case = TRUE)) {
    c(low = "dry years", high = "wet years")
  } else if (grepl("^(t|temp|tmax|tmin|tmean|tas)([0-9_]|$)", variable, ignore.case = TRUE)) {
    c(low = "cold years", high = "hot years")
  }
  pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), " points")
  switch(direction,
    none = paste0(
      "No clear response: modelled loss does not concentrate at either end of ",
      variable, ", so it is a weak trigger."
    ),
    either = paste0(
      "Losses concentrate at both ends",
      if (!is.null(poles)) paste0(" (", poles[["low"]], " and ", poles[["high"]], ")"),
      ": either end of ", variable, "."
    ),
    {
      end <- if (identical(direction, "above")) "high" else "low"
      paste0(
        "Losses concentrate in ",
        if (!is.null(poles)) paste0(poles[[end]], ", ") else "",
        end, " ", variable, " (", pct(depth[[end]]), " deeper than the middle deciles)."
      )
    }
  )
}

#' Welfare effect of a transfer against the predicted level
#'
#' For a log outcome the transfer is a level amount, so its log effect is taken
#' against the predicted year-t level, `log(exp(y_t) + T) - y_t` (the formula of
#' `.policy_annual_channel_block()`, R2-BUG-07); a zero transfer is exactly 0.
#' Identity outcomes add the transfer directly. Rows with a transfer but no
#' finite prediction return `NA` for the caller to handle.
#'
#' @param y_t Predicted outcome on the model scale.
#' @param transfer Transfer on the model scale (`outcome_level_scale()`).
#' @param is_log Whether the outcome is log-transformed.
#' @return Numeric vector, same length as `transfer`.
#' @keywords internal
sp_dynamic_effect <- function(y_t, transfer, is_log) {
  if (!is_log) {
    return(as.numeric(transfer))
  }
  y_t <- as.numeric(y_t)
  transfer <- as.numeric(transfer)
  out <- rep(NA_real_, length(transfer))
  out[!is.na(transfer) & transfer == 0] <- 0
  ok <- is.finite(y_t) & is.finite(transfer) & transfer != 0
  out[ok] <- log(pmax(exp(y_t[ok]) + transfer[ok], 1e-10)) - y_t[ok]
  out
}

# Display and chart of the shock run outputs ----

#' Shock summary as a readable table: one row per measure, one column per scenario
#'
#' @param summary Result of `sp_shock_summary()`.
#' @param currency `"PPP"` or `"LCU"`, for the money prefix.
#' @return Data frame of strings; `Measure` plus one column per scenario.
#' @keywords internal
.sp_shock_display <- function(summary, currency = "PPP") {
  if (is.null(summary) || !nrow(summary)) {
    return(data.frame(Measure = character()))
  }
  prefix <- .sp_currency_prefix(currency)
  money <- function(x) fmt_num(x, digits = 0, prefix = prefix, na = "Not available")
  pct <- function(x) fmt_num(100 * x, digits = 1, suffix = "%", na = "Not available")
  measures <- list(
    "Years with payments" = function(s) pct(s$activation_freq),
    "Mean annual cost" = function(s) money(s$mean_total_cost),
    "Median annual cost" = function(s) money(s$median_total_cost),
    "1-in-20 year cost" = function(s) money(s$cost_1_in_20),
    "Of which transfers (mean)" = function(s) money(s$mean_transfer_cost),
    "Of which administration (mean)" = function(s) money(s$mean_admin_cost),
    "False positives (paid, no loss event)" = function(s) pct(s$false_positive_rate),
    "False negatives (loss event, not paid)" = function(s) pct(s$false_negative_rate),
    "Spending in locations without a loss event" = function(s) pct(s$leakage_share),
    "Variability of annual cost (CV)" = function(s) {
      fmt_num(s$cost_cv %||% NA_real_, digits = 2, na = "Not available")
    },
    "Modelled loss offset by payments" = function(s) pct(s$loss_offset_share %||% NA_real_),
    "Years the annual cap binds" = function(s) pct(s$cap_exhaustion_share %||% NA_real_),
    "Payments before no event (false alarms)" = function(s) pct(s$false_alarm_cost_share %||% NA_real_),
    "Breakeven effectiveness multiplier m" = function(s) {
      fmt_num(s$breakeven_m %||% NA_real_, digits = 2, na = "Not available")
    },
    "Member-years" = function(s) fmt_count(s$n_member_years)
  )
  # The anticipatory rows only appear for an anticipatory run
  if (!any(is.finite(c(summary$breakeven_m, summary$false_alarm_cost_share)))) {
    measures <- measures[setdiff(names(measures), c(
      "Payments before no event (false alarms)", "Breakeven effectiveness multiplier m"
    ))]
  }
  out <- data.frame(Measure = names(measures), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(summary))) {
    out[[summary$scenario[[i]]]] <- vapply(
      measures, function(f) f(summary[i, , drop = FALSE]), character(1)
    )
  }
  out
}

#' Annual cost by scenario: mean, median and 1-in-20 year
#'
#' Draws the precomputed summary (guidelines section 7); a scenario with fewer
#' than 20 member-years has no 1-in-20 bar.
#'
#' @param summary Result of `sp_shock_summary()`.
#' @param y_label Axis title.
#' @param height Widget height.
#' @return An `echarts4r` widget.
#' @keywords internal
echart_shock_cost_distribution <- function(summary, y_label = "Annual cost", height = "360px") {
  if (is.null(summary) || !nrow(summary)) {
    return(echart_blank("No shock-responsive cost to show.", height = height))
  }
  stats <- list(
    "Mean" = summary$mean_total_cost,
    "Median" = summary$median_total_cost,
    "1-in-20 year" = summary$cost_1_in_20
  )
  wide <- data.frame(scenario = as.character(summary$scenario), stringsAsFactors = FALSE)
  for (nm in names(stats)) wide[[nm]] <- stats[[nm]]
  e <- echarts4r::e_charts(wide, scenario, reorder = FALSE, height = height)
  e$x$opts$series <- lapply(seq_along(stats), function(i) {
    list(
      name = names(stats)[[i]], type = "bar",
      data = lapply(seq_len(nrow(wide)), function(r) {
        list(
          value = list(wide$scenario[[r]], wide[[names(stats)[[i]]]][[r]]),
          itemStyle = list(
            color = unname(.wise_cat[(i - 1L) %% length(.wise_cat) + 1L]),
            borderColor = "rgba(0,0,0,0)"
          )
        )
      }),
      barGap = "10%", barMaxWidth = 42
    )
  })
  e$x$opts$xAxis <- list(
    type = "category", data = wide$scenario, axisLabel = wise_eaxis_label(),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(alignWithLabel = TRUE), splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$yAxis <- list(
    type = "value", name = y_label, nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
  )
  e$x$opts$legend <- modifyList(
    list(bottom = 0, left = 0, orient = "horizontal"), wise_elegend_style()
  )
  e$x$opts$tooltip <- list(trigger = "axis", axisPointer = list(type = "shadow"))
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 14, top = 30, bottom = 46)
  e$x$opts$color <- unname(.wise_cat)
  wise_echart_theme(e)
}
