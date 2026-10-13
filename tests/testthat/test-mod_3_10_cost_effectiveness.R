# Cost-effectiveness tab (P2-2)

ce_sp <- function(...) {
  utils::modifyList(list(
    sp_type = "regular", budget_mode = "transfer_first", transfer_amount_usd = 30,
    transfer_n_payments = 4L, currency = "PPP", admin_cost_pct = 0
  ), list(...))
}

# Four households of 100 people; daily per-capita transfers 0.5, 1, 1, 0, so the
# annual cost is (50 + 100 + 100) * 365 = 91,250 and 400 people are represented
ce_frames <- function() {
  base <- data.frame(welfare = c(1, 2, 4, 6), hhsize = 2, weight = 100)
  pol <- base
  pol[[SP_TRANSFER_COL]] <- c(0.5, 1, 1, 0)
  list(base = base, pol = pol)
}

ce_args <- function(sp = ce_sp(), method = "headcount_ratio", effect = -0.05, ...) {
  f <- ce_frames()
  utils::modifyList(list(
    sp_scenario = shiny::reactiveVal(sp),
    baseline_svy = shiny::reactiveVal(f$base),
    policy_svy = shiny::reactiveVal(f$pol),
    metric_context = shiny::reactiveVal(list(
      method = method, label = "Poverty rate", availability = "ok",
      level_unit = "$ per day"
    )),
    effect_summary = shiny::reactiveVal(data.frame(
      scenario = "Hist", value = effect, coef_sd = 0.01
    )),
    metric_decomposition = shiny::reactiveVal(list(return_period = data.frame(
      scenario = "Hist", scope = "baseline_anchored", return_period = 20,
      total = effect * 2, status = "ok"
    ))),
    poverty_line = shiny::reactiveVal(3),
    hist_label = shiny::reactiveVal("Hist"),
    analysis_unit = shiny::reactiveVal("hh")
  ), list(...))
}

test_that("the tab divides the annual cost of a regular program by the Results effect", {
  testServer(mod_3_10_cost_effectiveness_server, args = ce_args(), {
    x <- ce()
    expect_identical(x$status, "ok")
    a <- x$res[x$res$basis == "Average year", ]
    t <- x$res[x$res$basis == "1-in-20 year", ]
    # 5 pp of 400 people = 20 lifted
    expect_equal(a$amount, 20)
    expect_equal(a$value, 91250 / 20)
    expect_equal(t$value, 91250 / 40)
    expect_identical(output$ce_body_ui$html |> as.character() |> grepl("ce_table", x = _), TRUE)
    ctx <- as.character(output$context_ui$html)
    expect_match(ctx, "Program: regular")
    expect_match(ctx, "Poverty line: \\$3.00 per day \\(Results\\)")
    expect_match(ctx, "Administration: 0.0% of cost")
    expect_match(as.character(output$scope_ui$html), "Cash transfers only")
  })
})

test_that("administration cost enters the cost and is printed", {
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(sp = ce_sp(admin_cost_pct = 20)), {
      a <- ce()$res
      expect_equal(a$cost[[1L]], 91250 / 0.8)
      expect_match(as.character(output$context_ui$html), "Administration: 20.0% of cost")
    })
})

test_that("a poverty metric without a line is not available", {
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(poverty_line = shiny::reactiveVal(NULL)), {
      expect_true(all(ce()$res$status == "unavailable"))
      expect_match(as.character(output$context_ui$html), "Poverty line: not available")
    })
})

test_that("the tab is empty-state safe when the SP lever is off, stale or without an effect", {
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(sp = list(sp_type = "regular", transfer_amount_usd = 0)), {
      expect_identical(ce()$status, "no_sp")
      expect_match(as.character(output$ce_body_ui$html), "No cash transfer to evaluate")
      expect_null(output$context_ui$html)
    })
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(stale = shiny::reactiveVal(TRUE)), {
      expect_identical(ce()$status, "stale")
      expect_match(as.character(output$ce_body_ui$html), "out of date")
      expect_match(as.character(output$stale_banner_ui$html), "out of date")
    })
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(effect_summary = shiny::reactiveVal(data.frame())), {
      expect_identical(ce()$status, "no_effect")
      expect_match(as.character(output$ce_body_ui$html), "not available yet")
    })
})

test_that("another active lever makes the figures not applicable", {
  testServer(mod_3_10_cost_effectiveness_server,
    args = ce_args(infra_scenario = shiny::reactiveVal(list(
      elec_universal = TRUE, elec_access_change_pct = 30
    ))), {
      expect_true(all(ce()$res$status == "not_applicable"))
    })
})

test_that("a shock program pairs its cost with the same member-years", {
  rows <- data.frame(
    scenario = "Hist", member = "Historical", sim_year = 1:4,
    total_cost = c(10, 20, 30, 40) * 1000, activated = TRUE
  )
  summary <- data.frame(
    scenario = c("Hist", "SSP"), activation_freq = c(0.25, 0.5),
    mean_total_cost = c(25000, 50000), cost_1_in_20 = NA_real_
  )
  args <- ce_args(
    sp = ce_sp(
      sp_type = "shock", payments_per_activation = 1L, trigger_type = "weather",
      trigger_variable = "t", trigger_direction = "above", trigger_value = 30,
      payout_scope = "local"
    ),
    shock_summary = shiny::reactiveVal(list(rows = rows, summary = summary)),
    annual_effects = shiny::reactiveVal(data.frame(
      scenario = "Hist", model_id = "Historical", sim_year = 1:4,
      value = c(-0.05, -0.05, -0.05, NA)
    ))
  )
  testServer(mod_3_10_cost_effectiveness_server, args = args, {
    x <- ce()
    expect_true(x$shock)
    a <- x$res[x$res$basis == "Average year", ]
    # Year 4 has no finite effect, so only years 1 to 3 enter the cost: 20,000
    expect_equal(a$cost, 20000)
    expect_equal(a$value, 20000 / 20)
    # No adverse-year support was supplied, so the tail is not available
    expect_identical(x$res$status[x$res$basis == "1-in-20 year"], "unavailable")
    expect_match(as.character(output$context_ui$html), "Program: shock-responsive")
    expect_match(as.character(output$climate_ui$html), "Climate-adjusted cost")
    d <- .sp_climate_cost_display(climate(), "PPP")
    expect_identical(d[["Cost change vs historical"]], c("Reference", "+100.0%"))
    expect_identical(d[["Change vs historical"]], c("Reference", "+25.0 pp"))
  })
})

test_that("a regular program has no climate-adjusted section", {
  testServer(mod_3_10_cost_effectiveness_server, args = ce_args(), {
    expect_null(climate())
    expect_null(output$climate_ui$html)
  })
})

test_that("the tab registers its exports", {
  testServer(mod_3_10_cost_effectiveness_server, args = ce_args(), {
    items <- wise_export_items(session)
    keys <- vapply(items, `[[`, character(1), "key")
    expect_true(all(c("policy_cost_effectiveness", "policy_shock_climate_cost") %in% keys))
    # The numeric table carries its context (metric, line, admin share, currency)
    tbl <- items[[match("policy_cost_effectiveness", keys)]]$fun()
    expect_true(all(c("metric", "poverty_line", "admin_share", "currency", "program") %in% names(tbl)))
    expect_equal(tbl$poverty_line[[1L]], 3)
  })
})
