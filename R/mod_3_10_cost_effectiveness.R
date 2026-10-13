#' 3_10_cost_effectiveness UI Function
#'
#' @description The Step 3 cost-effectiveness tab (P2-2): what the cash
#'   transfer costs and what the Results effect on the selected metric buys, in
#'   an average year and a 1-in-20 year, for regular and shock-responsive
#'   programs.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_10_cost_effectiveness_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("stale_banner_ui")),
    shiny::h4(
      "What does the cash transfer cost, and what does it buy?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::uiOutput(ns("scope_ui")),
      shiny::uiOutput(ns("context_ui")),
      shiny::uiOutput(ns("ce_body_ui")),
      shiny::uiOutput(ns("ce_note_ui"))
    ),
    shiny::uiOutput(ns("climate_ui"))
  )
}

#' 3_10_cost_effectiveness Server Functions
#'
#' Joins the program's annual cost to the Results effect (`sp_cost_effectiveness()`).
#' It follows the Results metric and poverty line (plan rule 15) and prints
#' them. Cash transfers only (decision 5).
#'
#' @param id Module id.
#' @param sp_scenario,infra_scenario,digital_scenario,labor_scenario,education_scenario
#'   Reactive scenario lists of the finished run.
#' @param baseline_svy,policy_svy Reactive survey frames of the run.
#' @param shock_summary Reactive list (`rows`, `summary`, ...) of a shock run.
#' @param metric_context Reactive Results metric metadata.
#' @param effect_summary Reactive expected paired effect per scenario.
#' @param annual_effects Reactive paired effect per scenario, member and year.
#' @param metric_decomposition Reactive metric decomposition (tails, supports).
#' @param poverty_line Reactive Results poverty line (outcome currency per day).
#' @param hist_label Reactive label of the historical scenario.
#' @param analysis_unit Reactive analysis unit.
#' @param stale Reactive; TRUE when the run is out of date.
#'
#' @noRd
mod_3_10_cost_effectiveness_server <- function(id,
                                               sp_scenario = reactive(NULL),
                                               infra_scenario = reactive(NULL),
                                               digital_scenario = reactive(NULL),
                                               labor_scenario = reactive(NULL),
                                               education_scenario = reactive(NULL),
                                               baseline_svy = reactive(NULL),
                                               policy_svy = reactive(NULL),
                                               shock_summary = reactive(NULL),
                                               metric_context = reactive(NULL),
                                               effect_summary = reactive(NULL),
                                               annual_effects = reactive(NULL),
                                               metric_decomposition = reactive(NULL),
                                               poverty_line = reactive(NULL),
                                               hist_label = reactive("Historical"),
                                               analysis_unit = reactive("hh"),
                                               stale = reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session$userData$wise_step3_stale <- stale

    output$stale_banner_ui <- shiny::renderUI({
      if (isTRUE(stale())) .stale_banner("Step 3 cost-effectiveness", note = NULL)
    })

    output$scope_ui <- shiny::renderUI({
      shiny::tags$p(
        class = "diagnostic-note",
        shiny::tags$b("Scope. "),
        "Cash transfers only. Counted: the modelled transfer, the administration",
        "share set in Payment settings, and the modelled welfare and poverty",
        "effect from Results. Not counted: delivery of other levers, targeting",
        "surveys, financing, and benefits the model does not represent. The",
        "infrastructure, digital, labour and education levers have no unit costs,",
        "so they get no cost-effectiveness figure."
      )
    })

    # Everything the tab shows, computed once per state of the run.
    ce <- reactive({
      if (isTRUE(stale())) {
        return(list(status = "stale"))
      }
      sp <- sp_scenario()
      if (!has_sp_change(sp)) {
        return(list(status = "no_sp"))
      }
      mc <- metric_context()
      eff <- effect_summary()
      if (is.null(mc) || !is.data.frame(eff) || !nrow(eff)) {
        return(list(status = "no_effect"))
      }
      if (!identical(mc$availability %||% "ok", "ok")) {
        return(list(status = "no_effect", reason = mc$reason))
      }
      au <- analysis_unit() %||% "hh"
      currency <- .sp_currency(sp$currency)
      admin <- .sp_admin_share(sp)
      shock <- identical(sp$sp_type, "shock")
      decomp <- metric_decomposition()
      effects <- sp_effect_by_scenario(eff, decomp$return_period)

      costs <- if (shock) {
        s <- shock_summary()
        ae <- annual_effects()
        matched <- if (is.data.frame(ae)) ae[is.finite(ae$value), , drop = FALSE]
        sp_shock_cost_by_scenario(
          s$rows, matched = matched, supports = decomp$adverse_support,
          historical = hist_label(), probability = 0.05
        )
      } else {
        total <- .sp_transfer_totals(policy_svy(), au, currency, admin)$total_cost
        data.frame(
          scenario = effects$scenario, cost_avg = total, cost_tail = total,
          stringsAsFactors = FALSE
        )
      }
      other_levers <- has_infra_change(infra_scenario()) ||
        has_digital_change(digital_scenario()) ||
        has_labor_change(labor_scenario()) ||
        has_education_change(education_scenario())
      line <- poverty_line()
      res <- sp_cost_effectiveness(
        effects, costs, mc$method,
        pop = .sp_ce_population(baseline_svy(), au),
        poverty_line = line, level_unit = mc$level_unit, sp_only = !other_levers
      )
      list(
        status = "ok", res = res, currency = currency, admin = admin, shock = shock,
        method = mc$method, label = mc$label, line = line,
        uses_line = mc$method %in% c("headcount_ratio", "gap")
      )
    })

    output$context_ui <- shiny::renderUI({
      x <- ce()
      if (!identical(x$status, "ok")) {
        return(NULL)
      }
      prefix <- .sp_currency_prefix(x$currency)
      shiny::tags$p(
        class = "text-muted small",
        paste(
          c(
            paste0("Program: ", if (x$shock) "shock-responsive" else "regular"),
            paste0("Metric: ", x$label),
            if (x$uses_line) {
              if (is.finite(suppressWarnings(as.numeric(x$line)[1L]))) {
                paste0("Poverty line: ", prefix, fmt_num(x$line, 2), " per day (Results)")
              } else {
                "Poverty line: not available"
              }
            },
            paste0("Administration: ", fmt_num(100 * x$admin, 1), "% of cost")
          ),
          collapse = " · "
        )
      )
    })

    empty_message <- function(x) {
      switch(x$status,
        stale = "Results are out of date. Re-run Step 3.",
        no_sp = paste(
          "No cash transfer to evaluate. Configure a transfer in Social protection",
          "and run Step 3."
        ),
        no_effect = x$reason %||% paste(
          "The policy effect is not available yet, so there is nothing to divide",
          "the cost by."
        ),
        NULL
      )
    }

    ce_display <- reactive({
      x <- ce()
      if (!identical(x$status, "ok")) {
        return(NULL)
      }
      .sp_ce_display(x$res, x$currency)
    })

    output$ce_body_ui <- shiny::renderUI({
      x <- ce()
      if (!identical(x$status, "ok")) {
        return(shiny::div(
          class = "empty-state",
          shiny::tags$p(empty_message(x))
        ))
      }
      shiny::tagList(
        shiny::div(
          class = "wise-reactable-controls",
          wise_reactable_csv_button(ns("ce_table"), "policy_cost_effectiveness")
        ),
        reactable::reactableOutput(ns("ce_table"))
      )
    })
    outputOptions(output, "ce_body_ui", suspendWhenHidden = FALSE)

    output$ce_table <- reactable::renderReactable({
      df <- ce_display()
      req(!is.null(df))
      .wise_diag_reactable(df)
    })

    output$ce_note_ui <- shiny::renderUI({
      x <- ce()
      if (!identical(x$status, "ok")) {
        return(NULL)
      }
      shiny::tags$ul(
        class = "diagnostic-note",
        if (identical(x$method, "headcount_ratio")) {
          shiny::tags$li(
            "Net people lifted is the net change in the number of poor. Households",
            "that fall into poverty offset those that leave it; gross movements",
            "are not modelled."
          )
        },
        shiny::tags$li(
          "Average year: the equal-model mean over the simulated years. 1-in-20",
          "year: the baseline-anchored adverse year used in Results",
          if (x$shock) {
            ", with the program's cost in the same member-years."
          } else {
            "; a regular program costs the same every year."
          }
        ),
        shiny::tags$li(
          "The interval reflects uncertainty in the estimated weather",
          "coefficients only. \"Not bounded\" means the interval reaches zero.",
          "\"Not applicable\" means no meaningful ratio exists (no effect, a wrong",
          "sign, or no spending)."
        ),
        shiny::tags$li(
          "Targeting errors come from one seeded draw. The model is static and",
          "annual: it has no poverty traps, price effects or multipliers."
        )
      )
    })

    # Shock programs: the bill under each climate scenario against history ----

    climate <- reactive({
      x <- ce()
      s <- shock_summary()
      if (!identical(x$status, "ok") || !isTRUE(x$shock) || is.null(s)) {
        return(NULL)
      }
      sp_shock_climate_cost(s$summary, reference = hist_label())
    })

    output$climate_ui <- shiny::renderUI({
      d <- climate()
      if (is.null(d)) {
        return(NULL)
      }
      shiny::tagList(
        shiny::h4("Climate-adjusted cost", class = "diagnostic-section-heading"),
        shiny::div(
          class = "results-section-card diagnostic-section-card",
          shiny::tags$p(
            class = "diagnostic-note",
            "The trigger is fixed on the historical record, so under climate",
            "change it fires more or less often and the bill moves with it. Costs",
            "include administration."
          ),
          shiny::div(
            class = "wise-reactable-controls",
            wise_reactable_csv_button(ns("climate_table"), "policy_shock_climate_cost")
          ),
          reactable::reactableOutput(ns("climate_table"))
        )
      )
    })
    outputOptions(output, "climate_ui", suspendWhenHidden = FALSE)

    output$climate_table <- reactable::renderReactable({
      d <- climate()
      req(!is.null(d))
      .wise_diag_reactable(.sp_climate_cost_display(d, ce()$currency))
    })

    # Exports ----

    wise_export_table(
      key = "policy_cost_effectiveness",
      label = "Cost-effectiveness of the cash transfer",
      step = 3L,
      fun = function() {
        x <- ce()
        if (!identical(x$status, "ok")) {
          return(NULL)
        }
        cbind(
          x$res,
          metric = x$method, poverty_line = suppressWarnings(as.numeric(x$line)[1L]),
          admin_share = x$admin, currency = x$currency,
          program = if (x$shock) "shock-responsive" else "regular"
        )
      },
      stale = stale,
      description = paste(
        "Annual cost, net change in the number of poor or value of the change,",
        "and the ratio per scenario, in the average and a 1-in-20 year, at the",
        "Results metric and poverty line."
      )
    )
    wise_export_table(
      key = "policy_shock_climate_cost",
      label = "Shock-responsive climate-adjusted cost",
      step = 3L,
      fun = function() climate(),
      stale = stale,
      description = paste(
        "Activation frequency and mean annual cost per scenario and their change",
        "against the historical scenario, with the trigger fixed on history."
      )
    )
  })
}
