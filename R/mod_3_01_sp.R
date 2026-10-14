#' 3_01_sp UI Function
#'
#' @description A shiny Module. Social protection scenario configuration.
#'   Allows the user to define a cash transfer program (shock-responsive
#'   or regular) with budget, targeting, transfer amount, timing and
#'   delivery parameters to be applied in the policy simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_01_sp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI-32: live reach/cost summary, matching the Step 1 weather/model cards.
    uiOutput(ns("sp_reach_ui")),

    # Program type - always visible ----
    uiOutput(ns("sp_type_ui")),

    # Collapsible configuration panel ----
    # Order: trigger (shock only), amount with its payment settings, targeting
    uiOutput(ns("sp_trigger_ui")),
    uiOutput(ns("sp_budget_amount_ui")),
    uiOutput(ns("sp_timing_ui")),
    uiOutput(ns("sp_targeting_ui"))
  )
}

# Patch an ionRangeSlider grid so major tick labels land exactly on the
# slider's step values. shiny's auto-grid (grid_num = n_steps /
# ceiling(n_steps / 10)) produces a fractional grid count for > 10 steps,
# which renders tick labels at irregular, off-step positions (e.g. a
# 5-60% slider showing a label at 54.5%). An explicit grid_num of the
# intended interval count keeps them evenly spaced and meaningful.
with_grid_num <- function(slider_tag, n) {
  for (i in seq_along(slider_tag$children)) {
    ch <- slider_tag$children[[i]]
    if (is.list(ch) && !is.null(ch$attribs) &&
      "data-grid-num" %in% names(ch$attribs)) {
      ch$attribs[["data-grid-num"]] <- n
      slider_tag$children[[i]] <- ch
      break
    }
  }
  slider_tag
}

#' 3_01_sp Server Functions
#'
#' @param id Module id.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from \code{mod_1_modelling_server()}.
#' @param survey_weather Reactive data frame of merged survey-weather data
#'   from \code{mod_1_modelling_server()}.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{sp_scenario}{Named list of social protection scenario parameters.}
#'   }
#'
#' @noRd
mod_3_01_sp_server <- function(id,
                               selected_outcome = reactive(NULL),
                               survey_weather = reactive(NULL),
                               variable_list = reactive(NULL),
                               analysis_unit = reactive("hh"),
                               hist_sim = reactive(NULL),
                               selected_weather = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: human-readable unit word ("individual" / "individuals" /
    # "Household" / "Households" / "Firm" / "Firms") driven by the user's
    # selection in mod_1_01_sample (`input$unit`).
    unit_word <- function(plural = TRUE, capitalize = FALSE, au = NULL) {
      au <- au %||% tryCatch(analysis_unit(), error = function(e) "hh")
      au <- if (is.null(au) || !nzchar(au)) "hh" else au
      word <- switch(au,
        ind  = if (plural) "individuals" else "individual",
        hh   = if (plural) "households" else "household",
        firm = if (plural) "firms" else "firm",
        if (plural) "households" else "household"
      )
      if (capitalize) {
        paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
      } else {
        word
      }
    }

    # Proxy variable candidates (numeric, non-missing for welfare rows) ----
    pmt_candidates <- reactive({
      svy <- survey_weather()
      vl <- variable_list()
      so <- selected_outcome()
      if (is.null(svy) || is.null(vl) || is.null(so) || nrow(vl) == 0) {
        return(character(0))
      }

      # Filter variable_list to ind/hh/firm/area level only
      level_mask <- vapply(seq_len(nrow(vl)), function(i) {
        isTRUE(vl$ind[i] == 1L) || isTRUE(vl$hh[i] == 1L) || isTRUE(vl$firm[i] == 1L) || isTRUE(vl$area[i] == 1L)
      }, logical(1))
      cands <- vl$name[level_mask]

      # Intersect with survey columns
      cands <- intersect(cands, names(svy))
      if (length(cands) == 0) {
        return(character(0))
      }

      # Keep only numeric vars with no NAs where outcome is non-missing
      outcome_rows <- !is.na(svy[[so$name]])
      keep <- Filter(function(v) {
        col <- svy[[v]]
        is.numeric(col) && !any(is.na(col[outcome_rows]))
      }, cands)

      # Return named character: display_label -> variable_name
      setNames(keep, vapply(keep, function(v) {
        lbl <- vl$label[vl$name == v]
        if (length(lbl) > 0 && nzchar(lbl[[1]])) lbl[[1]] else v
      }, character(1)))
    })

    # 1. program type ----

    output$sp_type_ui <- renderUI({
      tagList(
        tags$div(
          class = "sp-program-label",
          tags$span(
            class = "sp-inline-label",
            tags$i(class = "fa fa-hand-holding-dollar me-1"),
            "Program"
          )
        ),
        tags$div(
          class = "sp-program-choice",
          pill_toggle(
            inputId = ns("sp_type"),
            label = NULL,
            aria_label = "Program type",
            choices = c(
              "Regular"          = "regular",
              "Shock-responsive" = "shock"
            ),
            selected = "regular"
          ),
          tags$span(class = "sp-program-suffix", "cash transfer")
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # 2. Trigger (shock-responsive only) ----
    #
    # Weather variables a trigger can use: continuous columns of the historical
    # exposure (binned variables reach the model as categories, see
    # `.sp_trigger_variables()`). Read from the baseline survey so no exposure
    # table has to be rebuilt just to fill a dropdown.
    trigger_candidates <- reactive({
      hs <- tryCatch(hist_sim(), error = function(e) NULL)
      cols <- hs$pipeline$weather_exposure$weather_columns
      svy <- hs$svy
      if (is.null(cols) || is.null(svy)) {
        return(character(0))
      }
      # A binned variable reaches the model as a category, but Step 2 keeps its
      # continuous value in a companion `<var>_cont` column (P2-4): offer that.
      companion <- vapply(cols, function(v) {
        base <- sub("_cont$", "", v)
        grepl("_cont$", v) && base %in% cols && !is.numeric(svy[[base]])
      }, logical(1))
      cols <- cols[companion | (cols %in% names(svy) & vapply(
        cols, function(v) is.numeric(svy[[v]]), logical(1)
      ))]
      vl <- tryCatch(variable_list(), error = function(e) NULL)
      labels <- vapply(cols, function(v) {
        base <- if (v %in% names(svy)) v else sub("_cont$", "", v)
        lbl <- if (is.data.frame(vl)) vl$label[vl$name == base] else character(0)
        lbl <- if (length(lbl) && !is.na(lbl[[1]]) && nzchar(lbl[[1]])) lbl[[1]] else base
        if (identical(base, v)) lbl else paste0(lbl, " (continuous)")
      }, character(1))
      stats::setNames(cols, labels)
    })

    # Offer only the impact levels the survey frame supports (P2-5); keep the
    # current choice when it is still offered.
    observe({
      svy <- tryCatch(hist_sim()$svy, error = function(e) NULL)
      choices <- .sp_impact_level_choices(svy)
      current <- shiny::isolate(input$trigger_level)
      updateSelectInput(
        session, "trigger_level", choices = choices,
        selected = if (!is.null(current) && current %in% choices) current else "national"
      )
    })

    output$sp_trigger_ui <- renderUI({
      cands <- trigger_candidates()
      conditionalPanel(
        condition = paste0("input['", ns("sp_type"), "'] == 'shock'"),
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-bolt me-1"),
          "Trigger"
        ),
        config_flyout_block(
          ns("trigger_toggle"),
          "Trigger settings",
          toggle_label = "Trigger settings",
          display_label = textOutput(ns("trigger_summary"), inline = TRUE),
          if (length(cands) == 0) {
            no_data_warning(
              "No continuous weather variable is available. Run Step 2 first",
              "(a binned variable is used through its continuous value)."
            )
          } else {
            tagList(
              tags$p(
                class = "text-muted small",
                "Payments are made after the shock (ex post), in the survey",
                "location where the weather is measured. A household is paid in",
                "a year when its location and interview month pass the trigger;",
                "households of one location interviewed in different months can",
                "differ. Quantile-based welfare effects (RIF) do not respond to the",
                "payment."
              ),
              selectInput(
                ns("trigger_metric"),
                label = tags$span(
                  tags$i(class = "fa fa-ruler me-1"),
                  "Trigger measures",
                  info_popover(
                    title = "What the trigger measures",
                    tags$p(
                      tags$b("Weather variable:"), "a hazard index, as in the",
                      "response check below."
                    ),
                    tags$p(
                      tags$b("Impact:"), "modelled welfare loss, poverty rate or number",
                      "of poor, read from the central predictions of the year, nationally",
                      "or per survey location (interview months pooled). The threshold",
                      "comes from the historical years and is applied unchanged to climate",
                      "members. A real program would need an impact estimate this timely,",
                      "so read it as a benchmark. Loss is the percent of the households'",
                      "own typical welfare. Poverty needs the Step 2 poverty line, and per",
                      "location at least", SP_IMPACT_MIN_HH, "sampled households per",
                      "location. For a return period the poverty rate and the number of",
                      "poor give the same years."
                    )
                  )
                ),
                choices = c(
                  "Weather variable" = "weather",
                  "Modelled welfare loss (%)" = "loss",
                  "Poverty rate (%)" = "poverty_rate",
                  "Number of poor (people)" = "poor_count"
                ),
                selected = "weather"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("trigger_metric"), "'] != 'weather'"),
                selectInput(
                  ns("trigger_level"),
                  label = tags$span(
                    "Read at",
                    info_popover(
                      title = "Impact level",
                      tags$p(
                        "National uses one series. Administrative levels use the GAUL",
                        "codes in the survey data; only the levels the data supports",
                        "are listed. A survey location is a sampling cluster, too",
                        "small for a reliable poverty rate."
                      )
                    )
                  ),
                  choices = c(
                    "National" = "national", "Administrative level 1" = "admin1",
                    "Administrative level 2" = "admin2", "Survey location" = "location"
                  ),
                  selected = "national"
                ),
                tags$p(
                  class = "text-muted small",
                  "Pays when the metric is at or above the threshold (more loss or",
                  "more poor is adverse)."
                )
              ),
              pill_toggle(
                inputId = ns("trigger_type"),
                label = "Trigger on",
                aria_label = "Trigger type",
                choices = c("Weather level" = "weather", "Return period" = "return_period"),
                selected = "weather"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("trigger_metric"), "'] == 'weather'"),
              selectInput(
                ns("trigger_variable"),
                label = tags$span(
                  tags$i(class = "fa fa-cloud-sun-rain me-1"),
                  "Weather variable",
                  info_popover(
                    title = "Weather variable",
                    tags$p(
                      "Only continuous weather variables can trigger payments.",
                      "The value is read in the unit the model uses for the",
                      "variable: physical units, deviation from the reference",
                      "mean, or a standardized anomaly."
                    )
                  )
                ),
                choices = cands, selected = cands[[1]]
              ),
              textOutput(ns("trigger_unit"), container = function(...) {
                tags$p(class = "text-muted small", ...)
              }),
              uiOutput(ns("trigger_response")),
              pill_toggle(
                inputId = ns("trigger_direction"),
                label = "Pay when the value is",
                aria_label = "Trigger direction",
                choices = c(
                  "At or above" = "above", "At or below" = "below",
                  "Either end" = "either"
                ),
                selected = "above"
              )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("trigger_type"), "'] != 'return_period'"),
                conditionalPanel(
                  condition = paste0(
                    "input['", ns("trigger_direction"), "'] == 'either' && input['",
                    ns("trigger_metric"), "'] == 'weather'"
                  ),
                  numericInput(
                    ns("trigger_value_low"),
                    label = tags$span(
                      tags$i(class = "fa fa-gauge me-1"),
                      "Low threshold",
                      info_popover(
                        title = "Either extreme",
                        tags$p(
                          "Pays when the value is at or below the low threshold or at",
                          "or above the high threshold, for variables that hurt at",
                          "both ends (very dry and very wet)."
                        )
                      )
                    ),
                    value = NA
                  )
                ),
                numericInput(
                  ns("trigger_value"),
                  label = tags$span(
                    tags$i(class = "fa fa-gauge-high me-1"),
                    "Threshold value",
                    tags$span(
                      class = "text-muted small ms-1",
                      textOutput(ns("trigger_value_hint"), inline = TRUE)
                    ),
                    info_popover(
                      title = "Threshold value",
                      tags$p(
                        "One value for every location. It fires more often where the",
                        "variable is typically more extreme, so cost concentrates",
                        "there. Use a return period for the same activation chance",
                        "everywhere."
                      )
                    )
                  ),
                  value = NA
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("trigger_type"), "'] == 'return_period'"),
                numericInput(
                  ns("trigger_return_period_years"),
                  label = tags$span(
                    tags$i(class = "fa fa-hourglass-half me-1"),
                    "Return period (1 in x years)",
                    info_popover(
                      title = "Return period",
                      tags$p(
                        "Each location uses its own 1-in-x level, taken from its",
                        "historical years and applied unchanged to every climate",
                        "member. A 1-in-x trigger needs at least x historical years.",
                        "At either extreme the chance is shared between both ends",
                        "(1 in 2x each), which needs 2x years."
                      )
                    )
                  ),
                  value = 10, min = 1, step = 1
                ),
                textOutput(ns("trigger_support"), container = function(...) {
                  tags$p(class = "text-muted small", ...)
                })
              ),
              selectInput(
                ns("payout_scope"),
                label = tags$span(
                  tags$i(class = "fa fa-map-location-dot me-1"),
                  "Who is paid",
                  info_popover(
                    title = "Payout scope",
                    tags$p(
                      tags$b("Triggered locations only:"),
                      "each location decides for itself."
                    ),
                    tags$p(
                      tags$b("National gate, triggered locations:"),
                      "payments start only in a year when enough of the population",
                      "lives in locations that pass the trigger; then only those",
                      "locations are paid."
                    ),
                    tags$p(
                      tags$b("National gate, all targeted households:"),
                      "once the gate opens, every targeted household is paid,",
                      "wherever it lives. Bills are rarer and larger."
                    )
                  )
                ),
                choices = c(
                  "Triggered locations only" = "local",
                  "National gate, triggered locations" = "national_triggered",
                  "National gate, all targeted households" = "national_all"
                ),
                selected = "local"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("payout_scope"), "'] != 'local'"),
                sliderInput(
                  ns("national_k_pct"),
                  label = "Population in triggered locations needed (%)",
                  min = 5, max = 100, value = 20, step = 5, post = "%"
                )
              ),
              selectInput(
                ns("dyn_geo"),
                label = tags$span(
                  tags$i(class = "fa fa-location-crosshairs me-1"),
                  "Within triggered locations",
                  info_popover(
                    title = "Worst-affected locations",
                    tags$p(
                      "Added on top of the targeting rule and the payout scope.",
                      "\"Worst-affected share\" pays only the most severe triggered",
                      "locations of each year, ranked by how far the weather passes the",
                      "trigger in units of the location's own historical standard",
                      "deviation, until the chosen share of the triggered population",
                      "is covered. Not available when all targeted households are paid."
                    )
                  )
                ),
                choices = c(
                  "All triggered locations" = "all",
                  "Worst-affected share" = "worst_share"
                ),
                selected = "all"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("dyn_geo"), "'] == 'worst_share'"),
                sliderInput(
                  ns("dyn_geo_pct"),
                  label = "Triggered population paid (%)",
                  min = 5, max = 100, value = 50, step = 5, post = "%"
                )
              ),
              checkboxInput(
                ns("dyn_welfare"),
                label = tags$span(
                  "Also select on predicted welfare at the trigger",
                  info_popover(
                    title = "Predicted welfare (oracle)",
                    tags$p(
                      "A household is selected when the model's prediction of its own",
                      "welfare in that year is at or below the chosen share of the",
                      "households' historical mean welfare distribution (fixed for the",
                      "run). This knows each household's modelled welfare in the shock",
                      "year, which a real registry would not: read the result as an",
                      "upper bound and use larger error rates. Errors are drawn per",
                      "household and year with the run seed, the same in every scenario."
                    )
                  )
                ),
                value = FALSE
              ),
              conditionalPanel(
                condition = paste0("input['", ns("dyn_welfare"), "']"),
                sliderInput(
                  ns("dyn_welfare_pct"),
                  label = "Poorest at the trigger (bottom x%)",
                  min = 5, max = 60, value = 20, step = 5, post = "%"
                ),
                sliderInput(
                  ns("dyn_incl_error_pct"),
                  label = "Inclusion error at the trigger (%)",
                  min = 0, max = 50, value = 20, step = 5, post = "%"
                ),
                sliderInput(
                  ns("dyn_excl_error_pct"),
                  label = "Exclusion error at the trigger (%)",
                  min = 0, max = 50, value = 20, step = 5, post = "%"
                )
              ),
              pill_toggle(
                inputId = ns("transfer_timing"),
                label = "Payments are made",
                aria_label = "Payment timing",
                choices = c("After the shock (ex post)" = "expost", "On a forecast (anticipatory)" = "anticipatory"),
                selected = "expost"
              ),
              conditionalPanel(
                condition = paste0("input['", ns("transfer_timing"), "'] == 'anticipatory'"),
                sliderInput(
                  ns("forecast_pod_pct"),
                  label = tags$span(
                    "Forecast detection (POD, %)",
                    info_popover(
                      title = "Forecast skill",
                      tags$p(
                        "In a location-year where the weather really passes the trigger,",
                        "the forecast fires with this probability. A missed event pays",
                        "nobody. The draw is seeded by location and year."
                      ),
                      tags$p(
                        "100 % detection, 0 % false alarms and a multiplier of 1 give",
                        "exactly the ex-post design."
                      )
                    )
                  ),
                  min = 0, max = 100, value = 100, step = 5, post = "%"
                ),
                sliderInput(
                  ns("forecast_far_pct"),
                  label = tags$span(
                    "False-alarm rate (%)",
                    info_popover(
                      title = "False alarms",
                      tags$p(
                        "Where the weather does not pass the trigger, the forecast still",
                        "fires with this probability. A false alarm pays the full cost",
                        "(transfers and administration) and the households receive the",
                        "cash, but the multiplier below does not apply."
                      )
                    )
                  ),
                  min = 0, max = 100, value = 0, step = 5, post = "%"
                ),
                numericInput(
                  ns("effect_multiplier"),
                  label = tags$span(
                    "Effectiveness multiplier m",
                    info_popover(
                      title = "Effectiveness multiplier",
                      tags$p(
                        "Scales the welfare effect of a payment made before an event that",
                        "occurs (not its cost). It is an assumption: the model predicts",
                        "annual welfare and cannot show the mechanism (avoided asset",
                        "sales, scarring), so leave it at 1 unless you have evidence.",
                        "Diagnostics shows the breakeven m at which the forecast design",
                        "matches the ex-post design."
                      )
                    )
                  ),
                  value = 1, min = 0.1, max = 10, step = 0.1
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("transfer_timing"), "'] != 'anticipatory'"),
                numericInput(
                  ns("timeliness_weeks"),
                  label = tags$span(
                    "Delivery delay (weeks)",
                    info_popover(
                      title = "Delivery delay",
                      tags$p(
                        "Weeks between the shock and the payment. With a loss of",
                        "effectiveness per week below, the welfare effect is scaled by",
                        "1 - weeks x loss per week (never below 0). The cost does not",
                        "change. Both default to 0, which means no delay effect."
                      )
                    )
                  ),
                  value = 0, min = 0, max = 52, step = 1
                ),
                numericInput(
                  ns("delay_loss_pct_per_week"),
                  label = "Effectiveness lost per week of delay (%)",
                  value = 0, min = 0, max = 100, step = 1
                )
              ),
              sliderInput(
                ns("payments_per_activation"),
                label = tags$span(
                  tags$i(class = "fa fa-hashtag me-1"),
                  "Payments per activation",
                  info_popover(
                    title = "Payments per activation",
                    tags$p(
                      "Each payment is the amount per transfer set above. Annual",
                      "support in an activated year = amount \u{00d7} payments."
                    )
                  )
                ),
                min = 1, max = 12, value = 1, step = 1
              ),
              numericInput(
                ns("loss_event_pct"),
                label = tags$span(
                  tags$i(class = "fa fa-chart-line me-1"),
                  "Loss event (% of welfare)",
                  info_popover(
                    title = "Loss event",
                    tags$p(
                      "Scoring only: it changes no payment and no welfare result, so",
                      "changing it does not mark results stale and Diagnostics",
                      "re-scores the finished run. A location has a",
                      "loss event in a year when its households' modelled weather",
                      "loss is at least this share of their own typical welfare.",
                      "Paying without a loss event is a false positive; a loss",
                      "event without payment is a false negative."
                    )
                  )
                ),
                value = 10, min = 1, max = 50, step = 1
              )
            )
          }
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # The trigger's own validation message, or its summary beside the button.
    output$trigger_summary <- renderText({
      spec <- sp_scenario_spec()
      problem <- .sp_shock_problem(spec)
      if (!is.null(problem)) {
        return(problem)
      }
      trig <- .sp_spec_trigger(spec)
      impact_label <- c(
        loss = "modelled loss (%)", poverty_rate = "poverty rate (%)",
        poor_count = "number of poor"
      )[spec[["trigger_metric"]]]
      either <- identical(trig$direction, "either")
      side <- if (either) {
        "beyond"
      } else if (identical(trig$direction, "below")) {
        "at or below"
      } else {
        "at or above"
      }
      level <- if (identical(spec$trigger_type, "return_period")) {
        paste0(
          "its 1-in-", spec$trigger_return_period_years, "-year level",
          if (either) " at either end" else ""
        )
      } else if (either) {
        paste(spec$trigger_value_low, "or", spec$trigger_value)
      } else {
        paste(spec$trigger_value)
      }
      layers <- c(
        if (identical(spec[["dyn_geo"]], "worst_share")) {
          paste0("worst-affected ", spec$dyn_geo_pct, "% of triggered population")
        },
        if (isTRUE(spec[["dyn_welfare"]])) {
          paste0("poorest ", spec$dyn_welfare_pct, "% by predicted welfare")
        },
        if (identical(spec[["transfer_timing"]], "anticipatory")) {
          paste0("forecast POD ", spec$forecast_pod_pct, "%, false alarms ", spec$forecast_far_pct, "%")
        }
      )
      paste0(
        if (is.na(impact_label)) {
          spec$trigger_variable
        } else {
          paste0(impact_label, ", ", spec[["trigger_level"]])
        }, " ", side, " ", level, "; ",
        spec$payments_per_activation, " payment(s)",
        if (length(layers)) paste0("; ", paste(layers, collapse = ", "))
      )
    })

    # Unit the thresholds are read in (the Step 2 transformation of the variable).
    output$trigger_unit <- renderText({
      unit <- .sp_trigger_unit(
        input$trigger_variable,
        tryCatch(selected_weather(), error = function(e) NULL)
      )
      if (nzchar(unit)) paste0("Thresholds are in: ", unit, ".") else ""
    })

    # Response check (P2-1): modelled loss by decile of the chosen variable, so
    # the user sees which end of the variable is adverse before setting a
    # direction. Depends on the variable only, not on the other trigger fields.
    trigger_response <- reactive({
      var <- input$trigger_variable
      hs <- tryCatch(hist_sim(), error = function(e) NULL)
      ex <- hist_exposure()
      if (is.null(var) || !nzchar(var) || is.null(hs$pipeline) || is.null(ex)) {
        return(NULL)
      }
      tryCatch(
        sp_response_check(
          hs$pipeline, ex, var,
          is_log = identical(hs$so$transform %||% "", "log")
        ),
        error = function(e) list(problem = wise_user_error(e, "Response check"))
      )
    })

    output$trigger_response <- renderUI({
      r <- trigger_response()
      if (is.null(r)) {
        return(NULL)
      }
      if (!is.null(r$problem)) {
        return(tags$p(class = "text-muted small", r$problem))
      }
      d <- r$deciles
      pct <- function(x) fmt_num(100 * x, digits = 1, suffix = "%", na = "n/a")
      st <- sp_shock_state()
      basis <- if (is.list(st) && is.null(st$problem) &&
        is.finite(st$false_negative_rate %||% NA_real_)) {
        tags$p(
          class = "text-muted small",
          "Current trigger: misses ", pct(st$false_negative_rate),
          " of loss events, ", pct(st$false_positive_rate %||% NA_real_),
          " false positives (loss event as set below)."
        )
      }
      tags$div(
        class = "sp-response-check",
        tags$p(class = "small mb-1", tags$b("Response check. "), r$hint),
        tags$details(
          class = "small mb-1",
          tags$summary("Loss by decile"),
          tags$table(
            class = "table table-sm small mb-1",
            tags$caption(
              class = "small text-muted",
              "Mean modelled loss by decile of ", r$variable,
              " over household-years (negative = loss, survey-weighted)."
            ),
            tags$thead(tags$tr(
              tags$th("Decile"), tags$th("Range"), tags$th("Mean loss")
            )),
            tags$tbody(lapply(seq_len(nrow(d)), function(i) {
              tags$tr(
                tags$td(d$decile[[i]]),
                tags$td(paste(
                  fmt_num(d$min_value[[i]], digits = 2), "to",
                  fmt_num(d$max_value[[i]], digits = 2)
                )),
                tags$td(pct(d$mean_loss[[i]]))
              )
            }))
          )
        ),
        basis
      )
    })

    output$trigger_value_hint <- renderText({
      if (identical(input$trigger_direction, "either")) "(high threshold)" else ""
    })

    # Record length behind a return-period trigger.
    output$trigger_support <- renderText({
      tbl <- hist_exposure()$table
      var <- input$trigger_variable
      if (!identical(input$trigger_metric %||% "weather", "weather")) {
        return("")
      }
      if (is.null(tbl) || is.null(var) || !var %in% names(tbl) ||
        !"sim_year" %in% names(tbl)) {
        return("")
      }
      years <- length(unique(tbl$sim_year[is.finite(as.numeric(tbl[[var]]))]))
      either <- identical(input$trigger_direction, "either")
      paste0(
        "Historical record: ", years, " years, so a trigger of up to 1 in ",
        if (either) years %/% 2L else years,
        " years is supported", if (either) " at either extreme" else "",
        ". Annual probability: ",
        format(round(100 / max(1, suppressWarnings(as.numeric(
          input$trigger_return_period_years %||% 10
        ))), 1), nsmall = 1), "%."
      )
    })

    # 3. Targeting ----

    output$sp_targeting_ui <- renderUI({
      tags$div(
        class = "sp-targeting",
        tags$div(
          class = "sp-targeting-label",
          tags$span(
            class = "sp-inline-label",
            tags$i(class = "fa fa-crosshairs me-1"),
            "Targeting"
          )
        ),
        tags$div(
          class = "sp-targeting-select",
          selectInput(
            inputId = ns("targeting"),
            label = tags$span(class = "visually-hidden", "Targeting"),
            choices = stats::setNames(
              c("universal", "exante_poor", "pmt"),
              c(
                "Universal",
                "Welfare below threshold (ex-ante poor)",
                paste(
                  unit_word(plural = FALSE, capitalize = TRUE),
                  "characteristics (Proxy)"
                )
              )
            ),
            selected = "universal"
          )
        ),

        # Threshold for ex-ante poor targeting
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] == 'exante_poor'"),
          sliderInput(
            inputId = ns("targeting_threshold_pct"),
            label = tags$span(
              tags$i(class = "fa fa-users me-1"),
              "Poorest (bottom x%)"
            ),
            min = 5, max = 60, value = 20, step = 5, post = "%"
          ) |>
            # 11 intervals -> majors at 5, 10, ..., 60
            with_grid_num(11)
        ),

        # Targeting details flyout (P0-1): proxy rule, errors and how errors
        # are placed. Universal targeting has no rule detail and no errors.
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] != 'universal'"),
          config_flyout_block(
            ns("targeting_toggle"),
            "Targeting details",
            toggle_label = "Targeting details",
            display_label = textOutput(ns("targeting_summary"), inline = TRUE),

            # Proxy variable and cutoff (only for Proxy targeting)
            conditionalPanel(
              condition = paste0("input['", ns("targeting"), "'] == 'pmt'"),
              uiOutput(ns("pmt_variable_ui")),
              uiOutput(ns("pmt_cutoff_ui"))
            ),

            # Inclusion/exclusion errors
            sliderInput(
              inputId = ns("inclusion_error_pct"),
              label = tags$div(
                tags$span(
                  tags$i(class = "fa fa-user-plus me-1"),
                  "Inclusion error (%)"
                ),
                tags$div(
                  class = "text-muted",
                  style = "font-size: 0.8em; font-weight: normal;",
                  paste0("Non-eligible ", unit_word(plural = TRUE), " included")
                )
              ),
              min = 0, max = 30, value = 10, step = 5, post = "%"
            ),
            sliderInput(
              inputId = ns("exclusion_error_pct"),
              label = tags$div(
                tags$span(
                  tags$i(class = "fa fa-user-minus me-1"),
                  "Exclusion error (%)"
                ),
                tags$div(
                  class = "text-muted",
                  style = "font-size: 0.8em; font-weight: normal;",
                  paste0("Eligible ", unit_word(plural = TRUE), " excluded")
                )
              ),
              min = 0, max = 30, value = 10, step = 5, post = "%"
            ),
            selectInput(
              inputId = ns("error_concentration"),
              label = tags$span(
                "Where errors fall",
                info_popover(
                  title = "Where errors fall",
                  tags$p(
                    "The error rates above fix how many", unit_word(plural = TRUE),
                    "are wrongly included or excluded. This setting chooses which",
                    "ones. \"Random\" picks any unit with equal chance.",
                    "The other settings make units close to the cutoff likelier to",
                    "be misclassified, as in proxy-means tests. Closeness is the",
                    "difference in welfare (or proxy) percentile rank, not weighted.",
                    "Proxy variables with only two values always use random errors."
                  ),
                  tags$p(
                    "Error rates are shares of the weighted population when the",
                    "survey has weights, and of sampled rows otherwise. The",
                    "realised rate matches the slider to within one row's weight."
                  )
                )
              ),
              choices = c(
                "Random" = "0",
                "Near the cutoff" = "5",
                "Mostly near the cutoff" = "20"
              ),
              selected = "0"
            )
          )
        )
      )
    })

    # Proxy variable selector ----
    output$pmt_variable_ui <- renderUI({
      cands <- pmt_candidates()
      if (length(cands) == 0) {
        return(no_data_warning(
          "No suitable Proxy variable found. No covariates with",
          "non-missing values found (for selected outcome)."
        ))
      }
      selectInput(
        ns("pmt_variable"),
        label = tags$span(
          tags$i(class = "fa fa-list me-1"),
          "Proxy variable"
        ),
        choices = cands,
        selected = cands[[1]]
      )
    })

    # Proxy cutoff selector (type depends on variable) ----
    output$pmt_cutoff_ui <- renderUI({
      v <- input$pmt_variable
      req(v)
      svy <- survey_weather()
      req(svy, v %in% names(svy))

      col <- svy[[v]]
      col <- col[!is.na(col)]
      uniq <- sort(unique(col))

      if (length(uniq) == 2 && all(uniq %in% c(0, 1))) {
        # Binary variable: choose target value
        pill_toggle(
          ns("pmt_cutoff"),
          label = paste(
            "Target", unit_word(plural = TRUE),
            "where variable equals"
          ),
          choices = c("0" = 0, "1" = 1),
          selected = 0
        )
      } else {
        # Continuous variable: choose threshold
        sliderInput(
          ns("pmt_cutoff"),
          label = paste(
            "Include", unit_word(plural = TRUE),
            "with value \u2264"
          ),
          min = floor(min(col)),
          max = ceiling(max(col)),
          value = stats::quantile(col, 0.2),
          step = if (diff(range(col)) > 10) 1 else 0.1
        )
      }
    })

    # 4. Budget / transfer amount (linked) ----
    #
    # Two budget modes:
    #   "budget_first"   - user sets total budget (net of admin cost)
    #                      -> transfer per HH = (budget * (1 - admin%)) / n_beneficiaries
    #   "transfer_first" - user sets transfer per HH
    #                      -> total budget = (amount * n_beneficiaries) / (1 - admin%)
    #
    # Admin cost reduces the amount available for direct transfers in both modes.

    output$sp_budget_amount_ui <- renderUI({
      currency <- tryCatch(
        {
          so <- selected_outcome()
          if (is.null(so) || nrow(so) == 0 || !"units" %in% names(so)) {
            "PPP"
          } else {
            as.character(so$units[[1]])
          }
        },
        error = function(e) "PPP"
      )
      if (is.na(currency) || !nzchar(currency)) currency <- "PPP"
      # The amount is in the outcome's currency; LCU amounts are in 2021 prices,
      # matching the "LCU (2021)" outcome option (see R2-BUG-04).
      is_lcu <- identical(.sp_currency(currency), "LCU")
      currency <- if (is_lcu) "2021 LCU" else currency

      tagList(
        # Amount and budget mode ----
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-money-bill-transfer me-1"),
          "Amount",
          info_popover(
            title = "Amount and budget mode",
            tags$p(
              tags$b("Per transfer"),
              "sets the amount paid to each recipient", unit_word(plural = FALSE),
              "per payment (or per activation in shock-responsive mode). Whether a",
              "recipient is a household or a person is set in Payment settings.",
              "The annual cost then depends on the number of recipients and",
              "payments per year."
            ),
            tags$p(
              tags$b("Total budget"),
              "sets the total annual amount available. The transfer per recipient is",
              "derived from the configured budget and recipient population."
            )
          )
        ),
        # A shock-responsive program pays a fixed amount per activation, so the
        # total-budget mode is offered for regular programs only (decision 12).
        conditionalPanel(
          condition = paste0("input['", ns("sp_type"), "'] != 'shock'"),
          tags$div(
            class = "sp-budget-mode",
            pill_toggle(
              inputId = ns("budget_mode"),
              label = NULL,
              aria_label = "Amount or budget mode",
              choices = stats::setNames(
                c("transfer_first", "budget_first"),
                c("Per transfer", "Total budget")
              ),
              selected = "transfer_first"
            )
          )
        ),

        # Amount entry ----
        conditionalPanel(
          condition = paste0(
            "input['", ns("budget_mode"), "'] == 'budget_first' && input['",
            ns("sp_type"), "'] != 'shock'"
          ),
          tags$div(
            class = "sp-inline-control",
            tags$label(
              class = "sp-inline-label",
              `for` = ns("budget_fixed"),
              tags$i(class = "fa fa-dollar-sign me-1"),
              paste0("Total annual budget (", currency, ")")
            ),
            numericInput(
              inputId = ns("budget_fixed"),
              label = NULL,
              value = 0, min = 0, step = 100000
            )
          )
        ),
        # Budget rule (shock-responsive programs only, P2-6)
        conditionalPanel(
          condition = paste0("input['", ns("sp_type"), "'] == 'shock'"),
          tags$label(
            class = "control-label",
            tags$i(class = "fa fa-scale-balanced me-1"),
            "Budget rule",
            info_popover(
              title = "Budget rule",
              tags$p(
                tags$b("Fixed amount:"),
                "every paid", unit_word(plural = FALSE),
                "gets the amount below in each activated year."
              ),
              tags$p(
                tags$b("Share of modelled loss:"),
                "a triggered location-month in a year gets a budget equal to",
                "the chosen share of the modelled weather loss of its paid",
                "households. The amount below is not used. Shared equally over the",
                "paid households (or people, per Payment settings)."
              ),
              tags$p(
                tags$b("What counts as loss:"),
                "a household's loss in a year is how far its modelled welfare",
                "falls below its own average over the historical years, in",
                "welfare units, with no random residual. Years above the average",
                "count as zero loss. The loss of households that are targeted",
                "and in the payout scope is summed with survey weights, so",
                "targeting errors and the payout scope carry through. Under the",
                "national gate with all targeted households, each location is",
                "still budgeted from its own loss."
              ),
              tags$p(
                tags$b("Annual cap:"),
                "an upper limit on total cost (including administration) in",
                "any one year. When the payments of a year would exceed it,",
                "all payments of that year are scaled down by the same",
                "proportion. Leave blank for no cap."
              )
            )
          ),
          pill_toggle(
            inputId = ns("budget_rule"),
            label = NULL,
            aria_label = "Budget rule",
            choices = stats::setNames(
              c("fixed", "loss_share"), c("Fixed amount", "Share of loss")
            ),
            selected = "fixed"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("budget_rule"), "'] == 'loss_share'"),
            tags$div(
              class = "sp-inline-control",
              tags$label(
                class = "sp-inline-label",
                `for` = ns("loss_share_pct"),
                tags$i(class = "fa fa-percent me-1"),
                "Share of modelled loss paid (%)"
              ),
              numericInput(
                inputId = ns("loss_share_pct"),
                label = NULL,
                value = 50, min = 1, max = 100, step = 5
              )
            )
          ),
          tags$div(
            class = "sp-inline-control",
            tags$label(
              class = "sp-inline-label",
              `for` = ns("annual_cap"),
              tags$i(class = "fa fa-lock me-1"),
              paste0("Annual cap on total cost (", currency, ", blank = none)")
            ),
            numericInput(
              inputId = ns("annual_cap"),
              label = NULL,
              value = NA, min = 0, step = 100000
            )
          )
        ),
        conditionalPanel(
          condition = paste0(
            "(input['", ns("budget_mode"), "'] == 'transfer_first' && input['",
            ns("sp_type"), "'] != 'shock') || (input['", ns("sp_type"),
            "'] == 'shock' && input['", ns("budget_rule"), "'] != 'loss_share')"
          ),
          tags$div(
            class = "sp-inline-control",
            tags$label(
              class = "sp-inline-label",
              `for` = ns("transfer_amount_usd"),
              tags$i(class = "fa fa-dollar-sign me-1"),
              paste0("Amount per transfer (", currency, ")")
            ),
            numericInput(
              inputId = ns("transfer_amount_usd"),
              label = NULL,
              value = 0, min = 0, step = 10
            )
          )
        )
      )
    })

    # 5. Frequency and timing ----
    #
    # If sp_type == "regular":
    #   - hide one-off vs regular radio (always regular)
    #   - hide anticipatory vs ex-post (not applicable)
    #   - hide timeliness (not applicable)
    #   - show only number of payments

    output$sp_timing_ui <- renderUI({
      tagList(config_flyout_block(
        ns("payment_toggle"),
        "Payment settings",
        toggle_label = "Payment settings",
        display_label = textOutput(ns("payment_summary"), inline = TRUE),

        # Number of payments - only for a regular program whose amount is set per
        # payment (a shock program sets payments per activation in its trigger)
        conditionalPanel(
          condition = paste0(
            "input['", ns("budget_mode"), "'] == 'transfer_first' && input['",
            ns("sp_type"), "'] != 'shock'"
          ),
          tags$label(
            class = "control-label",
            tags$i(class = "fa fa-clock me-1"),
            "Frequency and timing",
            info_popover(
              title = "Transfers per year",
              tags$p(
                "The transfer amount above is per payment. Annual support =",
                "payment amount \u00d7 transfers per year, which the simulation",
                "converts to a daily equivalent added to daily welfare.",
                "More transfers per year means a larger annual transfer for the",
                "same per-payment amount."
              )
            )
          ),
          tags$div(
            class = "sp-inline-slider",
            tags$span(
              class = "sp-inline-label",
              tags$i(class = "fa fa-hashtag me-1"),
              "Transfers per year"
            ),
            with_grid_num(
              sliderInput(
                inputId = ns("transfer_n_payments"),
                label = tags$span(class = "visually-hidden", "Transfers per year"),
                min = 2, max = 24, value = 6, step = 1
              ),
              11
            )
          )
        ),

        uiOutput(ns("amount_basis_ui")),

        numericInput(
          inputId = ns("admin_cost_pct"),
          label = tags$span(
            tags$i(class = "fa fa-building-columns me-1"),
            "Administration cost (% of total cost)",
            info_popover(
              title = "Administration cost",
              tags$p(
                "Share of total program spending used for delivery and",
                "administration. Only the rest reaches recipients, so welfare",
                "effects use the net transfer, while the cost shown includes",
                "administration."
              ),
              tags$p(
                "With an amount per payment, total cost = transfers /",
                "(1 - share). With a total budget, the budget is total cost and",
                "recipients receive (1 - share) of it.",
                "The default of 0 means no administration cost."
              )
            )
          ),
          value = 0, min = 0, max = 50, step = 1
        )
      ), tags$hr(style = "margin: 8px 0;"))
    })

    # Amount basis (household analysis only) ----
    output$amount_basis_ui <- renderUI({
      au <- tryCatch(analysis_unit(), error = function(e) "hh")
      if (!identical(au, "hh")) {
        return(tags$p(
          class = "text-muted small",
          "Amounts are per unit; a per-person basis applies to household analysis only."
        ))
      }
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-people-roof me-1"),
          "Amount basis",
          info_popover(
            title = "Amount basis",
            tags$p(
              tags$b("Per household:"),
              "each recipient household receives the amount, shared across its",
              "members, so larger households gain less per person."
            ),
            tags$p(
              tags$b("Per person:"),
              "each member of a recipient household receives the amount, so cost",
              "and welfare gain grow with household size. A total budget is",
              "shared across recipient people."
            )
          )
        ),
        pill_toggle(
          inputId = ns("amount_basis"),
          label = NULL,
          aria_label = "Amount basis",
          choices = c("Per household" = "per_household", "Per person" = "per_capita"),
          selected = shiny::isolate(input$amount_basis) %||% "per_household"
        )
      )
    })

    # One-line summaries shown beside the flyout buttons ----
    output$targeting_summary <- renderText({
      tg <- input$targeting %||% "universal"
      if (identical(tg, "universal")) {
        return("")
      }
      conc <- suppressWarnings(as.numeric(input$error_concentration %||% 0))
      paste0(
        if (identical(tg, "pmt")) "Proxy rule; " else "",
        "errors ", input$inclusion_error_pct %||% 10, "% / ",
        input$exclusion_error_pct %||% 10, "%",
        if (is.finite(conc) && conc > 0) ", near cutoff" else ""
      )
    })

    # Kept out of sp_budget_amount_ui so a basis change does not re-render
    # (and reset) the amount inputs.
    output$payment_summary <- renderText({
      admin <- suppressWarnings(as.numeric(input$admin_cost_pct %||% 0))
      basis <- input$amount_basis %||% "per_household"
      parts <- c(
        if (identical(input$budget_mode %||% "transfer_first", "transfer_first") &&
          !identical(input$sp_type, "shock")) {
          paste0(input$transfer_n_payments %||% 6L, " payments/year")
        },
        # Basis of the amount: per household (default) or per person; other
        # analysis units are paid per unit.
        {
          au <- tryCatch(analysis_unit(), error = function(e) "hh")
          if (identical(au, "hh")) {
            if (identical(basis, "per_capita")) "per person" else "per household"
          } else {
            paste("per", unit_word(plural = FALSE, au = au))
          }
        },
        if (is.finite(admin) && admin > 0) paste0("admin ", admin, "%")
      )
      if (length(parts)) paste(parts, collapse = ", ") else ""
    })

    # Scenario specification ----
    #
    # UI-43: the whole module sits in a bslib accordion panel that starts
    # collapsed, and hidden outputs are suspended - so none of these inputs
    # existed until the user expanded "Social protection", and the scenario
    # below silently fell back to the `%||%` defaults (including sp_type
    # "shock", which is no longer one of the offered choices). Render them
    # eagerly so an untouched panel still reports its real defaults.
    lapply(
      c(
        "sp_type_ui", "sp_budget_amount_ui", "sp_targeting_ui",
        "pmt_variable_ui", "pmt_cutoff_ui", "sp_timing_ui", "amount_basis_ui",
        "targeting_summary", "payment_summary",
        "sp_trigger_ui", "trigger_summary", "trigger_value_hint", "trigger_unit"
      ),
      function(out_id) {
        shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
      }
    )

    # Currency the amounts are entered in: the selected outcome's units. The
    # transfer is converted to the stored welfare scale (2021 PPP) per row when
    # this is "LCU" (R2-BUG-04), and costs are reported back in this currency.
    sp_currency <- reactive({
      so <- tryCatch(selected_outcome(), error = function(e) NULL)
      units <- if (is.null(so) || nrow(so) == 0 || !"units" %in% names(so)) {
        NULL
      } else {
        so$units[[1]]
      }
      .sp_currency(units)
    })

    # One definition of the scenario, read by both the reach preview below and
    # the module's return API - the preview cannot drift from what is run.
    sp_scenario_spec <- reactive({
      sp_type_val <- input$sp_type %||% "regular"
      is_regular <- TRUE
      list(
        # program type
        sp_type = sp_type_val,
        # Budget mode: a shock program pays a fixed amount per activation
        budget_mode = if (identical(sp_type_val, "shock")) {
          "transfer_first"
        } else {
          input$budget_mode %||% "transfer_first"
        },
        # Default to 0 (not 1,000,000) so an un-touched budget can never
        # accidentally apply a million-USD transfer if budget-first mode is
        # selected before a budget is entered.
        budget_fixed = input$budget_fixed %||% 0,
        # Targeting
        targeting = input$targeting %||% "exante_poor",
        targeting_threshold = input$targeting_threshold_pct %||% 20,
        pmt_variable = input$pmt_variable %||% NA_character_,
        pmt_cutoff = input$pmt_cutoff %||% NA_real_,
        inclusion_error_pct = input$inclusion_error_pct %||% 10,
        exclusion_error_pct = input$exclusion_error_pct %||% 10,
        # 0 = errors fall on random units; larger = nearer the cutoff
        error_concentration = suppressWarnings(
          as.numeric(input$error_concentration %||% 0)
        ),
        # Transfer amount, in `currency` (outcome units)
        currency = sp_currency(),
        transfer_amount_usd = input$transfer_amount_usd %||% 0,
        # Per-person basis only exists for household analysis units
        amount_basis = if (identical(
          tryCatch(analysis_unit(), error = function(e) "hh"), "hh"
        )) {
          input$amount_basis %||% "per_household"
        } else {
          "per_household"
        },
        # Administration as a percentage of total cost
        admin_cost_pct = input$admin_cost_pct %||% 0,
        # Shock-responsive trigger and payout (Trigger settings flyout)
        trigger_type = input$trigger_type %||% "weather",
        trigger_variable = input$trigger_variable %||% NA_character_,
        trigger_direction = input$trigger_direction %||% "above",
        trigger_value = suppressWarnings(
          as.numeric(input$trigger_value %||% NA_real_)
        ),
        trigger_value_low = suppressWarnings(
          as.numeric(input$trigger_value_low %||% NA_real_)
        ),
        trigger_return_period_years = suppressWarnings(
          as.numeric(input$trigger_return_period_years %||% NA_real_)
        ),
        trigger_metric = input$trigger_metric %||% "weather",
        trigger_level = input$trigger_level %||% "national",
        payout_scope = input$payout_scope %||% "local",
        national_k_pct = suppressWarnings(
          as.numeric(input$national_k_pct %||% 0)
        ),
        payments_per_activation = input$payments_per_activation %||% 1L,
        # Layered dynamic targeting (shock only)
        dyn_geo = input$dyn_geo %||% "all",
        dyn_geo_pct = suppressWarnings(as.numeric(input$dyn_geo_pct %||% 50)),
        dyn_welfare = isTRUE(input$dyn_welfare),
        dyn_welfare_pct = suppressWarnings(as.numeric(input$dyn_welfare_pct %||% 20)),
        dyn_incl_error_pct = suppressWarnings(as.numeric(input$dyn_incl_error_pct %||% 20)),
        dyn_excl_error_pct = suppressWarnings(as.numeric(input$dyn_excl_error_pct %||% 20)),
        # Budget rule and annual cap (shock only; blank cap = none)
        budget_rule = input$budget_rule %||% "fixed",
        loss_share_pct = suppressWarnings(as.numeric(input$loss_share_pct %||% NA_real_)),
        annual_cap = suppressWarnings(as.numeric(input$annual_cap %||% NA_real_)),
        loss_event_pct = suppressWarnings(as.numeric(input$loss_event_pct %||% 10)),
        # Timing - regular programs always have n payments
        transfer_frequency =
          if (is_regular) {
            "regular"
          } else {
            input$transfer_frequency %||% "oneoff"
          },
        transfer_n_payments =
          if (identical(sp_type_val, "shock")) {
            # Payments in an activated year, so the reach card and the cost
            # arithmetic read the same number as the dynamic transfer
            input$payments_per_activation %||% 1L
          } else if (is_regular) {
            input$transfer_n_payments %||% 6L
          } else {
            input$transfer_n_payments %||% 1L
          },
        transfer_timing =
          if (is_regular) {
            NA_character_
          } else {
            input$transfer_timing %||% "expost"
          },
        timeliness_weeks =
          if (is_regular) {
            NA_integer_
          } else {
            input$timeliness_weeks %||% 0L
          },
        # Forecast skill, multiplier and delay loss (shock only, P2-7)
        forecast_pod_pct = suppressWarnings(as.numeric(input$forecast_pod_pct %||% 100)),
        forecast_far_pct = suppressWarnings(as.numeric(input$forecast_far_pct %||% 0)),
        effect_multiplier = suppressWarnings(as.numeric(input$effect_multiplier %||% 1)),
        delay_loss_pct_per_week = suppressWarnings(as.numeric(input$delay_loss_pct_per_week %||% 0))
      )
    })

    # Live reach / cost preview (UI-32) ----
    #
    # Answers "who does this reach and what does it cost?" while the user is
    # still choosing a cutoff, instead of only after a full policy run. The
    # figures come from `.sp_scenario_reach()`, which reuses the run's own
    # eligibility function under the run's analysis seed and the diagnostics
    # tab's cost arithmetic - so this is the number the simulation will
    # produce, not a separate approximation of it.

    sp_preview_inputs <- shiny::debounce(reactive({
      hs <- tryCatch(hist_sim(), error = function(e) NULL)
      svy <- if (!is.null(hs$svy)) {
        hs$svy
      } else {
        tryCatch(survey_weather(), error = function(e) NULL)
      }
      list(
        hs = hs, svy = svy, spec = sp_scenario_spec(),
        analysis_unit = tryCatch(analysis_unit(), error = function(e) "hh"),
        display_type = input$sp_type %||% "regular"
      )
    }), 250)

    # Exposure of the historical pipeline, rebuilt once per Step 2 run.
    hist_exposure <- reactive({
      hs <- tryCatch(hist_sim(), error = function(e) NULL)
      if (is.null(hs$pipeline)) {
        return(NULL)
      }
      tryCatch(step2_exposure_resolve(hs$pipeline, hs), error = function(e) NULL)
    })

    # Expected activation and cost of a shock program from the historical years
    # (NULL for a regular program).
    sp_shock_state <- reactive({
      preview <- sp_preview_inputs()
      if (!identical(preview$spec$sp_type, "shock")) {
        return(NULL)
      }
      tryCatch(
        sp_shock_preview(
          preview$spec, preview$hs, preview$svy, preview$analysis_unit,
          seed = wise_current_seed(), exposure = hist_exposure()
        ),
        error = function(e) list(problem = wise_user_error(e, "Shock preview"))
      )
    })

    sp_reach <- reactive({
      # The frame must be the one the policy run will use. Step 2 may have
      # filtered survey_weather() down to a single baseline round; estimating
      # over every round instead inflates both the eligible population and the
      # cost, and shifts the welfare quantile that defines "ex-ante poor".
      # This mirrors `svy <- hs$svy %||% survey_weather()` in mod_3_06.
      preview <- sp_preview_inputs()
      hs <- preview$hs
      svy <- preview$svy
      if (is.null(svy)) {
        return(NULL)
      }
      r <- .sp_scenario_reach(
        svy           = svy,
        sp            = preview$spec,
        analysis_unit = preview$analysis_unit,
        seed          = wise_current_seed()
      )
      if (is.null(r)) {
        return(NULL)
      }
      # Record which frame these figures describe. Before Step 2 has run there
      # is no baseline round to filter to, so the preview covers every survey
      # round and will drop once Step 2 narrows it - worth saying, rather than
      # letting the number appear to change on its own.
      r$on_baseline <- !is.null(hs$svy)
      r
    })

    output$sp_reach_ui <- renderUI({
      preview <- sp_preview_inputs()
      r <- sp_reach()
      preview_spec <- preview$spec
      preview_unit <- preview$analysis_unit
      unit_pl <- unit_word(plural = TRUE, au = preview_unit)
      unit_sg <- unit_word(plural = FALSE, au = preview_unit)
      program_label <- if (identical(preview$display_type, "shock")) {
        "Shock-responsive"
      } else {
        "Regular"
      }
      program_title <- tags$span(
        class = "sp-summary-program",
        tags$span(class = "selection-card-badge", program_label),
        tags$span(class = "sp-summary-program-suffix", "cash transfer")
      )

      if (is.null(r)) {
        return(selection_summary_card(
          title = program_title,
          rows = list(list(
            name = paste("Recipient", unit_pl),
            pills = "Not available"
          )),
          compact = TRUE
        ))
      }

      # Report household recipients separately from their represented people.
      count_hint <- if (isTRUE(r$weighted)) {
        paste0(
          "Survey-weighted; ", fmt_count(r$n_rows), " of ",
          fmt_count(r$n_total), " sampled ", unit_pl
        )
      } else {
        paste0(
          "Unweighted sample count; ", fmt_count(r$n_rows), " of ",
          fmt_count(r$n_total), " sampled ", unit_pl
        )
      }

      is_shock <- identical(preview_spec$sp_type, "shock")
      cost_label <- if (is_shock) "Annual cost in a year it pays" else "Estimated annual cost"
      shock_rows <- if (is_shock) {
        s <- sp_shock_state()
        cur <- .sp_currency_prefix(preview_spec$currency)
        if (is.null(s) || !is.null(s$problem)) {
          list(selection_card_row(
            name = "Trigger", pills = s$problem %||% "Not available"
          ))
        } else {
          list(
            selection_card_row(
              name = "Years with payments",
              pills = fmt_num(100 * s$activation_freq, suffix = "%")
            ),
            selection_card_row(
              name = "Expected annual cost",
              pills = fmt_num(s$mean_cost, digits = 0, prefix = cur)
            ),
            selection_card_row(
              name = "1-in-20 year cost",
              pills = if (is.finite(s$cost_1_in_20)) {
                fmt_num(s$cost_1_in_20, digits = 0, prefix = cur)
              } else {
                "Needs 20 years"
              }
            ),
            selection_card_row(
              name = "Variability of annual cost (CV)",
              pills = fmt_num(s$cost_cv, digits = 2, na = "Not available")
            ),
            selection_card_row(
              name = "Modelled loss offset",
              pills = fmt_num(100 * s$loss_offset_share, suffix = "%", na = "Not available")
            ),
            if (is.finite(suppressWarnings(as.numeric(preview_spec$annual_cap %||% NA)))) {
              selection_card_row(
                name = "Years the cap binds",
                pills = fmt_num(100 * s$cap_exhaustion_share, suffix = "%", na = "Not available")
              )
            },
            if (is.finite(s$breakeven_m %||% NA_real_)) {
              selection_card_row(
                name = "Payments before no event",
                pills = fmt_num(100 * s$false_alarm_cost_share, suffix = "%", na = "Not available")
              )
            },
            if (is.finite(s$breakeven_m %||% NA_real_)) {
              selection_card_row(
                name = "Breakeven effectiveness multiplier m",
                pills = fmt_num(s$breakeven_m, digits = 2, na = "Not available")
              )
            },
            selection_card_row(
              name = "Population in triggered locations",
              pills = fmt_num(100 * s$mean_exposed_share, suffix = "%")
            ),
            selection_card_row(
              name = "Paid, no loss event",
              pills = fmt_num(100 * s$false_positive_rate, suffix = "%", na = "Not available")
            ),
            selection_card_row(
              name = "Loss event, not paid",
              pills = fmt_num(100 * s$false_negative_rate, suffix = "%", na = "Not available")
            )
          )
        }
      }

      targeting <- preview_spec$targeting
      targeting_info <- switch(targeting,
        exante_poor = paste(
          "The bottom", preview_spec$targeting_threshold,
          "% is selected by sampled", unit_pl, "before inclusion and exclusion",
          "errors. The reached share is survey-weighted, so it can differ from",
          "the cutoff when sampled", unit_pl, "represent different population",
          "sizes. Household size affects transfer cost, not this percentage."
        ),
        pmt = paste(
          "Eligibility follows the selected Proxy variable and cutoff. The reached",
          "share is survey-weighted; inclusion and exclusion errors are applied",
          "after the Proxy rule."
        ),
        `default` = paste(
          "Universal targeting reaches all sampled units; survey weights determine",
          "the population count and share."
        )
      )

      selection_summary_card(
        title = program_title,
        rows = Filter(Negate(is.null), c(list(
          selection_card_row(
            name  = paste("Recipient", unit_pl),
            pills = fmt_count(r$n_recipient_units)
          ),
          selection_card_row(
            name  = if (identical(analysis_unit(), "hh")) {
              "People represented"
            } else {
              "Population represented"
            },
            pills = fmt_count(r$n_pop)
          ),
          selection_card_row(
            name  = "Share of total population",
            pills = fmt_num(r$share_pct, suffix = "%")
          ),
          if (identical(r$amount_basis, "per_capita") &&
            identical(preview_unit, "hh")) {
            selection_card_row(
              name  = "Annual transfer per person",
              pills = fmt_num(
                r$transfer_per_person,
                digits = 2, prefix = .sp_currency_prefix(preview_spec$currency)
              )
            )
          } else {
            selection_card_row(
              name  = paste("Annual transfer per", unit_sg),
              pills = fmt_num(
                r$transfer_per_unit,
                digits = 2, prefix = .sp_currency_prefix(preview_spec$currency)
              )
            )
          },
          selection_card_row(
            name  = cost_label,
            pills = fmt_num(
              r$transfer_total,
              digits = 0, prefix = .sp_currency_prefix(preview_spec$currency)
            )
          ),
          if (isTRUE(r$admin_cost > 0)) {
            selection_card_row(
              name  = "Of which administration",
              pills = fmt_num(
                r$admin_cost,
                digits = 0, prefix = .sp_currency_prefix(preview_spec$currency)
              )
            )
          }
        ), shock_rows)),
        info = paste(
          count_hint,
          targeting_info,
          if (is_shock) {
            paste(
              "Recipients and cost are for a year in which the program pays.",
              "Years with payments, expected cost and the 1-in-20 cost come from",
              "the historical weather years of Step 2, evaluated per survey",
              "location; they are not a forecast. A loss event is a location whose",
              "modelled welfare loss reaches the share set in Trigger settings;",
              "paid without one, or a loss event without payment, is basis risk."
            )
          } else {
            ""
          },
          if (isTRUE(r$transfer_total <= 0) &&
            !identical(preview_spec$budget_rule, "loss_share")) {
            "No transfer is configured yet, so welfare would remain unchanged."
          } else {
            ""
          },
          if (!isTRUE(r$on_baseline)) {
            "Before Step 2 runs, this preview covers every survey round; after the run it uses the baseline round used by the simulation."
          } else {
            ""
          }
        ),
        compact = TRUE
      )
    })

    # Rendered eagerly for the same reason as the panels above (UI-43).
    shiny::outputOptions(output, "sp_reach_ui", suspendWhenHidden = FALSE)

    # Return API ----

    list(
      sp_scenario = sp_scenario_spec,
      sp_preview_inputs = sp_preview_inputs,
      sp_reach = sp_reach
    )
  })
}
