# Keep the last successful result/context pair intact until the next run has
# completed all work. A failed run therefore has no publication side effect.
.publish_decomposition_bundle <- function(previous, context, success) {
  if (!isTRUE(success)) {
    return(previous)
  }
  list(context = context)
}

# R2-BUG-14: Step 3 combines the live Step 1 fit with the stored Step 2
# result. The Step 2 run signature records the fit signature it was built
# from, so a different live fit means Step 2 is stale for the current model
# and the policy run must not start. Results without a recorded signature
# are not judged.
.step2_model_mismatch <- function(mf, hs) {
  if (is.null(mf) || is.null(hs) || !is.list(hs$.sig)) {
    return(FALSE)
  }
  !identical(mf$.sig, hs$.sig$fit_sig)
}

#' 3_06_policy_sim Server Functions
#'
#' Applies user-defined policy adjustments to survey covariates from the
#' policy scenario modules (mod_3_01 through mod_3_05), then re-runs the
#' Step 2 simulation pipeline against both the baseline and policy-adjusted
#' survey frames using the cached Step 2 weather, model fit and draws.
#'
#' @param id                Module id.
#' @param survey_weather    Reactive survey-weather df to be adjusted.
#' @param sp_scenario       Reactive named list from mod_3_01_sp_server().
#' @param infra_scenario    Reactive named list from mod_3_02_infra_server().
#' @param digital_scenario  Reactive named list from mod_3_03_digital_server().
#' @param labor_scenario    Reactive named list from mod_3_04_labor_server().
#' @param education_scenario Reactive list from mod_3_05_education_server().
#' @param selected_model    Reactive list of the selected Step 1 model's
#'   parameters. Used to restrict covariate levers to variables still in the
#'   model, so dropped variables do not appear as manipulated in diagnostics.
#' @param model_fit         Reactive list from mod_1 model fit.
#' @param selected_weather  Reactive selected-weather metadata.
#' @param hist_sim          Reactive Step 2 hist_sim list.
#' @param saved_scenarios   Reactive Step 2 named scenario list.
#' @param run_trigger       Reactive the parent fires to request a policy run
#'   (REACT-09): the run lifecycle stays inside this module instead of the
#'   parent calling the exported `run()` closure.
#'
#' @return Named list with baseline_svy, policy_svy, sim_run_id, plus
#'   re-simulated baseline_hist_sim/baseline_saved_scenarios and
#'   policy_hist_sim/policy_saved_scenarios.
#'
#' @noRd
mod_3_06_policy_sim_server <- function(id,
                                       survey_weather,
                                       sp_scenario = reactive(NULL),
                                       infra_scenario = reactive(NULL),
                                       digital_scenario = reactive(NULL),
                                       labor_scenario = reactive(NULL),
                                       education_scenario = reactive(NULL),
                                       selected_model = reactive(NULL),
                                       model_fit = reactive(NULL),
                                       selected_weather = reactive(NULL),
                                       hist_sim = reactive(NULL),
                                       saved_scenarios = reactive(list()),
                                       analysis_unit = reactive("hh"),
                                       skip_coef_draws = reactive(FALSE),
                                       residuals = reactive("original"),
                                       propagate_all_covariate_uncertainty =
                                         reactive(FALSE),
                                       survey_version = reactive(0L),
                                       sim_stale = reactive(FALSE),
                                       run_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    baseline_svy_rv <- reactiveVal(NULL)
    policy_svy_rv <- reactiveVal(NULL)
    sim_run_id <- reactiveVal(0L)
    # REACT-02: TRUE while a policy simulation is executing.
    sim_running <- reactiveVal(FALSE)
    run_generation <- reactiveVal(0L)
    run_status <- reactiveVal("idle")
    decomp_bundle_rv <- reactiveVal(list(context = NULL))
    decomp_context_rv <- reactive(decomp_bundle_rv()$context)
    annual_channels_rv <- reactive(decomp_bundle_rv()$annual_channels)
    decomp_scenarios_rv <- reactiveVal(list())
    diagnostic_summary_rv <- reactiveVal(NULL)
    # Shock-responsive run outputs (rows and summary); NULL for a regular program
    shock_summary_rv <- reactiveVal(NULL)
    # INT-08: TRUE while the stored policy results' run signature no longer
    # matches the current Step 2 output / scenario inputs.
    policy_stale <- reactiveVal(FALSE)

    baseline_hist_sim_rv <- reactiveVal(NULL)
    baseline_saved_scenarios_rv <- reactiveVal(list())
    policy_hist_sim_rv <- reactiveVal(NULL)
    policy_saved_scenarios_rv <- reactiveVal(list())
    weather_store_lease_rv <- reactiveVal(NULL)
    sp_scenario_rv <- reactiveVal(NULL)
    # Snapshot all policy domains with the run so summary surfaces describe
    # the configuration that produced the results.
    infra_scenario_rv <- reactiveVal(NULL)
    digital_scenario_rv <- reactiveVal(NULL)
    labor_scenario_rv <- reactiveVal(NULL)
    education_scenario_rv <- reactiveVal(NULL)

    # A worker result that arrives after the session ended is dropped.
    session_ended <- FALSE
    # File of the retained policy result (CR-PERF-04); removed when the run is
    # replaced and at session end.
    policy_artifact_rv <- reactiveVal(NULL)
    # The run handed to a worker (R2-PERF-07) and the function that releases
    # the button and the progress bar; both NULL when nothing is in flight.
    active_job <- NULL
    active_end <- NULL
    # Stop the in-flight worker run, if any, so the daemon is free at once.
    cancel_active_run <- function() {
      job <- active_job
      end <- active_end
      if (is.null(job)) {
        return(invisible(FALSE))
      }
      active_job <<- NULL
      active_end <<- NULL
      job$cancel()
      try(
        {
          run_status("idle")
          if (is.function(end)) end()
        },
        silent = TRUE
      )
      invisible(TRUE)
    }
    cleanup_weather_stores <- function() {
      cancel_active_run()
      session_ended <<- TRUE
      unlink(shiny::isolate(policy_artifact_rv())$file, force = TRUE)
      policy_artifact_rv(NULL)
      step2_weather_store_release(shiny::isolate(weather_store_lease_rv()))
      weather_store_lease_rv(NULL)
    }
    session$onSessionEnded(cleanup_weather_stores)

    # Run signature (INT-08) ----
    # The policy run inherits Step 2's signature and adds the scenario
    # configuration; a mismatch (or a stale Step 2) marks the results stale.

    .policy_sig_from_live <- function(hs = hist_sim()) {
      list(
        step = "policy",
        correction_version = "row_aligned_annual_v1",
        sim_sig = if (!is.null(hs)) hs$.sig %||% NULL else NULL,
        survey_version = survey_version(),
        scenarios = .sig_plain(list(
          # `loss_event_pct` only scores the trigger; Diagnostics re-scores the
          # stored run, so changing it must not mark results stale.
          sp        = sp_scenario()[setdiff(names(sp_scenario()), "loss_event_pct")],
          infra     = infra_scenario(),
          digital   = digital_scenario(),
          labor     = labor_scenario(),
          education = education_scenario()
        ))
      )
    }

    # REACT-18: take the *reactive*, not its value. Passing `sp_scenario()`
    # here handed the helper a promise; `observeEvent()` quotes the symbol
    # `observe_what`, so the promise was forced on the observer's first run -
    # registering the dependency once - and every later evaluation returned
    # the cached value without re-registering it. The observer therefore fired
    # exactly once per session and then went deaf, which is why changing a
    # Step 3 lever never marked the policy results stale. Calling `react()`
    # inside the quoted expression re-establishes the dependency on every
    # invalidation.
    policy_signature <- reactive(.policy_sig_from_live())
    shiny::observeEvent(policy_signature(),
      {
        bh <- baseline_hist_sim_rv()
        if (!is.null(bh) && !identical(policy_signature(), bh$.sig)) {
          policy_stale(TRUE)
        }
      },
      ignoreInit = TRUE
    )
    # Cascade: when Step 2 is stale (inputs changed, not yet re-run) the
    # policy results built on it are stale too.
    shiny::observeEvent(sim_stale(),
      {
        if (isTRUE(sim_stale()) && !is.null(baseline_hist_sim_rv())) {
          policy_stale(TRUE)
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(baseline_hist_sim_rv(), policy_stale(FALSE))
    # R2-PERF-07: a new Step 2 result makes an in-flight run obsolete; stop it
    # now instead of letting it hold the daemon until its result is dropped.
    shiny::observeEvent(hist_sim(),
      {
        if (cancel_active_run()) {
          shiny::showNotification(
            "The policy run was stopped because the Step 2 results changed.",
            type = "warning", duration = 6
          )
        }
      },
      ignoreInit = TRUE
    )

    run <- function() {
      # REACT-02: one policy simulation at a time. The guard is owned by the
      # module doing the work; the triggering button (mod_3_scenario) is
      # disabled via the exposed reactive.
      if (isTRUE(sim_running())) {
        return(invisible(NULL))
      }
      sim_running(TRUE)
      # TRUE once the run was handed to a worker; the callbacks then own
      # sim_running and run_status.
      deferred <- FALSE
      on.exit(if (!deferred) sim_running(FALSE), add = TRUE)
      run_generation(run_generation() + 1L)
      run_status("running")
      completed <- FALSE
      on.exit(
        {
          if (!completed && !deferred) run_status("failure")
        },
        add = TRUE
      )

      # REACT-17: these are upstream reactives that req() internally (e.g.
      # selected_weather() on the Step 1 weather selector). An unmet req()
      # used to propagate out of this function as a silent error, so the
      # click produced no progress bar, no notification and no banner - a
      # dead button with nothing to explain it. Read them defensively and
      # turn every missing prerequisite into a message the user can act on.
      .safe <- function(expr) tryCatch(expr, error = function(e) NULL)
      mf <- .safe(model_fit())
      sw <- .safe(selected_weather())
      hs <- .safe(hist_sim())
      ss <- .safe(saved_scenarios())
      # Use the exact survey that Step 2 used as the baseline. Step 2 may have
      # filtered survey_weather() to a single survey round (baseline_svy). The
      # Step 2 weather_raw was fetched against that filtered survey, so it
      # contains rows for all survey years in selected_surveys - joining the
      # FULL survey_weather() would pull in extra households from non-baseline
      # rounds and produce a systematically different aggregate.
      svy <- hs$svy %||% .safe(survey_weather())
      # The synthetic `poor` outcome is created during Step 1 preparation,
      # but may not be retained in the Step 2 survey snapshot.
      svy <- ensure_outcome_column(svy, hs$so)
      sp_cfg <- .safe(sp_scenario())
      infra_cfg <- .safe(infra_scenario())
      digital_cfg <- .safe(digital_scenario())
      labor_cfg <- .safe(labor_scenario())
      education_cfg <- .safe(education_scenario())
      model_vars <- model_term_names(.safe(selected_model()))

      .fail <- function(msg) {
        shiny::showNotification(msg, type = "error", duration = 8)
        invisible(NULL)
      }

      if (is.null(mf)) {
        return(.fail(paste(
          "No fitted model. Run the Step 1 model before simulating policy",
          "scenarios."
        )))
      }
      if (is.null(hs)) {
        return(.fail(
          "Step 2 simulation must be run before policy simulation."
        ))
      }
      if (.step2_model_mismatch(mf, hs)) {
        return(.fail(paste(
          "The Step 1 model changed after the Step 2 simulation ran.",
          "Re-run the Step 2 simulation before simulating policy scenarios."
        )))
      }
      if (is.null(sw)) {
        return(.fail(paste(
          "No weather variables selected. Configure them in Step 1 before",
          "simulating policy scenarios."
        )))
      }
      if (is.null(svy)) {
        return(.fail("Survey data not available."))
      }

      run_started <- proc.time()[["elapsed"]]
      fail <- function(e) {
        .wise_log_stage("step3_run", "failed", run_id = paste0("step3-", isolate(sim_run_id()) + 1L),
          elapsed = proc.time()[["elapsed"]] - run_started)
        shiny::showNotification(
          wise_user_error(e, "Policy simulation"),
          type = "error", duration = 8
        )
      }
      # Publishes a computed run (from step3_compute() in this process or from
      # a worker). Errors are handled here so both paths report the same way.
      publish <- function(computed, policy_sig) {
        tryCatch(
        {
          svy_mod <- computed$svy_mod
          decomp_context <- computed$decomp_context
          pol_out <- computed$pol_out
          baseline_out <- computed$baseline_out
          baseline_scenarios_out <- computed$baseline_scenarios_out
          diagnostic_summary_out <- computed$diagnostic_summary
          decomp_sc <- pol_out$decomp_scenarios

          if (isTRUE(computed$no_effect)) {
            shiny::showNotification(
              paste(
                "No policy change is configured - every lever is at zero, so",
                "the policy results will match the baseline. Set a transfer",
                "amount or budget under Social protection, or adjust another",
                "lever."
              ),
              type = "warning", duration = 10
            )
          }

          # Atomic publish (INT-09) ----
          # Every reactive value is written only now that the complete run
          # (simulation + decomposition) succeeded, so a failure anywhere
          # above leaves the previous results, diagnostics, and run ID intact.
          # INT-08: the policy run signature is stored with both result arms.
          final_context <- .finalize_decomposition_context(decomp_context)
          final_bundle <- .publish_decomposition_bundle(
            decomp_bundle_rv(), final_context, success = TRUE
          )
          final_bundle$annual_channels <- pol_out$annual_channels
          baseline_out$.sig <- policy_sig
          if (!is.null(pol_out$hist_sim)) pol_out$hist_sim$.sig <- policy_sig
          # CR-PERF-04: the retained worker result (policy arm on disk) lets
          # the metric-decomposition workers read it instead of receiving it.
          if (!is.null(pol_out$hist_sim)) {
            pol_out$hist_sim$.artifact <- computed$artifact
          }
          old_policy_artifact <- policy_artifact_rv()
          new_weather_lease <- step2_weather_store_acquire_scenarios(c(
            baseline_scenarios_out,
            pol_out$saved_scenarios %||% list()
          ))
          old_weather_lease <- weather_store_lease_rv()
          baseline_svy_rv(svy)
          # A shock program has no static transfer; for reach and diagnostics the
          # published frame marks households paid in at least one historical year.
          policy_svy_rv(if (is.null(pol_out$shock)) {
            svy_mod
          } else {
            published <- svy_mod
            published[[SP_TRANSFER_COL]] <- pol_out$shock$per_household *
              pol_out$shock$paid_historical
            published
          })
          baseline_hist_sim_rv(baseline_out)
          baseline_saved_scenarios_rv(baseline_scenarios_out)
          policy_hist_sim_rv(pol_out$hist_sim)
          policy_saved_scenarios_rv(pol_out$saved_scenarios)
          weather_store_lease_rv(new_weather_lease)
          step2_weather_store_release(old_weather_lease)
          policy_artifact_rv(computed$artifact)
          unlink(old_policy_artifact$file, force = TRUE)
          sp_scenario_rv(sp_cfg)
          infra_scenario_rv(infra_cfg)
          digital_scenario_rv(digital_cfg)
          labor_scenario_rv(labor_cfg)
          education_scenario_rv(education_cfg)
          decomp_bundle_rv(final_bundle)
          decomp_scenarios_rv(decomp_sc)
          diagnostic_summary_rv(diagnostic_summary_out)
          shock_summary_rv(pol_out$shock)
          policy_stale(FALSE)

          sim_run_id(isolate(sim_run_id()) + 1L)
          .wise_log_stage("step3_run", "succeeded", run_id = paste0("step3-", sim_run_id()),
            elapsed = proc.time()[["elapsed"]] - run_started)
          run_status("success")
          completed <<- TRUE
          shiny::showNotification(
            "Policy scenario results are ready.",
            type = "message", duration = 3
          )
          # R2-BUG-13: disclose rows treated as untreated because of missing
          # values.
          n_untreated <- pol_out$n_na_untreated %||% 0L
          if (n_untreated > 0L) {
            shiny::showNotification(
              sprintf(paste(
                "%d survey row(s) with a missing outcome or policy lever value",
                "were treated as untreated (no policy change)."
              ), n_untreated),
              type = "warning", duration = 8
            )
          }
        },
        error = function(e) {
          # A result that was not published must not keep its retained file.
          if (!identical(shiny::isolate(policy_artifact_rv())$file, computed$artifact$file)) {
            unlink(computed$artifact$file, force = TRUE)
          }
          fail(e)
        }
        )
      }

      tryCatch(
        {
          # INT-08: the signature is captured up front from the exact inputs
          # this run consumes (scenario reactives are read again below); a
          # signature built at publish time could record mid-run edits.
          policy_sig <- .policy_sig_from_live(hs)
          residuals_val <- residuals()
          generation <- run_generation()
          inputs <- list(
            svy = svy, mf = mf,
            sp_cfg = sp_cfg, infra_cfg = infra_cfg,
            digital_cfg = digital_cfg, labor_cfg = labor_cfg,
            education_cfg = education_cfg,
            model_vars = model_vars,
            analysis_unit = analysis_unit(),
            skip_coef = skip_coef_draws(),
            residuals = residuals_val,
            run_generation = generation,
            seed = wise_current_seed()
          )

          if (.wise_step3_async_available(hs, ss)) {
            # CR-PERF-04: the worker reads the retained Step 2 artifact; only
            # the small inputs travel. The button stays disabled (sim_running)
            # until the result is published or the run fails.
            deferred <- TRUE
            progress <- shiny::Progress$new(session, min = 0, max = 1)
            progress$set(value = 0.02, message = "Running policy simulation...",
              detail = "Starting background worker...")
            end_run <- function() {
              progress$close()
              sim_running(FALSE)
            }
            active_end <<- end_run
            # The result is dropped when the session ended, another run took
            # over, or the Step 2 result it was computed from was replaced.
            is_current <- function() {
              !session_ended && identical(run_generation(), generation) &&
                identical(hist_sim()$.sig, hs$.sig)
            }
            in_session <- function(fn) {
              function(...) {
                args <- list(...)
                shiny::withReactiveDomain(session, shiny::isolate({
                  if (session_ended) return(invisible(NULL))
                  do.call(fn, args)
                }))
              }
            }
            active_job <<- step3_async_submit(
              # The worker gets the slim model fit (no fitting environments,
              # which would drag the Shiny session into the snapshot), as in
              # Step 2.
              snapshot = c(inputs[setdiff(names(inputs), "mf")], list(
                mf = step2_slim_model_fit(mf),
                artifact = hs$.artifact[c("file", "sig")],
                hs_overlay = hs[intersect(
                  c("hist_label", "sim_summary", ".sig"), names(hs)
                )]
              )),
              hs = hs, ss = ss,
              on_progress = in_session(function(value, detail) {
                progress$set(value = value, detail = detail)
              }),
              is_current = function() {
                shiny::withReactiveDomain(session, shiny::isolate(is_current()))
              },
              on_result = in_session(function(computed) {
                active_job <<- NULL
                active_end <<- NULL
                on.exit(end_run(), add = TRUE)
                if (!is_current()) {
                  unlink(computed$artifact$file, force = TRUE)
                  run_status("idle")
                  return(invisible(NULL))
                }
                publish(computed, policy_sig)
                if (!identical(run_status(), "success")) run_status("failure")
              }),
              on_error = in_session(function(e) {
                active_job <<- NULL
                active_end <<- NULL
                on.exit(end_run(), add = TRUE)
                run_status("failure")
                fail(e)
              })
            )
          } else {
            # R2-PERF-01: a result whose member weather was released can only
            # be used by a worker (the retained artifact is gone or workers
            # are off). Say so instead of computing from missing frames.
            if (.step3_members_released(ss)) {
              stop(.STEP3_RELEASED_MESSAGE, call. = FALSE)
            }
            # Held in locals - INT-09 publishes all state atomically at the end
            # of a fully successful run.
            computed <- shiny::withProgress(
              message = "Running policy simulation...",
              value = 0.1,
              do.call(step3_compute, c(
                list(hs = hs, ss = ss), inputs,
                list(progress = function(value, detail = NULL) {
                  shiny::setProgress(value = value, detail = detail)
                })
              ))
            )
            publish(computed, policy_sig)
          }
        },
        error = fail
      )
      invisible(NULL)
    }

    # REACT-09: the parent requests a run by firing this trigger instead of
    # calling the exported run() closure.
    #
    # REACT-19: no `ignoreInit` here. The parent's `req()` already blocks both
    # states that precede a real click - NULL while the button has not been
    # rendered, and 0 once it has (an action button's 0 is not truthy). That
    # made `ignoreInit` actively harmful: it skips the handler on the
    # observer's first *successful* evaluation of the event expression, and
    # because every earlier evaluation aborted on the unmet `req()`, the first
    # successful one was the user's first click. Step 3 therefore ignored the
    # first "Run simulation" and worked on every click after that.
    shiny::observeEvent(run_trigger(), {
      run()
    })

    list(
      running = sim_running,
      baseline_svy = baseline_svy_rv,
      policy_svy = policy_svy_rv,
      sim_run_id = sim_run_id,
      run_generation = run_generation,
      run_status = run_status,
      decomp_context = decomp_context_rv,
      annual_channels = annual_channels_rv,
      decomp_scenarios = decomp_scenarios_rv,
      diagnostic_summary = diagnostic_summary_rv,
      shock_summary = shock_summary_rv,
      baseline_hist_sim = baseline_hist_sim_rv,
      baseline_saved_scenarios = baseline_saved_scenarios_rv,
      policy_hist_sim = policy_hist_sim_rv,
      policy_saved_scenarios = policy_saved_scenarios_rv,
      sp_scenario = sp_scenario_rv,
      infra_scenario = infra_scenario_rv,
      digital_scenario = digital_scenario_rv,
      labor_scenario = labor_scenario_rv,
      education_scenario = education_scenario_rv,
      clear_weather_stores = cleanup_weather_stores,
      policy_scenarios = reactive(list(
        A = infra_scenario() %||% list(), B = infra_scenario() %||% list(),
        C = infra_scenario() %||% list(), D = infra_scenario() %||% list(),
        E = digital_scenario() %||% list(), F = digital_scenario() %||% list(),
        K = education_scenario() %||% list(), L = education_scenario() %||% list(),
        M = education_scenario() %||% list(),
        G = infra_scenario() %||% list(), H = infra_scenario() %||% list(),
        I = infra_scenario() %||% list(),
        J = labor_scenario() %||% list()
      )),
      stale = policy_stale
    )
  })
}
