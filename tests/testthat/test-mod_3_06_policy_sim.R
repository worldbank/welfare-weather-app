library(testthat)

# End-to-end checks of the mod_3_06 async path (CR-PERF-04, R2-PERF-07,
# R2-PERF-01) through shiny::testServer(). The Step 2 result and the Step 1 fit
# come from the smoke fixture in dev/, so these run only in a source checkout.

.policy_sim_fixture <- function() {
  source(testthat::test_path("../../dev/bench_step3_helpers.R"), local = TRUE)
  source(testthat::test_path("../../dev/bench_step2_helpers.R"), local = TRUE)
  input <- .bench_small_step3_input(n = 60L)
  baseline <- .bench_small_step2_result(input, "ols", "one_ssp_one_period")
  fixture <- .bench_step3_policy_fixtures("targeted_sp")[[1L]]
  hs <- baseline$hist_sim_result
  hs$.sig <- list(step = "step2-fixture")
  hs$svy <- input$svy
  hs$so <- input$so
  list(
    input = input, fixture = fixture, hs = hs,
    ss = baseline$new_scenarios %||% list(),
    mf = input$models[["ols"]]
  )
}

# Write the retained Step 2 artifact the way the Step 2 worker does and point
# `hs` at it.
.policy_sim_with_artifact <- function(fx, dir) {
  art <- file.path(dir, "step2.qs2")
  qs2::qs_save(step2_share_constants(list(
    hist_sim_result = fx$hs, new_scenarios = fx$ss, .sig = "step2-result-sig"
  )), art)
  fx$hs$.artifact <- list(
    file = art, sig = "step2-result-sig", scenario_names = names(fx$ss)
  )
  fx
}

.policy_sim_args <- function(fx, hs_rv, ss_rv = shiny::reactiveVal(fx$ss)) {
  list(
    survey_weather = shiny::reactive(fx$input$svy),
    sp_scenario = shiny::reactive(fx$fixture$sp),
    infra_scenario = shiny::reactive(fx$fixture$infra),
    selected_model = shiny::reactive(NULL),
    model_fit = shiny::reactive(fx$mf),
    selected_weather = shiny::reactive(fx$input$sw),
    hist_sim = shiny::reactive(hs_rv()),
    saved_scenarios = shiny::reactive(ss_rv()),
    skip_coef_draws = shiny::reactive(TRUE)
  )
}

test_that("a new Step 2 result cancels the run in flight and frees the button", {
  skip_if_not(file.exists(testthat::test_path("../../dev/bench_step3_helpers.R")),
    "dev/ benchmark helpers not available")
  skip_if_not_installed("qs2")
  fx <- .policy_sim_with_artifact(.policy_sim_fixture(), withr::local_tempdir())

  cancelled <- 0L
  submitted <- NULL
  local_mocked_bindings(
    .wise_step3_async_available = function(hs, ss) TRUE,
    step3_async_submit = function(snapshot, hs, ss, on_result, on_error, ...) {
      submitted <<- snapshot
      list(cancel = function() {
        cancelled <<- cancelled + 1L
        invisible(TRUE)
      })
    }
  )

  hs_rv <- shiny::reactiveVal(fx$hs)
  trigger <- shiny::reactiveVal(NULL)
  args <- c(
    list(id = "pol"), .policy_sim_args(fx, hs_rv),
    list(run_trigger = trigger)
  )
  shiny::testServer(mod_3_06_policy_sim_server, args = args, {
    trigger(1L)
    session$flushReact()
    expect_false(is.null(submitted))
    expect_true(session$returned$running())
    expect_identical(session$returned$run_status(), "running")
    # Only small inputs travel: the artifact reference, no Step 2 frames.
    expect_identical(submitted$artifact$sig, "step2-result-sig")
    expect_null(submitted[["hs"]])
    expect_null(submitted[["ss"]])
    expect_false(inherits(submitted$mf$fit3, "environment"))

    # A second click while the run is in flight is ignored.
    trigger(2L)
    session$flushReact()
    expect_identical(cancelled, 0L)

    # A new Step 2 result stops the worker run and releases the button.
    hs_new <- fx$hs
    hs_new$.sig <- list(step = "step2-fixture-2")
    hs_rv(hs_new)
    session$flushReact()
    expect_identical(cancelled, 1L)
    expect_false(session$returned$running())
    expect_identical(session$returned$run_status(), "idle")
    expect_identical(session$returned$sim_run_id(), 0L)
    expect_null(session$returned$policy_hist_sim())

    # Nothing is in flight any more: another change cancels nothing.
    hs_new$.sig <- list(step = "step2-fixture-3")
    hs_rv(hs_new)
    session$flushReact()
    expect_identical(cancelled, 1L)
  })
})

test_that("a result whose member weather was released is never computed in process", {
  skip_if_not(file.exists(testthat::test_path("../../dev/bench_step3_helpers.R")),
    "dev/ benchmark helpers not available")
  skip_if_not_installed("qs2")
  fx <- .policy_sim_fixture()
  ss_released <- .step3_release_member_weather(fx$ss)
  skip_if_not(.step3_members_released(ss_released), "fixture has no member pipelines")

  # Workers are unavailable (artifact gone or WISEAPP_ASYNC_STEP3=0), so the
  # synchronous branch would run; it must refuse instead.
  local_mocked_bindings(
    step3_compute = function(...) stop("step3_compute must not run"),
    step3_async_submit = function(...) stop("no worker expected")
  )
  hs_rv <- shiny::reactiveVal(fx$hs)
  trigger <- shiny::reactiveVal(NULL)
  args <- c(
    list(id = "pol"), .policy_sim_args(fx, hs_rv, shiny::reactiveVal(ss_released)),
    list(run_trigger = trigger)
  )
  shiny::testServer(mod_3_06_policy_sim_server, args = args, {
    trigger(1L)
    session$flushReact()
    expect_identical(session$returned$run_status(), "failure")
    expect_false(session$returned$running())
    expect_identical(session$returned$sim_run_id(), 0L)
    expect_null(session$returned$policy_hist_sim())
  })
})

test_that("a run on the shared daemon is published and equals the in-process run", {
  skip_on_cran()
  skip_if_not(file.exists(testthat::test_path("../../dev/bench_step3_helpers.R")),
    "dev/ benchmark helpers not available")
  skip_if_not_installed("qs2")

  state <- .wise_step2_async_state
  withr::local_envvar(WISEAPP_ASYNC_SYNC = "0")
  try(mirai::daemons(0L), silent = TRUE)
  state$started <- FALSE
  withr::defer({
    try(mirai::daemons(0L), silent = TRUE)
    state$started <- FALSE
  })
  root <- withr::local_tempdir()
  withr::local_envvar(WISEAPP_ASYNC_ARTIFACT_ROOT = root)
  fx <- .policy_sim_with_artifact(.policy_sim_fixture(), root)
  expect_true(.wise_step3_async_available(fx$hs, fx$ss))

  withr::local_seed(123L)
  direct <- step3_compute(
    svy = fx$hs$svy, hs = fx$hs, ss = fx$ss, mf = fx$mf,
    sp_cfg = fx$fixture$sp, infra_cfg = fx$fixture$infra,
    model_vars = character(), analysis_unit = "hh", skip_coef = TRUE,
    residuals = "original", run_generation = 1L, seed = wise_current_seed()
  )

  hs_rv <- shiny::reactiveVal(fx$hs)
  trigger <- shiny::reactiveVal(NULL)
  args <- c(
    list(id = "pol"), .policy_sim_args(fx, hs_rv), list(run_trigger = trigger)
  )
  shiny::testServer(mod_3_06_policy_sim_server, args = args, {
    trigger(1L)
    session$flushReact()
    deadline <- Sys.time() + 120
    while (session$returned$running() && Sys.time() < deadline) {
      later::run_now(0.1)
      session$flushReact()
    }
    expect_identical(session$returned$run_status(), "success")
    expect_identical(session$returned$sim_run_id(), 1L)
    expect_false(session$returned$running())
    published <- session$returned$policy_hist_sim()
    expect_false(is.null(published))
    expect_true(file.exists(published$.artifact$file))
    expect_identical(published$pipeline$y_point, direct$pol_out$hist_sim$pipeline$y_point)
    expect_false(session$returned$stale())
  })
})
