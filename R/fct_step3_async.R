# Step 3 policy run: pure compute boundary (CR-PERF-04).
#
# `step3_compute()` holds the numerical work of one policy run and has no Shiny
# dependency, so the same code runs in the Shiny process today and in a mirai
# worker later. It returns plain data; publishing the results (INT-09) stays
# with the caller.

# Household-constant vectors (ids, weights, row maps) repeat in every pipeline
# of the policy arm. They are shared in memory in the worker, but
# serialisation writes one copy per pipeline and the read materialises them
# all again (about 400 MB extra at BFA 2x2). Reuse the Step 2 artifact
# sharing on the policy arm; the result stays bit-identical.
.step3_share_constants <- function(computed) {
  shared <- step2_share_constants(list(
    hist_sim_result = computed$pol_out$hist_sim,
    new_scenarios = computed$pol_out$saved_scenarios
  ))
  computed$pol_out$hist_sim <- shared$hist_sim_result
  computed$pol_out$saved_scenarios <- shared$new_scenarios
  computed$.shared_constants <- shared$.shared_constants
  computed
}

.step3_unshare_constants <- function(computed) {
  if (is.null(computed$.shared_constants)) return(computed)
  restored <- step2_unshare_constants(list(
    hist_sim_result = computed$pol_out$hist_sim,
    new_scenarios = computed$pol_out$saved_scenarios,
    .shared_constants = computed$.shared_constants
  ))
  computed$pol_out$hist_sim <- restored$hist_sim_result
  computed$pol_out$saved_scenarios <- restored$new_scenarios
  computed$.shared_constants <- NULL
  computed
}

# R2-PERF-01: the per-member weather frames of a Step 2 result (about 4.7 MB per
# member at BFA 2x2, 0.7 GB at 3x3) are read only by the Step 3 workers, which
# take them from the retained Step 2 artifact. When that artifact exists and
# Step 3 runs in workers, the Shiny process releases them at adoption. A
# released member keeps a marker, so a code path that still needs the frame
# fails visibly ("exposure unavailable") instead of computing from nothing;
# the synchronous Step 3 paths refuse released results explicitly.
.step3_release_marker <- function() structure(list(), class = "wiseapp_released_weather")

.step3_release_member_weather <- function(new_scenarios) {
  lapply(new_scenarios %||% list(), function(s) {
    if (!is.list(s) || !is.list(s$pipelines)) return(s)
    s$pipelines <- lapply(s$pipelines, function(p) {
      if (is.list(p) && !is.null(p$weather_raw)) p$weather_raw <- .step3_release_marker()
      p
    })
    s$members_released <- TRUE
    s
  })
}

.step3_members_released <- function(ss) {
  any(vapply(ss %||% list(), function(s) isTRUE(s$members_released), logical(1)))
}

# TRUE when the Shiny process may release member weather for a new result.
.step3_release_allowed <- function(artifact) {
  is.list(artifact) && .wise_step3_async_enabled() && .wise_step2_async_enabled() &&
    !.wise_step2_async_sync()
}

.STEP3_RELEASED_MESSAGE <- paste(
  "The stored Step 2 result is no longer available to the background worker.",
  "Re-run the Step 2 simulation."
)

# The policy arm is the baseline arm with corrected predictions: almost every
# field (weather_raw, weather_shared, svy, the household-constant vectors) is
# identical to the baseline's. Writing those again doubles the artifact, and
# reading them back doubles memory in the reader (about 300 MB of per-member
# weather at BFA 2x2). The worker replaces each large field that is identical
# to the baseline's with a link marker; the reader, which holds the baseline,
# points the field back at the baseline's own object (a reference, no copy).
.STEP3_LINK_MIN_BYTES <- 10000

.step3_link_marker <- function() structure(list(), class = "wiseapp_baseline_link")
.step3_is_link <- function(x) inherits(x, "wiseapp_baseline_link")

# Apply `entry_fn(policy_entry, baseline_entry)` to the policy hist_sim, its
# pipeline, each scenario and each member pipeline, paired with the baseline's.
.step3_map_arms <- function(pol_out, hs, ss, entry_fn) {
  if (is.list(pol_out$hist_sim) && is.list(hs)) {
    h <- entry_fn(pol_out$hist_sim, hs)
    if (is.list(h$pipeline) && is.list(hs$pipeline)) {
      h$pipeline <- entry_fn(h$pipeline, hs$pipeline)
    }
    pol_out$hist_sim <- h
  }
  for (nm in names(pol_out$saved_scenarios)) {
    p <- pol_out$saved_scenarios[[nm]]
    b <- ss[[nm]]
    if (!is.list(p) || !is.list(b)) next
    p <- entry_fn(p, b)
    for (m in names(p$pipelines)) {
      if (is.list(b$pipelines[[m]])) {
        p$pipelines[[m]] <- entry_fn(p$pipelines[[m]], b$pipelines[[m]])
      }
    }
    pol_out$saved_scenarios[[nm]] <- p
  }
  pol_out
}

.step3_dedupe_vs_baseline <- function(pol_out, hs, ss) {
  .step3_map_arms(pol_out, hs, ss, function(p, b) {
    for (f in setdiff(names(p), c("pipeline", "pipelines"))) {
      x <- p[[f]]
      if (!is.null(b[[f]]) && !.step3_is_link(x) &&
          as.numeric(utils::object.size(x)) > .STEP3_LINK_MIN_BYTES &&
          identical(x, b[[f]])) {
        p[[f]] <- .step3_link_marker()
      }
    }
    p
  })
}

.step3_relink_baseline <- function(pol_out, hs, ss) {
  .step3_map_arms(pol_out, hs, ss, function(p, b) {
    for (f in names(p)) {
      if (.step3_is_link(p[[f]])) {
        if (is.null(b[[f]])) {
          stop("The Step 3 result links to a baseline field that is missing: ", f,
            call. = FALSE)
        }
        p[[f]] <- b[[f]]
      }
    }
    p
  })
}

#' Check the run-owned invariants of a run read back from a worker.
#'
#' Serialisation keeps environment locks and shared references (one
#' `qs_save()` call), so the run-owned context and annual channels must arrive
#' locked and still point at each other. Anything else is refused rather than
#' repaired.
#' @noRd
step3_validate_worker_run <- function(computed) {
  channels <- computed$pol_out$annual_channels
  if (!is.environment(channels) || !environmentIsLocked(channels) ||
      !identical(channels$status, "ok")) {
    stop("The Step 3 result carries no valid annual channels.", call. = FALSE)
  }
  .validate_run_decomposition_context(computed$decomp_context, channels$run_identity)
  if (!identical(channels$context, computed$decomp_context)) {
    stop("The Step 3 result context and annual channels differ.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Baseline arm of a Step 3 run: the Step 2 `hist_sim` verbatim.
#'
#' Preserves the residual treatment captured by the Step 2 run and keeps the
#' source Step 2 signature as `.step2_sig`, because the baseline is republished
#' with the Step 3 policy signature.
#' @noRd
step3_baseline_arm <- function(hs, residuals = "original") {
  out <- hs
  out$residuals <- hs$residuals %||% residuals %||% "original"
  out$.step2_sig <- hs$.sig %||% NULL
  out
}

#' Compute one Step 3 policy run.
#'
#' @param svy Baseline survey frame (the one Step 2 used).
#' @param hs Step 2 `hist_sim` list.
#' @param ss Step 2 saved scenarios (named list).
#' @param mf Fitted model list from Step 1.
#' @param sp_cfg,infra_cfg,digital_cfg,labor_cfg,education_cfg Scenario
#'   configurations from the Step 3 lever modules.
#' @param model_vars Names of the variables still in the selected model.
#' @param analysis_unit `"hh"` or the analysis unit used by Step 2.
#' @param skip_coef Logical. Skip coefficient draws.
#' @param residuals Residual treatment carried by the baseline arm.
#' @param run_generation Integer generation of the run, used in the run identity.
#' @param seed Integer seed for the policy survey draws.
#' @param progress Function `(value, detail)` called at phase changes.
#'
#' @return List with `svy_mod`, `decomp_context`, `pol_out`, `baseline_out`,
#'   `baseline_scenarios_out`, `diagnostic_summary` and `no_effect`.
#' @noRd
step3_compute <- function(svy, hs, ss, mf,
                          sp_cfg = NULL, infra_cfg = NULL, digital_cfg = NULL,
                          labor_cfg = NULL, education_cfg = NULL,
                          model_vars = character(),
                          analysis_unit = "hh",
                          skip_coef = FALSE,
                          residuals = "original",
                          run_generation = 0L,
                          seed = NULL,
                          progress = function(value, detail = NULL) invisible(NULL)) {
  progress(0.1, "Applying policy levers...")
  svy_mod <- apply_policy_to_svy(
    svy,
    infra         = infra_cfg,
    sp            = sp_cfg,
    digital       = digital_cfg,
    labor         = labor_cfg,
    education     = education_cfg,
    model_vars    = model_vars,
    analysis_unit = analysis_unit,
    seed          = seed
  )

  policy_candidates <- .policy_candidate_cols(
    infra = infra_cfg,
    digital = digital_cfg,
    labor = labor_cfg,
    education = education_cfg,
    model_vars = model_vars
  )

  # A social-protection-only scenario changes nothing but the cash transfer
  # column, which is deliberately excluded from the covariate deltas - so it is
  # a perfectly valid run and must not be treated as "nothing configured". What
  # is worth flagging is a scenario that is a literal no-op (every lever at its
  # zero default): the run still goes ahead, but the policy arm will equal the
  # baseline.
  no_effect <- !.scenario_has_effect(svy, svy_mod, candidates = policy_candidates)

  progress(0.2, "Preparing baseline results...")
  # Baseline = Step 2 output verbatim. The survey is unchanged in the baseline
  # arm, so re-simulating would just reproduce the Step 2 results. Pass Step
  # 2's hist_sim and saved_scenarios straight through so the Results pane reads
  # exactly the same values Mod 2 shows. The Results pane and policy
  # resimulation both consume the Mod 2 schema ($pipeline for hist_sim,
  # $pipelines for each saved scenario), so no translation is required here.
  baseline_out <- step3_baseline_arm(hs, residuals)
  baseline_scenarios_out <- ss %||% list()

  progress(0.6, "Calculating policy scenario results...")
  # Derive the policy arm from the baseline pipelines by adding the analytic
  # per-household delta_total (the same number the Decomposition pane
  # reports). This (a) eliminates the baseline/policy disagreement when
  # residual draws have any stochastic component - both arms now share
  # identical train_aug / id_vec / svy_row_id, so residuals line up
  # household-for-household and a no-op policy yields a no-op visual effect;
  # and (b) removes the per-CMIP6-member re-simulation, which was the dominant
  # cost of every policy adjustment.
  skip_coef_val <- isTRUE(skip_coef)

  # PERF-22: covariate deltas and the training-outcome ecdf are weather- and
  # scenario-independent - every decompose_policy_effect() call below
  # (historical + per scenario-year) would otherwise rebuild both. Compute
  # once, pass through.
  deltas_pre <- .compute_policy_deltas(
    svy, svy_mod, hs$so$name, mf$weather_terms,
    candidate_cols = policy_candidates
  )
  F_hat_pre <- if (identical(mf$engine, "rif") &&
    !is.null(mf$train_data) &&
    hs$so$name %in% names(mf$train_data)) {
    stats::ecdf(mf$train_data[[hs$so$name]])
  } else {
    NULL
  }

  # W2-D: all decomposition calls in this published run share invariant
  # survey/model state. Weather hazards remain supplied per panel so
  # member-specific and year-specific weather cannot leak across bases.
  decomp_context <- .build_decomposition_context(
    svy_baseline = svy, svy_policy = svy_mod, model_fit = mf,
    so = hs$so, deltas = deltas_pre,
    skip_coef = skip_coef_val, F_hat = F_hat_pre,
    run_identity = paste0("generation-", run_generation),
    weather_panels = Filter(Negate(is.null), list(
      step2_resolve_weather(hs$weather_raw, hs)
    ))
  )
  if (is.null(decomp_context)) {
    stop("Unable to prepare policy decomposition context.", call. = FALSE)
  }

  pol_out <- apply_policy_delta_to_baseline(
    svy_baseline = svy,
    svy_policy = svy_mod,
    model_fit = mf,
    so = hs$so,
    hist_sim_baseline = baseline_out,
    saved_scenarios_baseline = baseline_scenarios_out,
    skip_coef = skip_coef_val,
    deltas = deltas_pre,
    F_hat = F_hat_pre,
    decomp_context = decomp_context,
    run_identity = decomp_context$run_identity,
    sp = sp_cfg,
    analysis_unit = analysis_unit,
    seed = seed
  )
  if (is.null(pol_out)) {
    stop("Policy simulation produced no results.", call. = FALSE)
  }

  # REACT-05: the production annual channels were computed above, so a failure
  # there already fails the whole run instead of silently presenting the
  # previous run as new.
  progress(0.85, "Summarizing policy effects...")

  # Production future summaries have already been reduced from the exact
  # channel blocks used to correct every member's predictions.
  progress(0.90, "Finalizing scenario summaries...")
  diagnostic_summary <- .policy_diagnostics_snapshot(
    svy_baseline = svy,
    svy_policy = svy_mod,
    outcome = hs$so$name,
    analysis_unit = analysis_unit,
    candidates = unique(c(policy_candidates, hs$so$name)),
    sp = sp_cfg,
    shock = pol_out$shock
  )
  if (is.null(diagnostic_summary)) {
    stop("Policy diagnostics produced no results.", call. = FALSE)
  }

  progress(1, "Results ready")
  list(
    svy_mod = svy_mod,
    decomp_context = decomp_context,
    pol_out = pol_out,
    baseline_out = baseline_out,
    baseline_scenarios_out = baseline_scenarios_out,
    diagnostic_summary = diagnostic_summary,
    no_effect = no_effect
  )
}


# Worker side ----

.wise_step3_async_enabled <- function() {
  value <- tolower(trimws(Sys.getenv("WISEAPP_ASYNC_STEP3", "1")))
  !value %in% c("0", "false", "no", "off")
}

# Whether this run can go to a worker: the adopted Step 2 result is on disk, is
# the one the Step 2 object describes, and the scenario list still matches.
# Anything else falls back to the synchronous path.
.wise_step3_async_artifact_ok <- function(hs, ss) {
  art <- hs$.artifact
  is.list(art) && is.character(art$file) && length(art$file) == 1L &&
    file.exists(art$file) &&
    identical(art$scenario_names, names(ss %||% list()))
}

.wise_step3_async_available <- function(hs, ss) {
  .wise_step3_async_enabled() && .wise_step2_async_enabled() &&
    !.wise_step2_async_sync() && .wise_step3_async_artifact_ok(hs, ss)
}

# Read the retained Step 2 artifact in a worker and apply the fields the main
# process set on `hist_sim` after the artifact was written. Refuses a file whose
# signature is not the one the snapshot names.
.wise_step3_read_step2 <- function(snapshot) {
  art <- snapshot$artifact
  step2 <- step2_unshare_constants(
    qs2::qs_read(art$file, nthreads = .WISE_STEP2_QS2_THREADS, validate_checksum = TRUE)
  )
  if (!is.list(step2) || !is.list(step2$hist_sim_result) ||
      !identical(step2$.sig, art$sig)) {
    stop("The stored Step 2 result does not match the current one.", call. = FALSE)
  }
  hs <- step2$hist_sim_result
  for (nm in names(snapshot$hs_overlay)) hs[[nm]] <- snapshot$hs_overlay[[nm]]
  list(hs = hs, ss = step2$new_scenarios %||% list())
}

#' Execute one Step 3 policy run in a worker process.
#'
#' Reads the retained Step 2 result artifact itself (the main process sends
#' only small inputs), runs `step3_compute()` and writes one `result.qs2`
#' holding the policy-arm outputs. The baseline arm is not returned: the caller
#' already holds the Step 2 result and rebuilds it with `step3_baseline_arm()`.
#'
#' @param snapshot Ordinary-object list with the `step3_compute()` inputs
#'   (`svy`, `mf`, scenario configs, ...) plus `artifact` (the retained Step 2
#'   file and signature) and `hs_overlay` (the fields the main process set on
#'   `hist_sim` after the artifact was written).
#' @param artifact_dir Directory that receives `result.qs2` and the progress file.
#' @export
step3_async_worker <- function(snapshot, artifact_dir) {
  .wise_apply_thread_limits()
  # mirai daemons draw from L'Ecuyer-CMRG; the seeded policy draws must match
  # the in-process run, so use the RNG kind of the process that submitted it.
  old_rng_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_rng_kind)), add = TRUE)
  do.call(RNGkind, as.list(
    snapshot$rng_kind %||% c("Mersenne-Twister", "Inversion", "Rejection")
  ))
  worker_started <- proc.time()[["elapsed"]]
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  progress_file <- file.path(artifact_dir, "progress.rds")
  progress <- function(value, detail = NULL) {
    try(.wise_step2_async_write_control(
      progress_file, list(value = value, detail = detail)
    ), silent = TRUE)
    invisible(NULL)
  }
  progress(0.02, "Reading the Step 2 result...")
  stage_at <- proc.time()[["elapsed"]]
  timings <- list()
  lap <- function(name) {
    now <- proc.time()[["elapsed"]]
    timings[[name]] <<- now - stage_at
    stage_at <<- now
  }
  step2 <- .wise_step3_read_step2(snapshot)
  hs <- step2$hs
  ss <- step2$ss
  rm(step2)
  lap("read_s")

  computed <- step3_compute(
    svy = snapshot$svy, hs = hs, ss = ss, mf = snapshot$mf,
    sp_cfg = snapshot$sp_cfg, infra_cfg = snapshot$infra_cfg,
    digital_cfg = snapshot$digital_cfg, labor_cfg = snapshot$labor_cfg,
    education_cfg = snapshot$education_cfg,
    model_vars = snapshot$model_vars,
    analysis_unit = snapshot$analysis_unit,
    skip_coef = snapshot$skip_coef,
    residuals = snapshot$residuals,
    run_generation = snapshot$run_generation,
    seed = snapshot$seed,
    progress = function(value, detail = NULL) progress(0.05 + 0.9 * value, detail)
  )
  # The baseline arm is the Step 2 result the caller already holds.
  computed$baseline_out <- NULL
  computed$baseline_scenarios_out <- NULL
  computed$pol_out <- .step3_dedupe_vs_baseline(computed$pol_out, hs, ss)
  rm(hs, ss)
  lap("compute_s")

  progress(0.97, "Writing the policy results...")
  result_file <- file.path(artifact_dir, "result.qs2")
  tmp <- paste0(result_file, ".tmp")
  # One call, so references shared between the context and the annual channels
  # survive the round trip.
  qs2::qs_save(.step3_share_constants(computed), tmp, nthreads = .WISE_STEP2_QS2_THREADS)
  if (!file.rename(tmp, result_file)) {
    unlink(tmp, force = TRUE)
    stop("Could not publish the Step 3 result artifact.", call. = FALSE)
  }
  lap("write_s")
  .wise_log_stage("step3_worker", "succeeded",
    elapsed = proc.time()[["elapsed"]] - worker_started)
  list(
    status = "succeeded", result_file = result_file,
    result_bytes = as.numeric(file.info(result_file)$size),
    timings = unlist(timings)
  )
}

# Read a worker result on the main thread and check its run-owned invariants.
step3_read_worker_result <- function(manifest, artifact_dir, hs, ss, residuals) {
  root <- normalizePath(artifact_dir, winslash = "/", mustWork = TRUE)
  file <- normalizePath(manifest$result_file, winslash = "/", mustWork = TRUE)
  if (!identical(manifest$status, "succeeded") || !startsWith(file, paste0(root, "/"))) {
    stop("Step 3 result manifest does not match the submitted job.", call. = FALSE)
  }
  if (is.numeric(manifest$result_bytes) && is.finite(manifest$result_bytes) &&
      manifest$result_bytes > .WISE_STEP2_RESULT_MAX_BYTES) {
    stop(sprintf(
      "The Step 3 result (%.1f GB) is larger than the %.0f GB limit.",
      manifest$result_bytes / 1024^3, .WISE_STEP2_RESULT_MAX_BYTES / 1024^3
    ), call. = FALSE)
  }
  read_started <- proc.time()[["elapsed"]]
  # Keep the result file: the metric-decomposition workers read the policy arm
  # from it. The caller owns the file and removes it with the run.
  kept <- file.path(.wise_step2_async_artifact_root(), "retained",
    paste0(basename(root), ".qs2"))
  dir.create(dirname(kept), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  if (suppressWarnings(file.rename(file, kept))) file <- kept else kept <- NULL
  ok <- FALSE
  on.exit(if (!ok && !is.null(kept)) unlink(kept, force = TRUE), add = TRUE)
  computed <- .step3_unshare_constants(
    qs2::qs_read(file, nthreads = .WISE_STEP2_QS2_THREADS, validate_checksum = TRUE)
  )
  computed$artifact <- if (!is.null(kept)) {
    list(file = normalizePath(kept, winslash = "/"), id = basename(root))
  }
  computed$pol_out <- .step3_relink_baseline(computed$pol_out, hs, ss)
  step3_validate_worker_run(computed)
  computed$timings <- c(manifest$timings, main_read_s = proc.time()[["elapsed"]] - read_started)
  computed$baseline_out <- step3_baseline_arm(hs, residuals)
  computed$baseline_scenarios_out <- ss %||% list()
  ok <- TRUE
  computed
}


# Main-process side ----

# Longest a job may wait for the dispatcher to accept it (queue memory cap).
.WISE_STEP3_SUBMIT_WAIT_SEC <- 120

# Run one Step 3 worker function (`worker`, a name exported by the package) on
# the shared local daemon. Callbacks run on the Shiny event loop; `is_current()`
# lets the caller drop a result whose session or run is gone.
# `on_progress(value, detail)` is polled from the worker's progress file.
# `convert(value, artifact_dir)` turns the worker's return value into the
# object handed to `on_result`; it runs before the job directory is removed.
# Returns the job record (environment) or NULL when the submit failed
# (`on_error` was called).
.wise_step3_async_task <- function(worker, snapshot, convert, on_result, on_error,
                                   on_progress = NULL, is_current = function() TRUE) {
  async <- tryCatch(.wise_step2_async_init(), error = function(e) FALSE)
  if (!isTRUE(async)) {
    on_error(simpleError("The background worker is not available."))
    return(invisible(NULL))
  }
  package_path <- getNamespaceInfo(asNamespace("wiseapp"), "path")
  development_package <- .wise_step2_async_is_dev_package()
  snapshot$rng_kind <- RNGkind()
  # A snapshot over the dispatcher's queue cap is never accepted, and
  # try_mirai() would serialise it again on every retry below. Refuse it up
  # front with a message instead of retrying (and growing memory) forever.
  cap_mb <- .wise_step2_async_queue_memory()
  snapshot_bytes <- length(serialize(snapshot, NULL, version = 3L))
  .wise_log_stage(
    "step3_snapshot", "built",
    run_id = if (is.character(worker)) worker else "custom",
    cache = paste0(round(snapshot_bytes / 1024^2), "MB")
  )
  if (!is.null(cap_mb) && snapshot_bytes > cap_mb * 1024^2) {
    on_error(simpleError(sprintf(
      paste("The inputs for the background worker are too large (%.0f MB,",
        "limit %.0f MB; WISEAPP_ASYNC_QUEUE_MEMORY_MB)."),
      snapshot_bytes / 1024^2, cap_mb
    )))
    return(invisible(NULL))
  }
  job <- new.env(parent = emptyenv())
  job$id <- sub("^step2-", "step3-", .wise_step2_async_id())
  job$artifact_dir <- file.path(.wise_step2_async_artifact_root(), "step3", job$id)
  job$poll_active <- TRUE
  job$submit_started <- proc.time()[["elapsed"]]
  artifact_dir <- job$artifact_dir

  finish <- function() {
    job$poll_active <- FALSE
    unlink(artifact_dir, recursive = TRUE, force = TRUE)
  }
  # R2-PERF-07: stop the task, queued or running, so the single daemon is free
  # at once (an interrupt reaches even a running SQL query). A cancelled job
  # never calls back; `finish()` removes its directory.
  job$cancelled <- FALSE
  job$cancel <- function() {
    if (isTRUE(job$cancelled)) return(invisible(FALSE))
    job$cancelled <- TRUE
    if (!is.null(job$handle)) try(mirai::stop_mirai(job$handle), silent = TRUE)
    finish()
    invisible(TRUE)
  }
  poll <- function() {
    if (!isTRUE(job$poll_active)) return(invisible(NULL))
    if (is.function(on_progress) && isTRUE(is_current())) {
      # The worker creates the file after it starts (and may be replacing it).
      progress_file <- file.path(artifact_dir, "progress.rds")
      p <- if (file.exists(progress_file)) {
        tryCatch(readRDS(progress_file), error = function(e) NULL, warning = function(w) NULL)
      }
      if (is.list(p)) try(on_progress(p$value, p$detail), silent = TRUE)
    }
    later::later(poll, delay = 0.5)
  }

  submit <- function() {
    if (isTRUE(job$cancelled)) return(invisible(NULL))
    if (!isTRUE(is_current())) {
      finish()
      return(invisible(NULL))
    }
    .wise_step2_async_ensure_daemon()
    task <- tryCatch({
      mirai::try_mirai({
        if (!isTRUE(getOption("wiseapp.async.worker_initialized", FALSE))) {
          if (isTRUE(development_package)) {
            pkgload::load_all(package_path, export_all = FALSE, helpers = FALSE,
              attach_testthat = FALSE, quiet = TRUE)
          } else {
            loadNamespace("wiseapp")
          }
          options(wiseapp.async.worker_initialized = TRUE)
        }
        # `worker` is the name of a package function; a function is accepted
        # for tests.
        fn <- if (is.character(worker)) utils::getFromNamespace(worker, "wiseapp") else worker
        fn(snapshot = snapshot, artifact_dir = artifact_dir)
      }, package_path = package_path, development_package = development_package,
        worker = worker, snapshot = snapshot, artifact_dir = artifact_dir,
        .compute = "default", .timeout = .wise_step2_async_timeout_ms("step3"))
    }, error = function(e) e)
    if (inherits(task, "error")) {
      finish()
      on_error(task)
    } else if (is.null(task)) {
      # Dispatcher memory cap: backpressure (another job's inputs are queued),
      # not a failure, but not forever either.
      waited <- proc.time()[["elapsed"]] - job$submit_started
      if (waited > .WISE_STEP3_SUBMIT_WAIT_SEC) {
        finish()
        on_error(simpleError(paste(
          "The background worker did not accept the job in time.",
          "Try again when the running job has finished."
        )))
      } else {
        later::later(submit, delay = 0.5)
      }
    } else {
      job$handle <- task
      later::later(poll, delay = 0.5)
      promises::then(task,
        onFulfilled = function(value) {
          job$poll_active <- FALSE
          if (isTRUE(job$cancelled)) return(invisible(NULL))
          if (!isTRUE(is_current())) return(finish())
          result <- tryCatch(convert(value, artifact_dir), error = function(e) e)
          finish()
          if (inherits(result, "error")) on_error(result) else on_result(result)
        },
        onRejected = function(e) {
          if (isTRUE(job$cancelled)) return(invisible(NULL))
          finish()
          if (isTRUE(is_current())) on_error(.wise_step2_async_describe_error(e))
        }
      )
    }
    invisible(NULL)
  }
  submit()
  invisible(job)
}

# Submit one Step 3 policy run (see `.wise_step3_async_task()`). `on_result`
# receives the same list as `step3_compute()`, plus `artifact` (the retained
# result file the metric workers read) and `timings`.
step3_async_submit <- function(snapshot, hs, ss, on_result, on_error,
                               on_progress = NULL, is_current = function() TRUE) {
  residuals <- snapshot$residuals
  .wise_step3_async_task(
    "step3_async_worker", snapshot,
    convert = function(manifest, artifact_dir) {
      step3_read_worker_result(manifest, artifact_dir, hs, ss, residuals)
    },
    on_result = on_result, on_error = on_error,
    on_progress = on_progress, is_current = is_current
  )
}


# Metric decomposition ----

#' Compute one metric decomposition in a worker process.
#'
#' Reads the retained Step 2 artifact and the retained Step 3 policy artifact
#' itself, runs `.policy_metric_decomposition()` and returns its (small) tables.
#'
#' @param snapshot List with `artifact` and `hs_overlay` (as for the policy
#'   run), `policy_artifact` (`file` of the retained policy result), `residuals`,
#'   and the decomposition arguments `method`, `pov_line`, `requested_residuals`,
#'   `endpoint_baseline`, `endpoint_policy`, `focus`, `unit`.
#' @param artifact_dir Directory for the progress file.
#' @export
step3_metric_worker <- function(snapshot, artifact_dir) {
  .wise_apply_thread_limits()
  old_rng_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_rng_kind)), add = TRUE)
  do.call(RNGkind, as.list(
    snapshot$rng_kind %||% c("Mersenne-Twister", "Inversion", "Rejection")
  ))
  step2 <- .wise_step3_read_step2(snapshot)
  policy <- .step3_unshare_constants(qs2::qs_read(
    snapshot$policy_artifact$file,
    nthreads = .WISE_STEP2_QS2_THREADS, validate_checksum = TRUE
  ))
  policy$pol_out <- .step3_relink_baseline(policy$pol_out, step2$hs, step2$ss)
  step3_validate_worker_run(policy)
  .policy_metric_decomposition(
    baseline_hist = step3_baseline_arm(step2$hs, snapshot$residuals),
    policy_hist = policy$pol_out$hist_sim,
    baseline_scenarios = step2$ss,
    policy_scenarios = policy$pol_out$saved_scenarios %||% list(),
    prepared = policy$pol_out$annual_channels,
    method = snapshot$method,
    pov_line = snapshot$pov_line,
    requested_residuals = snapshot$requested_residuals,
    endpoint_series_baseline = snapshot$endpoint_baseline,
    endpoint_series_policy = snapshot$endpoint_policy,
    focus_scenario = snapshot$focus,
    analysis_unit = snapshot$unit,
    validation_cache = new.env(parent = emptyenv())
  )
}

# The fields the main process set on `hist_sim` after the Step 2 artifact was
# written, for a metric job. They must equal what the policy-run worker used
# (hist_label, sim_summary and the Step 2 signature), because the policy result
# links its fields to that baseline. In the published baseline `.sig` is the
# policy signature; the Step 2 one is kept as `.step2_sig`.
.step3_metric_overlay <- function(baseline_hist) {
  out <- baseline_hist[intersect(c("hist_label", "sim_summary"), names(baseline_hist))]
  if (!is.null(baseline_hist$.step2_sig)) out$.sig <- baseline_hist$.step2_sig
  out
}

# Whether metric jobs can run: both retained artifacts exist.
.wise_step3_metric_async_available <- function(baseline_hist, policy_hist) {
  .wise_step3_async_enabled() && .wise_step2_async_enabled() &&
    !.wise_step2_async_sync() &&
    is.list(baseline_hist$.artifact) && file.exists(baseline_hist$.artifact$file %||% "") &&
    is.list(policy_hist$.artifact) && file.exists(policy_hist$.artifact$file %||% "")
}

# Submit one metric decomposition (see `.wise_step3_async_task()`).
step3_metric_submit <- function(snapshot, on_result, on_error,
                                is_current = function() TRUE) {
  .wise_step3_async_task(
    "step3_metric_worker", snapshot,
    convert = function(value, artifact_dir) value,
    on_result = on_result, on_error = on_error, is_current = is_current
  )
}
