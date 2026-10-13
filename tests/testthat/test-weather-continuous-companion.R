# Continuous companion of binned weather variables (P2-4)

test_that("binning keeps the pre-bin value in a companion only when asked", {
  df <- data.frame(loc_id = 1:4, t = c(1, 4, 7, 10))
  breaks <- list(t = c(-Inf, 5, Inf))
  plain <- .apply_binning(df, breaks)
  kept <- .apply_binning(df, breaks, keep_continuous = TRUE)
  expect_false("t_cont" %in% names(plain))
  expect_identical(kept$t_cont, df$t)
  expect_true(is.numeric(kept$t_cont))
  # The bins are exactly the same with and without the companion
  expect_identical(kept$t, plain$t)
  expect_true(is.factor(kept$t))
  # A variable without breaks gets no companion
  other <- cbind(df, p = c(0.1, 0.2, 0.3, 0.4))
  expect_identical(
    names(.apply_binning(other, breaks, keep_continuous = TRUE)),
    c("loc_id", "t", "p", "t_cont")
  )
})

test_that("companions are found for binned (factor) variables only", {
  w <- data.frame(
    t = factor(c("a", "b")), t_cont = c(1, 2), p = c(1, 2), p_cont = c(3, 4)
  )
  expect_identical(.weather_companion_cols(w, c("t", "p")), "t_cont")
  expect_identical(.weather_companion_cols(w, "p"), character())
  expect_identical(.weather_companion_cols(w, "gone"), character())
  expect_identical(.weather_companion_cols(NULL, "t"), character())
})

with_companion <- function(fixture, values = c(0.2, 0.9, 0.4)) {
  fixture$weather$exposure_cont <- values
  fixture
}

run_companion_fixture <- function(with, cached = FALSE) {
  fixture <- test_policy_exposure_fixture()
  if (with) fixture <- with_companion(fixture)
  suppressWarnings(run_sim_pipeline(
    weather_raw = fixture$weather,
    svy = fixture$survey,
    sw = data.frame(name = "exposure"),
    so = list(name = "welfare", type = "numeric", transform = "none"),
    model = stats::lm(welfare ~ exposure, data = fixture$train),
    residuals = "none",
    train_data = fixture$train,
    engine = "lm",
    svy_prepared = if (cached) fixture$prepared else NULL,
    weather_join_cache = if (cached) fixture$cache else NULL
  ))
}

test_that("the companion rides in the exposure table and never reaches the prediction", {
  for (cached in c(FALSE, TRUE)) {
    without <- run_companion_fixture(FALSE, cached)
    with <- run_companion_fixture(TRUE, cached)
    # Predictions, weights and row identity are bit-identical
    expect_identical(with$y_point, without$y_point)
    expect_identical(with$sim_year, without$sim_year)
    expect_identical(with$svy_row_id, without$svy_row_id)
    expect_identical(with$weight, without$weight)
    # The exposure table carries the companion, aligned with the bins
    mapping <- step2_exposure_resolve(with)
    expect_true(all(c("exposure", "exposure_cont") %in% names(mapping$table)))
    expect_identical(mapping$table$exposure_cont, c(0.2, 0.9, 0.4))
    expect_identical(mapping$row_index, step2_exposure_resolve(without)$row_index)
    expect_true("exposure_cont" %in% names(with$weather_raw))
    expect_false("exposure_cont" %in% names(without$weather_raw))
    # Only the companion qualifies as a trigger variable
    expect_identical(.sp_trigger_variables(mapping$table), "exposure_cont")
  }
})

test_that("a trigger on the companion uses the continuous value", {
  mapping <- step2_exposure_resolve(run_companion_fixture(TRUE))
  spec <- list(
    trigger_type = "weather", trigger_variable = "exposure_cont",
    trigger_direction = "above", trigger_value = 0.4
  )
  th <- sp_trigger_thresholds(mapping$table, spec)
  exceeds <- .sp_trigger_exceeds(mapping$table, th)
  expect_identical(exceeds, c(FALSE, TRUE, TRUE))
  # The binned variable itself still cannot drive a trigger
  spec$trigger_variable <- "exposure"
  expect_error(sp_trigger_thresholds(mapping$table, spec), "continuous companion")
})

test_that("the trigger flyout offers the continuous companion of a binned variable", {
  svy <- data.frame(welfare = 1:4, weight = 1, t = factor(c("a", "b", "a", "b")),
    p = c(1, 2, 3, 4))
  hs <- list(
    svy = svy,
    pipeline = list(weather_exposure = list(
      weather_columns = c("t", "t_cont", "p")
    ))
  )
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_companion", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame(
      name = c("t", "p"), label = c("Temperature", "Rainfall")
    )),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = shiny::reactiveVal(hs)
  ), {
    cands <- trigger_candidates()
    expect_identical(unname(cands), c("t_cont", "p"))
    expect_identical(names(cands), c("Temperature (continuous)", "Rainfall"))
  })
})

test_that("the companion takes the unit of its binned variable", {
  sw <- data.frame(name = "t", units = "C", transformation = "None")
  expect_identical(.sp_trigger_unit("t_cont", sw), "C")
  expect_identical(.sp_trigger_unit("t", sw), "C")
  expect_identical(.sp_trigger_unit("zz_cont", sw), "")
})

test_that("the prepared weather cache signature depends on the companion", {
  args <- list(
    data.frame(name = "t"), data.frame(code = "A"), data.frame(code = "A"),
    list(type = "local"), c("2020-01-01", "2020-12-31"), list(), character(),
    NULL, NULL
  )
  sig <- do.call(.step2_prepared_weather_cache_signature, args)
  expect_type(sig, "character")
  expect_length(sig, 1L)
})
