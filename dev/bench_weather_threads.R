# Focused S2-P16 weather-only benchmark.
#
# This intentionally does not call dev/bench_step2.R. It loads one survey,
# constructs one weather variable, and calls get_weather() directly. Use it for
# fast thread/rounding characterization before running any broader benchmark.
#
# Example:
#   WISEAPP_WEATHER_COUNTRY=COL \
#   WISEAPP_WEATHER_YEAR=2018 \
#   WISEAPP_WEATHER_VAR=t \
#   WISEAPP_WEATHER_THREADS=1,2 \
#   WISEAPP_WEATHER_REPETITIONS=1 \
#   Rscript dev/bench_weather_threads.R

options(golem.app.prod = FALSE)
devtools::load_all(quiet = TRUE)

.env <- function(name, default = "") Sys.getenv(name, unset = default)
.env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(.env(name)))
  if (is.na(value)) default else value
}

data_path <- .env("WISEAPP_DATA_PATH")
if (!nzchar(data_path)) {
  stop("Set WISEAPP_DATA_PATH to the local data directory.", call. = FALSE)
}
data_path <- normalizePath(path.expand(data_path), mustWork = TRUE)

country <- .env("WISEAPP_WEATHER_COUNTRY", "COL")
year <- .env_int("WISEAPP_WEATHER_YEAR", 2018L)
weather_name <- .env("WISEAPP_WEATHER_VAR", "t")
threads <- strsplit(.env("WISEAPP_WEATHER_THREADS", "1,2"), ",", fixed = TRUE)[[1L]]
threads <- unique(trimws(threads))
if (!all(threads %in% c("1", "2"))) {
  stop("WISEAPP_WEATHER_THREADS must contain only 1 and/or 2.", call. = FALSE)
}
repetitions <- max(1L, .env_int("WISEAPP_WEATHER_REPETITIONS", 1L))
future <- identical(.env("WISEAPP_WEATHER_FUTURE", "0"), "1")
weather_binned <- identical(.env("WISEAPP_WEATHER_BINNED", "0"), "1")
perturbation <- .env("WISEAPP_WEATHER_PERTURBATION", "additive")
if (!perturbation %in% c("additive", "multiplicative")) {
  stop("WISEAPP_WEATHER_PERTURBATION must be additive or multiplicative.", call. = FALSE)
}

params <- build_connection_params("local", path = data_path)
metadata <- load_overview_metadata(params)
surveys <- build_survey_fnames(metadata$survey_list, "hh", params)
surveys <- surveys[surveys$code == country & surveys$year == year, , drop = FALSE]
if (!nrow(surveys)) {
  stop("No household survey found for ", country, " / ", year, call. = FALSE)
}

survey_data <- load_data(surveys$fname[[1L]], params, collect = TRUE,
                         unify_schemas = TRUE)
survey_data <- add_time_columns(survey_data)
dates <- extract_survey_dates(survey_data)
weather_info <- get_weather_vars(metadata$variable_list)
if (!weather_name %in% weather_info$name) {
  stop("Weather variable not found: ", weather_name, call. = FALSE)
}
selected_weather <- data.frame(
  name = weather_name,
  ref_start = 1L,
  ref_end = 3L,
  temporalAgg = temporal_agg_default(
    weather_info$units[match(weather_name, weather_info$name)]
  ),
  cont_binned = if (weather_binned) "Binned" else "Continuous",
  binning_method = if (weather_binned) "Equal frequency" else NA_character_,
  num_bins = if (weather_binned) 3L else NA_integer_,
  transformation = "None",
  stringsAsFactors = FALSE
)

rss <- function() .wx_process_tree_rss_bytes()
run <- function(mode) {
  values <- vector("list", repetitions)
  for (i in seq_len(repetitions)) {
    gc()
    before <- rss()
    started <- proc.time()[["elapsed"]]
    result <- get_weather(
      survey_data = survey_data,
      selected_surveys = surveys,
      selected_weather = selected_weather,
      dates = dates,
      connection_params = params,
      ssp = if (future) "ssp2_4_5" else NULL,
      future_period = if (future) list(c("2040-01-01", "2060-12-01")) else NULL,
      perturbation_method = if (future) setNames(perturbation, weather_name) else NULL,
      weather_collect = "bounded",
      weather_threads = mode
    )
    elapsed <- proc.time()[["elapsed"]] - started
    after <- rss()
    values[[i]] <- list(
      elapsed = elapsed,
      rss_before = before,
      rss_after = after,
      rows = sum(vapply(result, nrow, integer(1L))),
      selected_threads = attr(result, "weather_collection_policy")$weather_threads$selected_threads,
      result = result
    )
  }
  values
}

baseline <- NULL
canonical <- function(value, digits) {
  result <- lapply(value, function(frame) {
    if (!is.data.frame(frame)) return(frame)
    .wx_round_weather_values(frame, weather_name, digits = digits)
  })
  continuous <- attr(value, "continuous_weather")
  if (!is.null(continuous)) {
    attr(result, "continuous_weather") <- .wx_round_weather_values(
      continuous, weather_name, digits = digits
    )
  }
  result
}
for (mode in threads) {
  runs <- run(mode)
  for (i in seq_along(runs)) {
    item <- runs[[i]]
    if (is.null(baseline)) baseline <- item$result
    parity <- vapply(c(3L, 5L), function(digits) {
      identical(canonical(baseline, digits), canonical(item$result, digits))
    }, logical(1L))
    cat(sprintf(
      "future=%s perturbation=%s binned=%s mode=%s repetition=%d elapsed=%.3f rss_before=%.0f rss_after=%.0f rows=%d selected=%d parity3=%s parity5=%s\n",
      future, perturbation, weather_binned, mode, i, item$elapsed, item$rss_before, item$rss_after,
      item$rows, item$selected_threads, parity[[1L]], parity[[2L]]
    ))
  }
}
