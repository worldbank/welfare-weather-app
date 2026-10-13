# Compare interview-date chart palettes with a local sample.
#
# Run from the repository root:
#   Rscript dev/compare_interview_dates.R

source("R/utils_plot_theme.R")
source("R/fct_surveystats.R")

data_root <- Sys.getenv("WISEAPP_DATA_PATH", unset = "")
if (!nzchar(data_root)) stop("Set WISEAPP_DATA_PATH to the local data folder.")
data_root <- path.expand(data_root)
country_codes <- c("BEN", "BFA")
survey_files <- unlist(lapply(country_codes, function(code) {
  list.files(
    file.path(data_root, "microdata", "hh", code),
    pattern = "[.]parquet$",
    full.names = TRUE
  )
}), use.names = FALSE)
if (length(survey_files) == 0L) stop("No local household files found.")

survey_data <- do.call(rbind, lapply(survey_files, function(path) {
  as.data.frame(arrow::read_parquet(
    path,
    col_select = c("economy", "year", "timestamp")
  ))
}))
survey_data <- add_time_columns(survey_data)
plot_data <- summarise_interview_dates(survey_data)

plots <- list(
  "Recommended: sequential navy-to-blue" = plot_interview_dates(
    plot_data, "grouped", palette = "sequential"
  ),
  "WISE brand colors" = plot_interview_dates(
    plot_data, "grouped", palette = "wise"
  ),
  "Okabe-Ito categorical" = plot_interview_dates(
    plot_data, "grouped", palette = "okabe_ito"
  )
)

svg_string <- function(plot) {
  svglite::stringSVG(
    print(plot),
    width = 10.5,
    height = if (inherits(plot, "ggplot") &&
                identical(plot$labels$y, NULL)) 4.5 else 4.2
  )
}

cards <- vapply(names(plots), function(title) {
  paste0(
    '<article class="option">',
    '<h2>', htmltools::htmlEscape(title), '</h2>',
    '<p class="recommendation">',
    switch(title,
      "Recommended: sequential navy-to-blue" = "Recommended: older waves begin in navy, transition through lighter navy, and recent waves end in brighter blues.",
      "WISE brand colors" = "Strongest brand alignment, but the colors do not encode the natural time order.",
      "Okabe-Ito categorical" = "Colorblind-safe, but unordered colors are less meaningful for survey years."
    ),
    '</p><div class="chart">', svg_string(plots[[title]]), '</div></article>'
  )
}, character(1))

html <- paste0(
  '<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Interview dates palette options</title><style>',
  ':root{font-family:"Open Sans",Arial,sans-serif;color:#1d2a35;',
  'background:#f7f9fb}body{max-width:1400px;margin:0 auto;padding:32px 24px;',
  'background:#f7f9fb}header{margin-bottom:24px}h1{color:#002244;',
  'font-size:clamp(1.6rem,3vw,2.35rem);margin:0 0 8px}header p{color:#5b6b79;',
  'margin:0;line-height:1.5}.option{background:#fff;border:1px solid #d9e1e7;',
  'border-radius:12px;box-shadow:0 4px 16px rgba(0,34,68,.08);margin:20px 0;',
  'padding:20px 22px}.option h2{color:#002244;font-size:1.2rem;margin:0 0 4px;',
  'font-weight:600}.recommendation{color:#5b6b79;font-size:.9rem;margin:0 0 10px;',
  'min-height:1.35em}.chart{overflow-x:auto}.chart svg{display:block;',
  'max-width:100%;height:auto}@media(max-width:700px){body{padding:20px 12px;',
  '}.option{padding:14px 12px;border-radius:9px}}',
  '</style></head><body><header><h1>Interview dates palette options</h1>',
  '<p>Preview of the approved grouped-column layout using Benin and Burkina Faso',
  ' household records from the local data folder. Each country has a distinct',
  ' sequential colour family, with survey years ordered within each family.',
  ' The production default is sequential navy-to-blue. The y-axis label is reactive: Households, Individuals,',
  ' or Firms follows the selected analysis level.</p></header>',
  paste(cards, collapse = ""),
  '</body></html>'
)

out_dir <- file.path("dev", "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, "interview_dates_options.html")
writeLines(html, out_path, useBytes = TRUE)
message("Wrote: ", normalizePath(out_path))
