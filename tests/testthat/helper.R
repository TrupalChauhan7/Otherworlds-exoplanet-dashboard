# Shared setup for the test suite. testthat sources helper*.R before the tests.
# We locate the project root, source the two pure-R helper modules (no Shiny is
# needed to test them), and load the processed dataset with the same factor
# levels app.R applies at startup, so the chart builders get the columns they
# expect.

find_project_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "app.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("Could not locate project root (app.R not found).")
    d <- parent
  }
}

PROJ_ROOT <- find_project_root()

source(file.path(PROJ_ROOT, "R", "02_plot_helpers.R"))
source(file.path(PROJ_ROOT, "R", "03_text_helpers.R"))

METHOD_LEVELS <- c("Transit", "Radial Velocity", "Microlensing", "Imaging", "Other")
SIZE_LEVELS <- c("Earth-size (<1.25 R Earth)", "Super-Earth (1.25-<2 R Earth)",
                 "Sub-Neptune (2-<4 R Earth)", "Neptune-size (4-<6 R Earth)",
                 "Giant (>=6 R Earth)")

load_processed <- function() {
  path <- file.path(PROJ_ROOT, "data", "processed", "exoplanets_dashboard.csv")
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  d$discovery_group   <- factor(d$discovery_group, levels = METHOD_LEVELS)
  d$planet_size_group <- factor(d$planet_size_group, levels = SIZE_LEVELS)
  d$disc_facility     <- as.factor(d$disc_facility)
  d
}

exo_test <- load_processed()
