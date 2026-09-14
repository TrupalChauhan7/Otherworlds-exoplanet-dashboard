# ============================================================================
# 01_prepare_data.R
# Build the cleaned dashboard dataset from the raw NASA Exoplanet Archive export.
#
# Input : data/raw/PSCompPars_2026.07.21_16.45.43.csv  (never modified)
# Output: data/processed/exoplanets_dashboard.csv
#
# Design decisions (see Design Freeze document):
#   - One row per confirmed planet (PSCompPars); pl_name is unique -> no dedup.
#   - Keep 17 meaningful fields + 3 derived fields; drop the 60+ uncertainty/flag cols.
#   - Keep missing values in the data; charts use plot-specific complete cases.
#   - st_spectype is retained but is NOT a core filter (62.9% missing).
# ============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(forcats)
})

# --- Locate raw + processed paths -------------------------------------------
raw_candidates <- c(
  file.path("data", "raw", "PSCompPars_2026.07.21_16.45.43.csv"),
  "PSCompPars_2026.07.21_16.45.43.csv"
)
raw_file <- raw_candidates[file.exists(raw_candidates)][1]

out_dir  <- file.path("data", "processed")
out_path <- file.path(out_dir, "exoplanets_dashboard.csv")

# Idempotency guard: rebuild ONLY when the processed file is missing or older
# than the raw export. Without this, Shiny's automatic R/ auto-loading re-runs
# this whole pipeline (and rewrites the file) on every app launch.
needs_build <- !file.exists(out_path) ||
  (!is.na(raw_file) && file.mtime(raw_file) > file.mtime(out_path))

if (needs_build) {
  if (is.na(raw_file)) {
    stop("Raw NASA CSV not found in data/raw/ or project root.")
  }

  # NASA exports begin with metadata lines starting with '#'. comment = '#'
  # tells readr to skip them and read the real header row that follows.
  raw <- read_csv(raw_file, comment = "#", show_col_types = FALSE)

# --- The 17 fields we keep ---------------------------------------------------
selected_vars <- c(
  "pl_name", "hostname", "sy_pnum",
  "discoverymethod", "disc_year", "disc_facility",
  "pl_rade", "pl_bmasse", "pl_orbper", "pl_orbsmax", "pl_eqt", "pl_insol",
  "st_teff", "st_rad", "st_mass", "st_spectype",
  "sy_dist"
)
stopifnot(all(selected_vars %in% names(raw)))

exo <- raw |> select(all_of(selected_vars))

# --- Derived variables -------------------------------------------------------

# discovery_group: collapse 11 raw methods into 5 readable, legend-friendly
# groups. Keeps four named methods and buckets the rare tail into "Other".
main_methods <- c("Transit", "Radial Velocity", "Microlensing", "Imaging")

exo <- exo |>
  mutate(
    discovery_group = if_else(discoverymethod %in% main_methods,
                              discoverymethod, "Other"),
    discovery_group = factor(
      discovery_group,
      levels = c("Transit", "Radial Velocity", "Microlensing", "Imaging", "Other")
    ),

    # discovery_era: three survey eras + implicit custom via the year slider.
    discovery_era = case_when(
      disc_year <= 2008 ~ "Early (1992-2008)",
      disc_year <= 2018 ~ "Kepler era (2009-2018)",
      TRUE              ~ "TESS/current (2019-2026)"
    ),
    discovery_era = factor(
      discovery_era,
      levels = c("Early (1992-2008)", "Kepler era (2009-2018)", "TESS/current (2019-2026)")
    ),

    # planet_size_group: intuitive size bands from planet radius (Earth radii).
    # Thresholds are stated in the report; NA radius stays NA (not a category).
    planet_size_group = case_when(
      is.na(pl_rade)   ~ NA_character_,
      pl_rade < 1.25   ~ "Earth-size (<1.25 R Earth)",
      pl_rade < 2      ~ "Super-Earth (1.25-<2 R Earth)",
      pl_rade < 4      ~ "Sub-Neptune (2-<4 R Earth)",
      pl_rade < 6      ~ "Neptune-size (4-<6 R Earth)",
      TRUE             ~ "Giant (>=6 R Earth)"
    ),
    planet_size_group = factor(
      planet_size_group,
      levels = c("Earth-size (<1.25 R Earth)", "Super-Earth (1.25-<2 R Earth)",
                 "Sub-Neptune (2-<4 R Earth)", "Neptune-size (4-<6 R Earth)",
                 "Giant (>=6 R Earth)")
    ),

    # Type conversions
    disc_year        = as.integer(disc_year),
    sy_pnum          = as.integer(sy_pnum),
    discoverymethod  = as.factor(discoverymethod),
    disc_facility    = as.factor(disc_facility),
    st_spectype      = na_if(trimws(st_spectype), "")
  )

# --- Integrity check ---------------------------------------------------------
  if (anyDuplicated(exo$pl_name) > 0) {
    warning("Duplicate planet names detected - PSCompPars should be one row per planet.")
  }

  # --- Write processed dataset -----------------------------------------------
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write_csv(exo, out_path)
  message(sprintf("Prepared %d planets, %d columns; wrote %s",
                  nrow(exo), ncol(exo), out_path))
}
