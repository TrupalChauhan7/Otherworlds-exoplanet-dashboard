# ============================================================================
# 03_text_helpers.R
# Small helpers that turn the current filtered data into plain-English summary
# text and reactive value-box figures. Keeps wording logic out of the server.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# Most common level of a factor/character vector (ignoring NA), as text.
most_common <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("none")
  tb <- sort(table(x), decreasing = TRUE)
  names(tb)[1]
}

# Safe median with rounding, returns "n/a" when empty.
safe_median <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("n/a")
  format(round(median(x), digits), big.mark = ",")
}

# One-line caption under the animated method-share bar. The bar now carries the
# per-method numbers, so this states only the thesis: the single claim the tab
# exists to make. It is about the CATALOGUE and observational selection, never a
# claim about how many planets exist.
insight_sentence <- function(df) {
  n <- nrow(df)
  if (n == 0) {
    return("No planets match the current filters. Try widening the year range or method selection.")
  }
  top_method <- most_common(df$discovery_group)
  share      <- round(100 * mean(df$discovery_group == top_method, na.rm = TRUE))
  sprintf(paste0("%s found %d%% of these %s confirmed planets \u2014 the catalogue ",
                 "reflects how we look, not the true population."),
          top_method, share, format(n, big.mark = ","))
}

# Caption under the Host-stars proportion bar. Folds in the three numbers the
# stat cards used to carry (host stars, typical temperature, single-planet share).
# The closing clause is deliberately NOT the same sentence as this tab's "what to
# notice" line — one caveat per tab, phrased once, never copy-pasted.
hoststars_caption <- function(df) {
  d <- df[!duplicated(df$hostname) & !is.na(df$hostname), ]   # one row per host star
  n <- nrow(d)
  if (n == 0) {
    return("No host stars match the current filters. Try widening the year range or method selection.")
  }
  teff <- d$st_teff[!is.na(d$st_teff) & d$st_teff > 0]
  # Share is on TRUE system multiplicity (sy_pnum, one row per system) - the same
  # basis as the proportion bar and the multiplicity chart, so the three agree
  # under any filter. Counting filtered planets per host would inflate "single".
  m <- df[!is.na(df$hostname) & !is.na(df$sy_pnum), ]
  m <- m[!duplicated(m$hostname), ]
  temp_bit <- if (length(teff) > 0) {
    sprintf(", typically Sun-like (~%s K)", format(round(median(teff), -1), big.mark = ","))
  } else ""
  # If no system has a known planet count, the share clause has nothing to report -
  # drop it rather than print a placeholder.
  single_bit <- if (nrow(m) > 0) {
    sprintf(paste0("; %d%% have just one known planet \u2014 a limit of what we can ",
                   "detect as much as a fact about the systems."),
            round(100 * mean(m$sy_pnum == 1)))
  } else "."
  sprintf("In this selection: %s host stars%s%s",
          format(n, big.mark = ","), temp_bit, single_bit)
}

# Caption under the Earth-context selectivity bar. n_total is the FULL catalogue
# size, so the sentence states how selective the current filters are. The closing
# clause is the habitability guard — the one caveat that must never be dropped
# from this tab, however the filters are set.
earth_caption <- function(df, n_total) {
  n <- nrow(df)
  if (n == 0) {
    return(paste0("None of the ", format(n_total, big.mark = ","),
                  " confirmed planets match these filters. Widen the radius, ",
                  "temperature or distance range."))
  }
  sprintf(paste0("%s of %s confirmed planets match your filters \u2014 median radius %s R Earth, ",
                 "median period %s days. Matching a size or temperature filter says nothing ",
                 "about atmosphere, water, or life."),
          format(n, big.mark = ","), format(n_total, big.mark = ","),
          safe_median(df$pl_rade, 2), safe_median(df$pl_orbper, 1))
}

# Interpretation text shown on the Detection Bias tab.
bias_interpretation <- function(df) {
  n <- nrow(df)
  if (n == 0) return("No planets in the current selection.")
  top <- most_common(df$discovery_group)
  share <- round(100 * mean(df$discovery_group == top, na.rm = TRUE))
  sprintf(
    paste0("Within this selection, %s contributes %d%% of the planets. ",
           "This reflects the sensitivity of observing methods and survey design; ",
           "it should NOT be read as the true proportion of planets in the galaxy."),
    top, share
  )
}
