# The processed dataset is the contract the whole dashboard depends on. If a data
# refresh ever changed its shape, these tests fail loudly instead of the app
# breaking silently at runtime.

test_that("processed dataset honours the 20-column contract", {
  expect_equal(ncol(exo_test), 20)
  expected <- c("pl_name", "hostname", "sy_pnum", "discoverymethod", "disc_year",
                "disc_facility", "pl_rade", "pl_bmasse", "pl_orbper", "pl_orbsmax",
                "pl_eqt", "pl_insol", "st_teff", "st_rad", "st_mass", "st_spectype",
                "sy_dist", "discovery_group", "discovery_era", "planet_size_group")
  expect_setequal(names(exo_test), expected)
})

test_that("every planet is unique (PSCompPars is one row per planet)", {
  expect_false(any(duplicated(exo_test$pl_name)))
})

test_that("the dataset is the expected NASA snapshot size", {
  expect_gt(nrow(exo_test), 6000)
})

test_that("discovery_group is a clean 5-level factor with every planet grouped", {
  expect_true(is.factor(exo_test$discovery_group))
  expect_setequal(levels(exo_test$discovery_group), METHOD_LEVELS)
  expect_true(all(!is.na(exo_test$discovery_group)))
})

test_that("planet_size_group uses only the defined ordinal bands", {
  expect_true(is.factor(exo_test$planet_size_group))
  expect_setequal(levels(exo_test$planet_size_group), SIZE_LEVELS)
  observed <- unique(as.character(stats::na.omit(exo_test$planet_size_group)))
  expect_true(all(observed %in% SIZE_LEVELS))
})

test_that("core numeric fields are physically plausible", {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  yr <- exo_test$disc_year[!is.na(exo_test$disc_year)]
  expect_true(all(yr >= 1989 & yr <= this_year + 1))   # first confirmed exoplanets ~1992
  rade <- exo_test$pl_rade[!is.na(exo_test$pl_rade)]
  expect_true(all(rade >= 0))         # radii are non-negative
  expect_gt(stats::median(rade), 0)   # and the typical planet has a real size
  pnum <- exo_test$sy_pnum[!is.na(exo_test$sy_pnum)]
  expect_true(all(pnum >= 1))
})
