# The reactive summary sentences must never crash on an empty selection (a filter
# combination that matches nothing) and must summarise a real one sensibly.

test_that("most_common returns the modal non-NA value", {
  expect_equal(most_common(c("a", "a", "b")), "a")
  expect_equal(most_common(c("x", NA, NA, "x", "y")), "x")
})

test_that("most_common degrades gracefully on empty / all-NA input", {
  expect_equal(most_common(character(0)), "none")
  expect_equal(most_common(c(NA, NA)), "none")
})

test_that("safe_median rounds, formats, and handles empty input", {
  expect_equal(safe_median(c(1, 2, 3)), "2")
  expect_equal(safe_median(c(1, 2, 3, 4)), "2.5")
  expect_equal(safe_median(numeric(0)), "n/a")
  expect_equal(safe_median(c(NA, NA)), "n/a")
})

test_that("insight_sentence returns a friendly message on an empty selection", {
  msg <- insight_sentence(exo_test[0, ])
  expect_type(msg, "character")
  expect_match(msg, "No planets", fixed = TRUE)
})

test_that("insight_sentence summarises a real selection with a percentage", {
  msg <- insight_sentence(exo_test)
  expect_type(msg, "character")
  expect_match(msg, "%")
})

test_that("bias_interpretation handles empty and non-empty selections", {
  expect_equal(bias_interpretation(exo_test[0, ]), "No planets in the current selection.")
  expect_match(bias_interpretation(exo_test), "%")
})

test_that("earth_caption reports the catalogue total it is given", {
  msg <- earth_caption(exo_test[0, ], n_total = 6324)
  expect_match(msg, "6,324", fixed = TRUE)
})
