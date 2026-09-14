# Every chart builder should return a ggplot for the real data without error.
# Building the object wires up the aesthetics and scales but does not render, so
# these stay fast and headless (no graphics device, no plotly).

test_that("all chart builders return ggplot objects on the full dataset", {
  d <- exo_test
  expect_s3_class(plot_timeline(d, "annual"), "ggplot")
  expect_s3_class(plot_timeline(d, "cumulative"), "ggplot")
  expect_s3_class(plot_facilities(d, metric = "count"), "ggplot")
  expect_s3_class(plot_facilities(d, metric = "share"), "ggplot")
  expect_s3_class(plot_size_mix(d), "ggplot")
  expect_s3_class(plot_star_temp(d), "ggplot")
  expect_s3_class(plot_system_multiplicity(d), "ggplot")
  expect_s3_class(plot_radius_period(d, show_earth = TRUE), "ggplot")
  expect_s3_class(plot_radius_period(d, show_earth = FALSE), "ggplot")
  expect_s3_class(plot_distribution(d, "pl_rade"), "ggplot")
  expect_s3_class(plot_distribution(d, "pl_bmasse"), "ggplot")
  expect_s3_class(plot_bias_facets(d), "ggplot")
  expect_s3_class(plot_period_by_method(d), "ggplot")
  expect_s3_class(plot_earth_context(d), "ggplot")
})

test_that("the timeline and signature scatter actually compute (stat layers run)", {
  built_tl <- ggplot2::ggplot_build(plot_timeline(exo_test))
  expect_type(built_tl$data, "list")
  expect_gt(length(built_tl$data), 0)

  built_rp <- ggplot2::ggplot_build(plot_radius_period(exo_test))
  expect_type(built_rp$data, "list")
  expect_gt(length(built_rp$data), 0)
})

test_that("chart builders survive a tiny single-method selection", {
  small <- exo_test[exo_test$discovery_group == "Imaging", ]
  expect_s3_class(plot_radius_period(small), "ggplot")
  expect_s3_class(plot_distribution(small, "pl_rade"), "ggplot")
})
