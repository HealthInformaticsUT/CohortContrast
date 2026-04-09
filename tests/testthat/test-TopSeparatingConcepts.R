library(testthat)
library(CohortContrast)

test_that("getTopSeparatingConcepts returns top concepts for explicit k", {
  skip_if_not_installed("nanoparquet")

  summaryPath <- system.file("example", "st", "lc500s", package = "CohortContrast")
  skip_if(identical(summaryPath, ""), "Bundled summary example not available.")

  result <- getTopSeparatingConcepts(
    studyPath = summaryPath,
    n = 5,
    k = 3
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_named(result, c("id", "name", "enrichment", "target_prevalence"))
  expect_true(all(nzchar(result$id)))
})

test_that("getTopSeparatingConcepts auto k matches metadata best k", {
  skip_if_not_installed("nanoparquet")

  summaryPath <- system.file("example", "st", "lc500s", package = "CohortContrast")
  skip_if(identical(summaryPath, ""), "Bundled summary example not available.")

  autoResult <- getTopSeparatingConcepts(
    studyPath = summaryPath,
    n = 5,
    k = "auto"
  )

  k3Result <- getTopSeparatingConcepts(
    studyPath = summaryPath,
    n = 5,
    k = 3
  )

  expect_equal(attr(autoResult, "k"), 3L)
  expect_equal(autoResult$id, k3Result$id)
})
