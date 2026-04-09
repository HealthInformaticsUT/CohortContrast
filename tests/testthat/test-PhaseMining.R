test_that("nGramClusterSummarization returns one row per cluster", {
  result <- tibble::tibble(
    ngram_cluster = c(1L, 1L, 2L),
    ngram_names = list(
      c("A", "B"),
      c("A", "C"),
      c("D", "E")
    ),
    n_unique_persons = c(2, 3, 2),
    unique_persons_ids = list(
      c("p1", "p2"),
      c("p2", "p3", "p4"),
      c("p5", "p6")
    ),
    ngram_time = list(c(1, 2), c(3, 4), c(5, 6))
  )

  summary <- CohortContrast::nGramClusterSummarization(result, top_n = 2)

  expect_s3_class(summary, "data.frame")
  expect_equal(sort(summary$ngram_cluster), c(1L, 2L))
  expect_equal(nrow(summary), 2)
  expect_true("top_concepts" %in% names(summary))
})
