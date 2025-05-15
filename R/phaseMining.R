#' Function for discovering ngrams from the extracted data from CohortContrast function
#'
#' @param data CohortContrastObject returned by CohortContrast or GUI snapshot
#' @param collapse_size integer for days to use for collapse same concept ids
#' @export
nGramDiscovery <- function(data, collapse_size = 0) {
  pre <- preprocess_data(data, collapse_size)
  collapsed_long <- pre$collapsed
  map_v <- pre$features %>%
    dplyr::select(concept_id = CONCEPT_ID, concept_name = CONCEPT_NAME) %>%
    dplyr::distinct()

  cli::cli_alert_warning("Running n-gram discovery...")
  all_phases <- running_ngrams(collapsed_long, map_v)
  cli::cli_alert_success("Running n-gram discovery.")
  if (nrow(all_phases) == 0) {
    cli::cli_alert_warning("No significant n-grams found.")
    return(NULL)
  }

  cli::cli_alert_warning("Running n-gram clustering...")
  clustered <- run_clustering(all_phases)
  cli::cli_alert_success("Running n-gram clustering.")
  result <- clustered %>% dplyr::select(ngram_label = name_ngram,
                                        ngram_cluster = cluster,
                                        ngram_ids = id_list,
                                        ngram_names = concept_names,
                                        observed_ngram_count = observed_count,
                                        expected_ngram_count = expected_count,
                                        overrep_ratio,
                                        n_unique_persons = n_persons,
                                        unique_persons_ids = person_ids,
                                        ngram_time_diff = time_diff,
                                        ngram_time = time,
                                        observed_count_p_value = p_value,
                                        observed_test = sig_pass,
                                        persons_count_p_value = binom_pval,
                                        persons_test = person_pass)
  return(result)
}

#' Function for summarizing ngrams from nGramDiscovery output
#'
#' @param result output from nGramDiscovery
#' @param top_n integer for number of most frequent concept to show per cluster
#' @export
nGramClusterSummarization <- function(result, top_n = 5) {
  cluster_summary <- result %>%
    dplyr::group_by(ngram_cluster) %>%
    dplyr::summarise(
      n_phases = dplyr::n(),
      avg_persons = mean(n_unique_persons),
      total_unique_patients = length(unique(unlist(unique_persons_ids))),
      avg_time = mean(unlist(ngram_time), na.rm = TRUE),
      .groups = 'drop'
    )

  top_concepts <- result %>%
    tidyr::unnest(ngram_names) %>%
    dplyr::group_by(ngram_cluster, ngram_names) %>%
    dplyr::summarise(freq = length(unique(unlist(unique_persons_ids))), .groups = 'drop') %>%
    dplyr::group_by(ngram_cluster) %>%
    dplyr::slice_max(freq, n = top_n) %>%
    dplyr::summarise(
      ngram_cluster = as.integer(ngram_cluster),
      top_concepts = paste(unique(ngram_names), collapse = " + ")
    )

  result <- dplyr::left_join(cluster_summary, top_concepts, by = "ngram_cluster") %>%
    dplyr::distinct() %>%
    dplyr::arrange(avg_time)
  return(result)
}

#' @keywords internal
preprocess_data <- function(data, collapse_size = 0) {
  features <- data$data_features %>%
    dplyr::filter(ABSTRACTION_LEVEL == -1, ZTEST, TARGET_SUBJECT_PREVALENCE > 0.01)

  patients <- data$data_patients %>%
    dplyr::filter(
      CONCEPT_ID %in% unique(features$CONCEPT_ID),
      ABSTRACTION_LEVEL == -1,
      COHORT_DEFINITION_ID == "target"
    )

  long <- patients %>%
    tidyr::unnest(TIME_TO_EVENT) %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT)

  collapsed <- long %>%
    dplyr::arrange(PERSON_ID, CONCEPT_ID, TIME_TO_EVENT) %>%
    dplyr::group_by(PERSON_ID, CONCEPT_NAME, CONCEPT_ID) %>%
    dplyr::mutate(
      time_diff = TIME_TO_EVENT - dplyr::lag(TIME_TO_EVENT, default = dplyr::first(TIME_TO_EVENT)),
      new_group = cumsum(ifelse(dplyr::row_number() == 1, 1, time_diff > collapse_size))
    ) %>%
    dplyr::group_by(PERSON_ID, CONCEPT_ID, CONCEPT_NAME, new_group) %>%
    dplyr::summarise(TIME_TO_EVENT = dplyr::first(TIME_TO_EVENT), .groups = "drop") %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT)

  list(collapsed = collapsed, features = features)
}


#' @keywords internal
find_enriched_ngrams <- function(data, n = 2, prev_probs = 1) {
  stopifnot(n >= 2)

  filtered <- data %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT) %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::ungroup()

  patients <- filtered %>%
    dplyr::count(PERSON_ID, name = "n_events") %>%
    dplyr::filter(n_events >= n)

  filtered <- dplyr::semi_join(filtered, patients, by = "PERSON_ID")

  extract_ngrams <- function(vec, n) {
    if (length(vec) < n) return(NULL)
    stats::embed(vec, n)[, n:1, drop = FALSE]
  }
  make_ngram <- function(mat) {
    apply(mat, 1, function(row) paste(sort(row), collapse = "-"))
  }

  observed <- filtered %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT) %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::summarise(
      grams = list({
        ids <- CONCEPT_ID
        times <- TIME_TO_EVENT
        ngrams <- extract_ngrams(ids, n)
        timegrams <- extract_ngrams(times, n)
        if (is.null(ngrams)) return(tibble::tibble())
        tibble::tibble(
          ngram = make_ngram(ngrams),
          time_diff = apply(timegrams, 1, function(x) max(x) - min(x)),
          start_time = apply(timegrams, 1, min),
          person = as.character(PERSON_ID[1])
        )
      }),
      .groups = "drop"
    ) %>% tidyr::unnest(grams)

  summary <- observed %>%
    dplyr::group_by(ngram) %>%
    dplyr::summarise(
      observed_count = dplyr::n(),
      n_persons = dplyr::n_distinct(person),
      person_ids = list(unique(person)),
      time_diff = mean(time_diff, na.rm = TRUE),
      time = mean(start_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_persons > length(unique(data$PERSON_ID)) * prev_probs)

  concept_counts <- filtered %>%
    dplyr::count(PERSON_ID, CONCEPT_ID, name = "count")

  concept_counts_matrix <- concept_counts %>%
    tidyr::pivot_wider(names_from = CONCEPT_ID, values_from = count, values_fill = 0)
  concept_counts_matrix$total_n <- rowSums(concept_counts_matrix[,-1])

  n_ops_df <- patients %>%
    dplyr::mutate(n_ops = pmax(n_events - n + 1, 0)) %>%
    dplyr::select(PERSON_ID, n_ops) %>%
    dplyr::mutate(PERSON_ID = as.character(PERSON_ID))

  expected <- summary %>%
    dplyr::mutate(concept_ids = stringr::str_split(ngram, "-", simplify = FALSE)) %>%
    dplyr::mutate(expected_count = purrr::map_dbl(concept_ids, function(ids) {
      concepts <- ids[ids %in% colnames(concept_counts_matrix)]
      if (length(concepts) < n) return(0)
      rows <- concept_counts_matrix[, c("total_n", concepts, "PERSON_ID"), drop = FALSE]
      expected_probs <- apply(rows, 1, function(row) {
        n_total <- row[["total_n"]]
        counts <- as.numeric(row[concepts])
        if (n_total < n || any(counts == 0)) return(0)
        prob <- counts[1] / n_total
        for (i in 2:length(counts)) {
          prob <- prob * counts[i] / (n_total - (i - 1))
        }
        prob * factorial(n)
      })
      tibble::tibble(PERSON_ID = as.character(rows[["PERSON_ID"]]), prob_product = expected_probs) %>%
        dplyr::left_join(n_ops_df, by = "PERSON_ID") %>%
        dplyr::mutate(exp = prob_product * n_ops) %>%
        dplyr::summarise(total = sum(exp, na.rm = TRUE)) %>%
        dplyr::pull(total)
    }))

  result <- dplyr::bind_cols(summary, expected_count = expected$expected_count) %>%
    dplyr::filter(expected_count > 0) %>%
    dplyr::mutate(
      overrep_ratio = observed_count / expected_count,
      p_value = stats::pnorm(observed_count, mean = expected_count, sd = sqrt(expected_count), lower.tail = FALSE)
    )
  return(result)
}

#' @keywords internal
running_ngrams <- function(collapsed_long, map_v, global_prev = 0.01) {
  all_phase_results <- list()
  total_patients <- length(unique(collapsed_long$PERSON_ID))
  n_val <- 1

  while (TRUE) {
    n_val <- n_val + 1
    cli::cli_alert_warning(paste0("Running n = ", n_val))

    result <- find_enriched_ngrams(collapsed_long, n = n_val, prev_probs = global_prev)
    if (nrow(result) == 0) break
    if (n_val == 2) global_prev <- mean(result$n_persons) / total_patients

    result_named <- result %>%
      dplyr::mutate(id_list = stringr::str_split(ngram, "-", simplify = FALSE)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(concept_names = list(purrr::map_chr(id_list, ~ map_v$concept_name[match(as.integer(.x), map_v$concept_id)]))) %>%
      dplyr::mutate(name_ngram = paste(unlist(concept_names), collapse = " + ")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        overrep_ratio = observed_count / expected_count,
        binom_pval = stats::pbinom(n_persons - 1, total_patients, prob = global_prev, lower.tail = FALSE),
        person_pass = binom_pval < 0.05 / nrow(result),
        sig_pass = p_value < 0.05 / nrow(result),
        n = n_val
      ) %>%
      dplyr::filter(person_pass, sig_pass)

    if (nrow(result_named) > 0) {
      all_phase_results[[paste0("n", n_val)]] <- result_named
    } else {
      break
    }
  }

  dplyr::bind_rows(all_phase_results) %>% dplyr::distinct()
}

#' @keywords internal
run_clustering <- function(all_phases) {
  all_concepts <- sort(unique(unlist(all_phases$concept_names)))
  all_patients <- sort(unique(unlist(all_phases$person_ids)))

  concept_sparse <- Matrix::sparseMatrix(
    i = rep(seq_along(all_phases$concept_names), lengths(all_phases$concept_names)),
    j = match(unlist(all_phases$concept_names), all_concepts),
    x = 1,
    dims = c(nrow(all_phases), length(all_concepts)),
    dimnames = list(NULL, all_concepts)
  )

  patient_sparse <- Matrix::sparseMatrix(
    i = rep(seq_along(all_phases$person_ids), lengths(all_phases$person_ids)),
    j = match(unlist(all_phases$person_ids), all_patients),
    x = 1,
    dims = c(nrow(all_phases), length(all_patients)),
    dimnames = list(NULL, all_patients)
  )

  binary_concepts <- as.matrix((concept_sparse > 0) + 0)
  binary_patients <- as.matrix((patient_sparse > 0) + 0)

  dist_concepts <- vegan::vegdist(binary_concepts, method = "jaccard", binary = TRUE)
  dist_patients <- vegan::vegdist(binary_patients, method = "jaccard", binary = TRUE)
  dist_time <- stats::dist(scale(all_phases$time))

  norm <- function(x) (x - min(x)) / (max(x) - min(x))
  dist_hybrid <- stats::as.dist(norm(as.matrix(dist_concepts)) + norm(as.matrix(dist_patients)))
  hc <- stats::hclust(dist_hybrid, method = "complete")

  k_to_try <-  max(2, round(sqrt(length(all_concepts)))):round(sqrt(nrow(all_phases)))
  sil_scores <- sapply(
    k_to_try,
    function(k) {
      cl <- stats::cutree(hc, k)
      mean(cluster::silhouette(cl, dist_hybrid)[, 3])
    }
  )

  best_k <- k_to_try[which.max(sil_scores)]
  all_phases$cluster <- as.integer(stats::cutree(hc, k = best_k))
  all_phases
}

