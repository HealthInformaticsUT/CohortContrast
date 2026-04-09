#' Get Top Concepts That Best Separate Clusters
#'
#' Returns the top `n` main concepts ranked by standard deviation of concept
#' prevalence across clusters (same ranking idea as the UI "Top N by SD"
#' filter).
#'
#' The function accepts either:
#' - A summary-mode directory (with `metadata.json` and `concept_summaries.parquet`)
#' - A patient-level study directory (with `data_patients.parquet`)
#'
#' For patient-level inputs, clustering summaries are generated on the fly with
#' [precomputeSummary()] when needed.
#'
#' @param studyPath Path to a CohortContrast study folder (summary or patient).
#' @param n Number of top concepts to return.
#' @param k Cluster count as an integer, or `"auto"` to select the best
#'   precomputed `k` from `metadata.json`.
#' @param precomputeIfNeeded Logical; if `TRUE` and clustering summaries are not
#'   available, run [precomputeSummary()] into a temporary directory.
#' @param clusterKValuesWhenAuto Integer vector of `k` values to precompute when
#'   `k = "auto"` and precomputation is needed.
#' @param conceptLimit Maximum concept count passed to [precomputeSummary()]
#'   when precomputation is needed.
#' @param ... Additional arguments forwarded to [precomputeSummary()] when
#'   precomputation is needed.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{id}{Concept ID}
#'   \item{name}{Concept name}
#'   \item{enrichment}{Target/control prevalence ratio}
#'   \item{target_prevalence}{Target cohort prevalence}
#' }
#' The selected `k` is attached as `attr(result, "k")`.
#'
#' @examples
#' if (requireNamespace("nanoparquet", quietly = TRUE)) {
#'   summaryPath <- system.file("example", "st", "lc500s", package = "CohortContrast")
#'
#'   top_auto <- getTopSeparatingConcepts(summaryPath, n = 5, k = "auto")
#'   top_k3 <- getTopSeparatingConcepts(summaryPath, n = 5, k = 3)
#'
#'   head(top_auto)
#'   head(top_k3)
#' }
#'
#' @export
getTopSeparatingConcepts <- function(studyPath,
                                     n = 10,
                                     k = "auto",
                                     precomputeIfNeeded = TRUE,
                                     clusterKValuesWhenAuto = c(2, 3, 4, 5),
                                     conceptLimit = 60,
                                     ...) {
  if (!is.character(studyPath) || length(studyPath) != 1 || !nzchar(studyPath)) {
    stop("`studyPath` must be a non-empty character string.")
  }
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single numeric value >= 1.")
  }
  n <- as.integer(n)

  modeInfo <- checkDataMode(studyPath)
  usePath <- normalizePath(studyPath, mustWork = TRUE)

  requestedK <- .cohortContrastNormalizeKValue(k)
  if (identical(requestedK, "auto")) {
    clusterKValuesWhenAuto <- .cohortContrastNormalizeKVector(clusterKValuesWhenAuto, "clusterKValuesWhenAuto")
  }

  if (modeInfo$mode == "patient") {
    needsPrecompute <- FALSE
    if (!identical(requestedK, "auto")) {
      summaryPath <- file.path(usePath, paste0("clustering_k", requestedK, "_summary.parquet"))
      needsPrecompute <- !file.exists(summaryPath)
    } else {
      hasAnyClustering <- length(list.files(usePath, pattern = "^clustering_k\\d+_summary\\.parquet$")) > 0
      needsPrecompute <- !hasAnyClustering
    }

    if (needsPrecompute) {
      if (!isTRUE(precomputeIfNeeded)) {
        stop(
          "Clustering summaries were not found in patient-level study path. ",
          "Set `precomputeIfNeeded = TRUE` or run `precomputeSummary()` first."
        )
      }

      precomputeKs <- if (identical(requestedK, "auto")) clusterKValuesWhenAuto else requestedK
      tempOut <- file.path(
        tempdir(),
        paste0("cohortcontrast_top_concepts_", basename(usePath), "_", as.integer(Sys.time()))
      )
      precomputeResult <- precomputeSummary(
        studyPath = usePath,
        outputPath = tempOut,
        clusterKValues = precomputeKs,
        conceptLimit = conceptLimit,
        ...
      )
      usePath <- normalizePath(precomputeResult$outputPath, mustWork = TRUE)
      modeInfo <- checkDataMode(usePath)
    }
  }

  if (modeInfo$mode != "summary" && modeInfo$mode != "patient") {
    stop("Unsupported data mode: ", modeInfo$mode)
  }

  metadata <- readJsonFileIfExists(file.path(usePath, "metadata.json"))
  resolvedK <- if (identical(requestedK, "auto")) {
    .cohortContrastResolveBestKFromMetadata(metadata, usePath)
  } else {
    requestedK
  }

  clusteringPath <- file.path(usePath, paste0("clustering_k", resolvedK, "_summary.parquet"))
  if (!file.exists(clusteringPath)) {
    available <- .cohortContrastListAvailableClusterKs(usePath)
    if (length(available) == 0) {
      stop("No `clustering_k*_summary.parquet` files found in: ", usePath)
    }
    stop(
      "Requested k=", resolvedK, " is not available in: ", usePath, ". ",
      "Available k values: ", paste(available, collapse = ", ")
    )
  }

  ranking <- .cohortContrastRankConceptsByClusterSd(clusteringPath)
  if (nrow(ranking) == 0) {
    return(data.frame(
      id = character(0),
      name = character(0),
      enrichment = numeric(0),
      target_prevalence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  metrics <- .cohortContrastLoadConceptMetrics(usePath, clusteringPath)
  merged <- merge(ranking, metrics, by = "id", all.x = TRUE, sort = FALSE)

  merged <- merged[order(-merged$sd_prevalence, merged$id), , drop = FALSE]
  merged <- utils::head(merged, n)

  out <- data.frame(
    id = as.character(merged$id),
    name = as.character(merged$name),
    enrichment = as.numeric(merged$enrichment),
    target_prevalence = as.numeric(merged$target_prevalence),
    stringsAsFactors = FALSE
  )

  attr(out, "k") <- resolvedK
  out
}

#' @keywords internal
.cohortContrastNormalizeKValue <- function(k) {
  if (is.character(k) && length(k) == 1 && tolower(k) == "auto") {
    return("auto")
  }
  if (!is.numeric(k) || length(k) != 1 || is.na(k)) {
    stop("`k` must be a single integer >= 2 or \"auto\".")
  }
  kInt <- as.integer(k)
  if (is.na(kInt) || kInt < 2) {
    stop("`k` must be a single integer >= 2 or \"auto\".")
  }
  kInt
}

#' @keywords internal
.cohortContrastNormalizeKVector <- function(x, argName) {
  if (!is.numeric(x) || length(x) < 1 || any(is.na(x))) {
    stop("`", argName, "` must be a numeric vector of integers >= 2.")
  }
  out <- unique(as.integer(x))
  out <- out[!is.na(out) & out >= 2]
  if (length(out) == 0) {
    stop("`", argName, "` must contain at least one integer >= 2.")
  }
  sort(out)
}

#' @keywords internal
.cohortContrastResolveBestKFromMetadata <- function(metadata, studyPath) {
  if (!is.list(metadata)) {
    stop(
      "`k = \"auto\"` requires metadata.json with clustering information. ",
      "No valid metadata found in: ", studyPath
    )
  }

  bestFromDemographics <- metadata$demographics$distributions$clusters_best_k
  if (!is.null(bestFromDemographics) && is.numeric(bestFromDemographics) && length(bestFromDemographics) > 0) {
    bestK <- suppressWarnings(as.integer(bestFromDemographics[[1]]))
    if (!is.na(bestK) && bestK >= 2) {
      return(bestK)
    }
  }

  clustering <- metadata$clustering
  if (is.list(clustering) && length(clustering) > 0) {
    kNames <- names(clustering)
    if (!is.null(kNames) && length(kNames) > 0) {
      kInts <- suppressWarnings(as.integer(kNames))
      sil <- vapply(kNames, function(kName) {
        val <- clustering[[kName]]$silhouette_score
        suppressWarnings(as.numeric(val))
      }, numeric(1))

      valid <- !is.na(kInts) & !is.na(sil)
      if (any(valid)) {
        kValid <- kInts[valid]
        silValid <- sil[valid]
        return(kValid[which.max(silValid)])
      }
    }
  }

  stop(
    "`k = \"auto\"` could not determine a best k from metadata.json in: ",
    studyPath
  )
}

#' @keywords internal
.cohortContrastListAvailableClusterKs <- function(studyPath) {
  files <- list.files(studyPath, pattern = "^clustering_k\\d+_summary\\.parquet$")
  if (length(files) == 0) {
    return(integer(0))
  }
  sort(unique(as.integer(gsub("^clustering_k(\\d+)_summary\\.parquet$", "\\1", files))))
}

#' @keywords internal
.cohortContrastReadParquetDf <- function(path) {
  as.data.frame(nanoparquet::read_parquet(path), stringsAsFactors = FALSE)
}

#' @keywords internal
.cohortContrastAsConceptId <- function(x) {
  out <- as.character(x)
  out <- gsub("\\.0$", "", out)
  out
}

#' @keywords internal
.cohortContrastRankConceptsByClusterSd <- function(clusteringPath) {
  df <- .cohortContrastReadParquetDf(clusteringPath)

  conceptCol <- if ("CONCEPT_ID" %in% colnames(df)) "CONCEPT_ID" else if ("concept_id" %in% colnames(df)) "concept_id" else NULL
  prevalenceCol <- if ("prevalence" %in% colnames(df)) "prevalence" else if ("PREVALENCE" %in% colnames(df)) "PREVALENCE" else NULL
  clusterCol <- if ("cluster" %in% colnames(df)) "cluster" else if ("CLUSTER" %in% colnames(df)) "CLUSTER" else NULL

  if (is.null(conceptCol) || is.null(prevalenceCol) || is.null(clusterCol)) {
    stop("Clustering summary is missing required columns in: ", clusteringPath)
  }

  keep <- rep(TRUE, nrow(df))
  if ("IS_ORDINAL" %in% colnames(df)) {
    keep <- keep & !as.logical(df$IS_ORDINAL)
  } else if ("ORDINAL" %in% colnames(df)) {
    keep <- keep & (is.na(df$ORDINAL) | suppressWarnings(as.integer(df$ORDINAL) == 0))
  }
  df <- df[keep, , drop = FALSE]

  if (nrow(df) == 0) {
    return(data.frame(id = character(0), sd_prevalence = numeric(0), stringsAsFactors = FALSE))
  }

  df$id <- .cohortContrastAsConceptId(df[[conceptCol]])
  df$cluster_norm <- as.character(df[[clusterCol]])
  df$prevalence_num <- suppressWarnings(as.numeric(df[[prevalenceCol]]))
  df <- df[!is.na(df$prevalence_num), c("id", "cluster_norm", "prevalence_num"), drop = FALSE]

  if (nrow(df) == 0) {
    return(data.frame(id = character(0), sd_prevalence = numeric(0), stringsAsFactors = FALSE))
  }

  agg <- stats::aggregate(prevalence_num ~ id + cluster_norm, data = df, FUN = function(x) mean(x, na.rm = TRUE))
  sdDf <- stats::aggregate(prevalence_num ~ id, data = agg, FUN = function(x) {
    if (length(x) <= 1) {
      return(0)
    }
    stats::sd(x, na.rm = TRUE)
  })
  colnames(sdDf)[colnames(sdDf) == "prevalence_num"] <- "sd_prevalence"
  sdDf[order(-sdDf$sd_prevalence, sdDf$id), c("id", "sd_prevalence"), drop = FALSE]
}

#' @keywords internal
.cohortContrastFirstNonEmpty <- function(x) {
  vals <- as.character(x)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) {
    return(NA_character_)
  }
  vals[[1]]
}

#' @keywords internal
.cohortContrastLoadConceptMetrics <- function(studyPath, clusteringPath) {
  conceptSource <- NULL
  if (file.exists(file.path(studyPath, "concept_summaries.parquet"))) {
    conceptSource <- .cohortContrastReadParquetDf(file.path(studyPath, "concept_summaries.parquet"))
  } else if (file.exists(file.path(studyPath, "data_features.parquet"))) {
    conceptSource <- .cohortContrastReadParquetDf(file.path(studyPath, "data_features.parquet"))
  } else {
    conceptSource <- data.frame()
  }

  clusteringDf <- .cohortContrastReadParquetDf(clusteringPath)
  conceptColCluster <- if ("CONCEPT_ID" %in% colnames(clusteringDf)) "CONCEPT_ID" else if ("concept_id" %in% colnames(clusteringDf)) "concept_id" else NULL
  nameColCluster <- if ("CONCEPT_NAME" %in% colnames(clusteringDf)) "CONCEPT_NAME" else if ("concept_name" %in% colnames(clusteringDf)) "concept_name" else NULL

  fallbackNames <- data.frame(id = character(0), fallback_name = character(0), stringsAsFactors = FALSE)
  if (!is.null(conceptColCluster) && !is.null(nameColCluster) && nrow(clusteringDf) > 0) {
    fallbackNames <- stats::aggregate(
      clusteringDf[[nameColCluster]],
      by = list(id = .cohortContrastAsConceptId(clusteringDf[[conceptColCluster]])),
      FUN = .cohortContrastFirstNonEmpty
    )
    colnames(fallbackNames)[2] <- "fallback_name"
  }

  if (!is.data.frame(conceptSource) || nrow(conceptSource) == 0 || !("CONCEPT_ID" %in% colnames(conceptSource))) {
    out <- fallbackNames
    if (nrow(out) == 0) {
      return(data.frame(
        id = character(0),
        name = character(0),
        enrichment = numeric(0),
        target_prevalence = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    out$name <- out$fallback_name
    out$enrichment <- NA_real_
    out$target_prevalence <- NA_real_
    return(out[, c("id", "name", "enrichment", "target_prevalence"), drop = FALSE])
  }

  metricDf <- conceptSource
  metricDf$id <- .cohortContrastAsConceptId(metricDf$CONCEPT_ID)
  metricDf$name <- if ("CONCEPT_NAME" %in% colnames(metricDf)) as.character(metricDf$CONCEPT_NAME) else NA_character_
  metricDf$enrichment <- if ("PREVALENCE_DIFFERENCE_RATIO" %in% colnames(metricDf)) suppressWarnings(as.numeric(metricDf$PREVALENCE_DIFFERENCE_RATIO)) else NA_real_
  metricDf$target_prevalence <- if ("TARGET_SUBJECT_PREVALENCE" %in% colnames(metricDf)) suppressWarnings(as.numeric(metricDf$TARGET_SUBJECT_PREVALENCE)) else NA_real_

  out <- stats::aggregate(
    cbind(enrichment, target_prevalence) ~ id,
    data = metricDf[, c("id", "enrichment", "target_prevalence"), drop = FALSE],
    FUN = function(x) {
      vals <- suppressWarnings(as.numeric(x))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        return(NA_real_)
      }
      max(vals)
    }
  )

  namesDf <- stats::aggregate(metricDf$name, by = list(id = metricDf$id), FUN = .cohortContrastFirstNonEmpty)
  colnames(namesDf)[2] <- "name"
  out <- merge(out, namesDf, by = "id", all.x = TRUE, sort = FALSE)
  out <- merge(out, fallbackNames, by = "id", all.x = TRUE, sort = FALSE)
  out$name <- ifelse(is.na(out$name) | !nzchar(out$name), out$fallback_name, out$name)
  out$fallback_name <- NULL

  out[, c("id", "name", "enrichment", "target_prevalence"), drop = FALSE]
}
