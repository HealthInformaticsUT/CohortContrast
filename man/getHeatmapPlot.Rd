% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotHeatmap.R
\name{getHeatmapPlot}
\alias{getHeatmapPlot}
\title{Generate a Heatmap Plot}
\usage{
getHeatmapPlot(ccObject)
}
\arguments{
\item{ccObject}{An object returned by the CohortContrast GUI, typically created
when the "Create Visual Snapshot" button is pressed. It should contain
target_row_annotation, target_col_annotation, and target_matrix components.}
}
\value{
A plot object generated by `createHeatmapPlot`.
}
\description{
This function generates a heatmap using the CohortContrast GUI object.
It extracts necessary components from the ccObject and passes them to the
`createHeatmapPlot` function to generate the plot.
}
\examples{
\dontrun{
# Assuming `ccObject` (visual snapshot) is already created via CohortContrast GUI
ccObject <- readRDS("./visual_snapshots/CohortContrastDataVisualSnapshot.rds")
getHeatmapPlot(ccObject)
}
}
