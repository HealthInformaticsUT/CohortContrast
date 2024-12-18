## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = TRUE, eval=FALSE, echo=TRUE------------------------------------
# 
# ################################################################################
# #
# # Execute
# #
# ################################################################################
# 
# data = CohortContrast::CohortContrast(
#   cdm,
#   targetTable = targetTable,
#   controlTable = controlTable,
#   pathToResults = getwd(),
#   domainsIncluded = c(
#     "Drug",
#     "Condition",
#     "Measurement",
#     "Observation",
#     "Procedure",
#     "Visit",
#     "Visit detail"
#   ),
#   prevalenceCutOff = 2.5,
#   topK = FALSE, # Number of features to export
#   presenceFilter = 0.2, # 0-1, percentage of people who must have the chosen feature present
#   complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
#   getSourceData = FALSE, # If true will generate summaries with source data as well
#   runZTests = TRUE,
#   runLogitTests = FALSE,
#   createOutputFiles = TRUE,
#   safeRun = FALSE,
#   complName = "CohortContrastStudy")
# 

