library(testthat)

pathToResults <<- getwd()

################################################################################
#
# Database credentials
#
################################################################################

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")


test_that("Connect", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  expect_s4_class(con, "duckdb_connection")
})


test_that("Get connection cdm reference", {
  cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")
  expect_s3_class(cdm, "cdm_reference")
})

test_that("Disconnect", {
  CDMConnector::cdmDisconnect(cdm)
  expect_false(DBI::dbIsValid(con))
})
