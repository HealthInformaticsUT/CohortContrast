library(testthat)

testthat::skip_on_cran()
skip_if_no_integration()

pathToResults <<- getwd()

################################################################################
#
# Database credentials
#
################################################################################

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))
cdm <- CDMConnector::cdmFromCon(con = con, cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")


test_that("Connect", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))
  expect_s4_class(con, "duckdb_connection")
})


test_that("Get connection cdm reference", {
  cdm <- CDMConnector::cdmFromCon(con = con, cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")
  expect_s3_class(cdm, "cdm_reference")
})

test_that("Disconnect", {
  CDMConnector::cdmDisconnect(cdm)
  expect_false(DBI::dbIsValid(con))
})
