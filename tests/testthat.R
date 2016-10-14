library(testthat)
library(dbfaker)

Sys.setenv("R_TESTS" = "")
test_check("dbfaker")
