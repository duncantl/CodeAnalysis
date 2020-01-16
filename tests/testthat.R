library(testthat)
# https://github.com/r-lib/testthat/issues/86
Sys.setenv("R_TESTS" = "")

library(CodeAnalysis)

if(FALSE) {
    test_check("CodeAnalysis")
} else
    message("skipping the testhat check")

