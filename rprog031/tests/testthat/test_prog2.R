library(rprog031) #Change this if you're using submodule
#REM this file exists as a hard link between submodule and package test directory
                                        # rprog031/tests/testthat/test_prog2.R
                                        # rprog031/ProgrammingAssignment2/test_cachematrix.R
context("caching an inverse of a matrix")

test_that("makeCacheMatrix with no arguments returns a list", {
    expect_equal(class(makeCacheMatrix()), "list")
    

})
