library(rprog031)


context("Outcomes frame created from data")
outcome <- load_outcomes(outcome="pneumonia")
test_that("loads_outcomes loads proper file and creates data frame" , {

    expect_equal(class(outcome), "data.frame")
})


    
context("Finding the best hospital in the state")

test_that("Best function exists and takes the proper arguments", {
    ## expect_error(best())
    ## expect_error(best(state = "AK"))
    ## expect_error(best(outcome = "pneumonia"))
    ## expect_error(best(state = "AK", outcome = "herpies"), "invalid outcome", fixed=TRUE)
    ## expect_error(best(state = "XX", outcome = "heart attack"), "invalid state", fixed=TRUE)
    ## expect_equal(best("TX", "heart attack"), "CYPRESS FAIRBANKS MEDICAL CENTER")
    ## expect_equal(best("TX", "heart failure"),  "FORT DUNCAN MEDICAL CENTER")
    ## expect_equal(best("MD", "heart attack"),  "JOHNS HOPKINS HOSPITAL, THE")
    ## expect_equal(best("MD", "pneumonia"), "GREATER BALTIMORE MEDICAL CENTER")
})



context("Return name of hospital in given state with given rank")

test_that("rankhospital takes three arguments", {
    expect_error(rankhospital())
    
    expect_equal(rankhospital("TX", "heart failure", 4), "DETAR HOSPITAL NAVARRO")
    expect_equal(rankhospital("MD", "heart attack", "worst"),  "HARFORD MEMORIAL HOSPITAL")
    myfuncton <- function() {NA}
    #expect_equal(myfuncton(), NA )
})

context("Ranking each hospital in the state")

test_that("rankall is a stupid function but it works", {
    

})
    
