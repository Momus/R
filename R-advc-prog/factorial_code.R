## Peer-graded Assignment: Functional and Object-Oriented Programming
## Part 1: Factorial Function

########
## Please note: calls to library() are made next to the functions that
## require that specific library to illustrate what needs what.
#######

## Write a function that computes the factorial of an integer greater
## than or equal to 0.

## Factorial of a number n is n * (n-1) * (n - 2) * … * 1.  Factorial
## of 0 is defined to be 1.

## Write four different versions of the Factorial function:

##################
## Factorial_loop: compute the factorial of an integer
## using looping.
Factorial_loop <-  function(x) {
    stopifnot(x > 0)
    if (x == 0) return(1)
    factorial <- 1
    for (i in x:1) {
        factorial <- factorial * x
        x <- x - 1
    }
    factorial
}


####################
## Factorial_reduce: compute factorial using reduce() purrr, or
## Reduce() base.

library(purrr)

Factorial_reduce <- function(x) {
    stopifnot(x > 0)
    if (x == 0) return(1)
    seq(1 , x, 1.0) %>%
        reduce(`*`)
}


#################
## Factorial_func: use recursion to compute the factorial.


Factorial_func <- function(x){
    stopifnot(x > 0)
    if (x == 0) 1
    else if (x == 1) 1
    else x * Factorial_func(x - 1)
}


################
## Factorial_mem: use memoization to compute the factorial.

## My computer can only calculate factorials up to 170, so that's the
## size of the table.
fact_tbl <- c(1, 2, rep(NA, 168)) 

Factorial_mem <- function(x){
    stopifnot(x > 0)
    if (!is.na(fact_tbl[x])) fact_tbl[x]
    else {
        fact_tbl[x - 1] <<- Factorial_mem(x - 1)
        fact_tbl[x] <<- x * fact_tbl[x - 1]
        return(fact_tbl[x])
    }
}


#########The two functions below are extra: not part of assignment.
#
## There are two types of recursion: Regular and Extra Crispy. And by
## Extra-Crispy, I mean tail-call.
Factorial_tail <- function(x, fact = 1){
    stopifnot(x > 0)
    if (x == 0) return(fact)
    next_fact <- x * fact
    next_x <- x - 1
    Factorial_func(next_x, next_fact)
}

## Intermediate steps aren't calculated (factorial is like a
## reduce function), so just fill up the table with each call.
Factorial_memtail <- function(x, fact = 1, original_x = -1){
    stopifnot(x > 0)
    ## By definition:
    if (x == 0) 1
    ## If this is the first time the function has been called:
    if (original_x == -1) {
        ## Value is already in the table? No need to stick around.
        if (!is.na(fact_tbl[x])) fact_tbl[x]
        ## No luck? Preserve the param function was originally called
        ## with.
        original_x <- x}
    if (x == 1) {
        fact_tbl[original_x] <<- fact 
        return(fact)
    } else {
        next_fact <- x * fact
        next_x <- x - 1
        Factorial_memtail(next_x, next_fact, original_x)
    }
}
#
#########The two functions above are extra: not part of assignment.
###################################################################

## use the microbenchmark package to time the operation of these
## functions and provide a summary of their performance.
library(microbenchmark)
fact_10 <- microbenchmark(Factorial_loop(10),
                         Factorial_reduce(10),
                         Factorial_func(10),
                         Factorial_mem(10),
                         times = 1000L)

fact_100 <- microbenchmark(Factorial_loop(100),
                         Factorial_reduce(100),
                         Factorial_func(100),
                         Factorial_mem(100),
                         times = 1000L)

fact_max <- microbenchmark(Factorial_loop(170),
                         Factorial_reduce(170),
                         Factorial_func(170),
                         Factorial_mem(170),
                         times = 1000L)


##In addition to timing your functions for specific inputs, make sure
##to show a range of inputs in order to demonstrate the timing of each
##function for larger inputs.
library(purrr)
library(dplyr)
f10 <- print(fact_10) %>% select(expr, min, mean, median, max)

sink("./factorial_output.txt")
cat("
fmax
sink()

## writeLines(print(fact_10),
##             file = "./factorial_output.txt",
##             append = FALSE)

## write.table(fact_100,
##             file = "./factorial_output.txt",
##             append = TRUE)

## write.table(fact_max,
##             file = "./factorial_output.txt",
##             append = TRUE)






