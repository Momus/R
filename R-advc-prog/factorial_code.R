## Write a function that computes the factorial of an integer greater
## than or equal to 0.

## Factorial of a number n is n * (n-1) * (n - 2) * … * 1.  Factorial
## of 0 is defined to be 1.

## Write four different versions of the Factorial function:

##################
## Factorial_loop: compute the factorial of an integer
## using looping.
Factorial_loop <-  function(x) {
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



#################
## Factorial_func: use recursion to compute the factorial.



################
## Factorial_mem: use memoization to compute the factorial.

