## This should exist in two locations (hard link)
## ./R/Prog2.R <-- for testing
## ./ProgrammingAddisngment2/cachematrix.R <-- submodule on Github

## Functions to create a matrix object that can compute its inverse.


#' Create a matrix object: list cointaining object and functions to
#'     set the object, get the object, set the inverse of the object
#'     and get the inverse of the object
#' @param x object that is the subject of the list
#' @return list including object x and useful functions
#' @examples
#' makeCacheMatrix() empty object with no subject
#' $get() will show the subject
#' $set(matrix(c(1,2,3,4), ncol=2) will place 2x2 matrix as
#'     subject of object
#' $getinverse() will display the cached inverse
#' $setinverse() will set the inverse cache 

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) x_inverse <<- solve
    getinverse <- function() x_inverse
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}


#' if inverse has been previously computed for object, will return it,
#' otherwise will compute inverse, store it, and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$getinverse()
    if(!is.null(x_inverse)){
        message("using cached data")
        return(x_inverse)
    }
    my_matrix <- x$get()
    x_inverse <- solve(my_matrix, ...)
    x$setinverse(x_inverse)
    x_inverse
    }
