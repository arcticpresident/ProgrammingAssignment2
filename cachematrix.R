## This file contains functions that are used to compute the inverse
## of a matrix and cache it's value. If a matrix has it's inverse
## cached, the value is fetched from there instead of computing it
## again.

## makeCacheMatrix creates a special "matrix" which is actually a list
## containing functions to support basic operations on a matrix
## set - set the value of the matrix
## get - get the value of the matrix
## setInverse - set the inverse of the matrix
## getInverse - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special matrix created
## with makeCacheMatrix. It checks to see if the inverse has already
## been calculated. If so, it get's it from the cache and skips
## computation. Otherwise it calculates the cache of the matrix and
## sets the value in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
