## These two functions save a square, nonsingular, matrix together with 
## its inverse, so that the inverse needs to be calculated only once.

## This function takes a matrix and creates a list of four functions 
## that allow manipulating the matrix itself and its inverse.
## The input matrix is supposed to be square and nonsingular.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function takes as an argument a set created by the makeCacheMatrix
## function, returns the inverse of the associated matrix by retrieving it
## from the cache if it is already there, or by calculating (and caching) it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    d <- x$get()
    inv <- solve(d, ...)
    x$setinv(inv)
    inv
}

