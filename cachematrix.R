## This script allows you to cache the inverse of a matrix, so that you don't need to
## perform time consuming calculation of a matrix's inverse if it has already been
## solved before.

## makeCacheMatrix caches the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y) {
                x <<- y
                i <<- NULL
        }
        get  <- function() x
        setInverse <- function(inverse) {
                i  <<- inverse
        }
        getInverse  <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve takes the object created by makeCacheMatrix as an argument. It returns
## the matrix's inverse by retrieving the cached inverse, or by calculating the inverse
## if the cache is empty.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setInverse(i)
        i
}
