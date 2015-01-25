## cachematrix.R
## Functions to cache the inverse of a matrix. Since matrix inversion is usually
## a costly computations, there might be benefits to caching the results of inversion 
## rather than compute it repeatedly. 
## 
## This file contains functions that provide a cache layer on top a regular 
## matrix and a function to make this additional layer transparent.

## Constructs a matrix wrapper that maintains a cached result of inverting the matrix.
## 'x' The wrapped matrix whose inversion result will be cached
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    ## Sets the underlying matrix, destroys any current inversion result.
    set <- function(newX) {
    	x <<- newX
    	inverse <<- NULL
    }

    ## Returns the underlying (raw) matrix.
    get <- function() x

    ## Sets the inverse cached by this CacheMatrix.
    setinverse <- function(matrixInverse) {
    	inverse <<- matrixInverse
    }

    ## Returns the currently cached inverse result.
    getinverse <- function() inverse

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of a matrix, preventing multiple calculations
## of the inverse if a previous result has already been computed
## 'x' The matrix whose inverse needs to be computed
## '...' Additional parameters for the inverse() library function
cacheSolve <- function(x, ...) {
        inverse = x$getinverse()

        if (!is.null(inverse)) {
        	return (inverse)
        }

        originalMatrix <- x$get()
        inverse <- solve(originalMatrix, ...)
        x$setinverse(inverse)

        ## Return a matrix that is the inverse of 'x'
        inverse
}
