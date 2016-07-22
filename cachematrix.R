## Script : cacheMatrix.R
## Description : This script deals with the creation and manipulation of a special matrix which caches its inverse value

## Creates a list object with getter/setter functions for a matrix object
makeCacheMatrix <- function(x = matrix()) {
    inverseVal <- NULL

    set <- function(inputMatrix) {
        x <<- inputMatrix
        inverseVal <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inputInverse) {
        inverseVal <<- inputInverse
    }

    getInverse <- function() {
        inverseVal
    }

    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function detects if the inverse for the cached Matrix is already computed.
## If it is, it returns the stored inverse value for the given cached Matrix object
## If not, then it computes the inverse for the given cached matrix (using "solve") and stores it within the cached matrix object

cacheSolve <- function(x, ...) {
    inverseVal <- x$getInverse()
    if(!is.null(inverseVal)) {
        message("getting cached data")
    }
    else{
        inverseVal <- solve(x$get(), ...)
        x$setInverse(inverseVal)
    }
    inverseVal
}
