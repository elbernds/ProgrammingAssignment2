## These functions allows the user to create a special matrix to
## cache and compute the inverse of the matrix.

## Creates a special matrix object that can cache its inverse
## by returning a list of getter and setter functions of the matrix
## and its calculated inverse.

makeCacheMatrix <- function(matrixVar = matrix()) {
        # Initialize matrix inverse to NULL
        matrixInverse <- NULL
        # Function to set the matrix
        set <- function(y) {
            # Check if we have a NULL parameter
            if (is.null(y)) {
                message("please set a non-null matrix")
                return()
            }
            # Check if we have the same matrix
            # If we don't have, clear the cache
            if (!identical(matrixVar, y)) {
                message("setting new matrix")
                matrixVar <<- y
                matrixInverse <<- NULL
            }
        }
        # Function to get the matrix
        get <- function() matrixVar
        # Function to set the matrix inverse
        setMatrixInverse <- function(inverse) matrixInverse <<- inverse
        # Function to get the matrix inverse
        getMatrixInverse <- function() matrixInverse
        # Return the functions as a list
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## Compute/calculate the inverse of the matrix and return it
## Return the inverse of the matrix from cache if it was already
## calculated or if the same matrix was set.

cacheSolve <- function(x, ...) {
        # Check if parameter is NULL
        if (is.null(x)) {
            message("cache matrix not created")
            return(NULL)
        }
        # If we have an existing matrix inverse cached value
        # and the matrix has not changed, return the cached
        # matrix inverse value
        cachedMatrixInverse <- x$getMatrixInverse()
        if(!is.null(cachedMatrixInverse)) {
            message("getting cached data")
            return(cachedMatrixInverse)
        }
        # Get the matrix and compute the inverse
        data <- x$get()
        if (!is.null(data)) {
            newMatrixInverse <- solve(data, ...)
            x$setMatrixInverse(newMatrixInverse)
            newMatrixInverse   
        } else {
            message("please set a non-null matrix")
        }
}
