## A pair of functions that cache the inverse of a matrix.
## makeCacheMatrix(): 
## creates a special matrix object that can cache its inverse.
## cacheSolve(): 
## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setSolve <- function(inverse) sol <<- inverse
        getSolve <- function() sol
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed) ,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getSolve()
        if (!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setSolve(sol)
        sol
}
