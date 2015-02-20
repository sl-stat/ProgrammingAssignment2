## This is my Assignment 2 for a Coursera class - R Programming.
## The functions below are used together to cache the inverse of a matrix.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                      # set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                       # get the value of the matrix
    setsolve <- function(solve) m <<- solve   # set the value of the inverse. Function "solve" computes the inverse of a square matrix 
    getsolve <- function() m                  # get the value of the inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")       # if inverse is not null, return the inverse
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)                    # otherwise, compute the inverse
    x$setsolve(m)
    m                                        # Return a matrix that is the inverse of 'x'
}
