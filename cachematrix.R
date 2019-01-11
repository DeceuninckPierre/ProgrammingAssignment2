## ------------------------------------------------------------ ##
##                 Programming Assignment 2                     ##
##                    by  P.Deceuninck                          ##
## ------------------------------------------------------------ ##

## Below are two functions that are used to create a special object 
##      that stores an invertible matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
##      set             set the value of the invertible matrix
##      get             get the value of the invertible matrix
##      setinverse      set the value of the inverse of the matrix
##      getinverse      get the value of the inverse of the matrix
##
## Arguments:
## x    invertible matrix, default value is matrix()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve retrieves the inverse from the cache.
##
## Arguments:
##      x       an invertible matrix
##      ...     further arguments passed to or from other methods

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
