#writing a pair of functions that cache the inverse of a matrix;

#First step:
#makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solving) inverse <<- solving
    getinverse <- function() inverse
    list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}

#Second Step:
#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
