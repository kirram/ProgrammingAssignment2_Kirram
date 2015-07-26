
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    A <- NULL
    set <- function(y) {
        x <<- y
        A <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) A <<- inverse
    getinverse <- function() A
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    A <- x$getinverse()
    if(!is.null(A)) {
      message("cache value=")
        return(A)
    }
    B <- x$get()
    A <- solve(B)
    x$setinverse(A)
    A
}
