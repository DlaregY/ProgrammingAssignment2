## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It is really a list that contains a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     invMat <- NULL
     set <- function(y) {
          x <<- y
          invMat <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) invMat <<- inverse
     getinverse <- function() invMat
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
     invMat <- x$getinverse()
     if(!is.null(invMat)) {
          message("getting cached data")
          return(invMat)
     }
     data <- x$get()
     invMat <- solve(data, ...)
     x$setinverse(invMat)
     invMat
}
