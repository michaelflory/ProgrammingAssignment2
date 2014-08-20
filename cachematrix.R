##** Rename file to cachematrix.R

## Overall, this file contains 2 functions: makeCacheMatrix and cacheSolve. 
## Together, these functions will calculate the inverse of a matrix. They use the
## cache to save memory and computation time by calculating the inverse once, 
## saving to cache, and only re-calculating in the future if the matrix has changed.


# The makeCacheMatrix function creates a special "matrix"  that can cache its
# inverse. It will set the value of the matrix, get the value of the matrix,
# set the value of the inverse, and get the value of the inverse.
# 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


# The function cacheSolve calculates the inverse of the special "matrix" created 
# with makeCacheMatrix. cacheSolve first checks to see if the inverse has 
# been calculated previously. If so, it gets the inverse from the cache. 
# Otherwise, it calculates the inverse and caches it via the setinverse function.

cacheSolve <- function(x, ...) {
          m <- x$getinverse()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
     

}
