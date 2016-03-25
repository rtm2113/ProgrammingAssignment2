## Put comments here that give an overall description of what your
## functions do
## Functions below save processing time by cacheing the inverse of a matrix
## for future calculations. 

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv.mat <- NULL
      set <- function(y) {
            x <<- y
            inv.mat <<- NULL
      }
      get <- function() x
      setinverse <- function(solve)x <<- solve
      getinverse <- function() inv.mat
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse of the special matrix calculated above.
## Before calculating the inverse, however, it checks to see if the inverse has
## already been cached. If already cached, it retrieves that value rather than
## calculating it again.

cacheSolve <- function(x, ...) {
      
      inv.mat <- x$getinverse()
      if(!is.null(inv.mat)) {
            message("getting cached data")
            return(inv.mat)
      }
      message("no inverse cached, calculating")
      mat <- x$get()
      inv.mat <- solve(mat, ...)
      x$setinverse(inv.mat)
      inv.mat
}
