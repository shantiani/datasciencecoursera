## The function makeCacheMatrix returns a list of functions that either set or 
## retrieve the value of the matrix and its inverse. 
## The function cacheSolve calls functions from makeCacheMatrix to get the 
## stored value of inverse (getinverse) from the cache.  If it does not already 
## exist in memory, it calls get to retrieve the current matrix, calculates the 
## inverse and saves that value to the cache (with setinverse)

## This function returns a list of functions to set or get the value of a 
## matrix and its inverse.
## If the inverse has not yet been calculated or if the matrix has changed, 
## the inverse is set to null.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(newinv) inv <<- newinv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function tests to see if there is a value for the inverse matrix in the 
## cache.  If so, that value is returned.  If not, it gets the newest version of 
## matrix and solves for the new inverse.  This new inverse is stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

