## makeCacheMatrix and cacheSolve provide extension to the matrix
## object to improve performance of repeated inverse calculations
## through result caching.

## Wrapper function to provide caching functionality 
## for matrix inverse calculation

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize inverse to null, as it has not been calculated
      inverse <- NULL
      
      ## Property set; if x is changed, NULL out inverse, as inverse 
      ## of the new matrix may not be the same
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      ## Return the original, non-inversed matrix
      getOriginal <- function() x
      ## Set the value of the inverse
      setInverse <- function(workedInverse) inverse <<- workedInverse
      ## Return the inverse matrix
      getInverse <- function() inverse
      
      ## return the CacheMatrix object
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse())
}


## Returns the inverse of a matrix.  
## If the inverse is cached, the cached version is returned.  
## Otherwise, the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
      
      ## Check the cached version, and return if found
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      ## Cached version was not found, so get the matrix and solve
      data <- x$getOriginal()
      inverse <- solve(data, ...)
      ## Cache the result and return it
      x$setInverse(inverse)
      inverse
}
