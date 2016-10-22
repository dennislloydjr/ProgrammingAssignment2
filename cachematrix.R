## Invert a matrix and cache it's value. If the matrix has already been inverted and 
## has not changed, the inverse is not recalculated and the cache is used instead.

## Provides a cache-enabling list of functions for inversing the given matrix_to_cache
makeCacheMatrix <- function(matrix_to_cache = matrix()) {
  cached_inverse <- NULL
  set <- function(matrix) {
    matrix_to_cache <<- matrix
    cached_inverse <<- NULL
  }
  get <- function() {
    matrix_to_cache
  }
  setInverse <- function(inverse) {
    cached_inverse <<- inverse
  }
  getInverse <- function() {
    cached_inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Inverts the given matrix, x. If x has already been encountered and is unchanged, 
## a cached inverse is returned instead of re-calculating inverse.
cacheSolve <- function(x, ...) {

  if(is.null(x$getInverse())) {
    # the inverse is not cached, compute and cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  } else {
    message("getting cached data")
  }
  
  x$getInverse()
}
