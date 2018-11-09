## Creates a special matrix that can cache it's inverse
## Returns the list of available methods
makeCacheMatrix <- function(value = matrix()) {
  inverse <- NULL
  
  set <- function(newValue) {
    value <<- newValue
    inverse <<- NULL
  }
  
  get <- function() {
    value
  }
  
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## Calculates the inverse of our special matrix.
## If the inverse was calculated before it returns the cached value instead of
## solving the matrix again.
cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrixValue <- m$get()
  inverse <- solve(matrixValue, ...)
  m$setInverse(inverse)
  inverse
}
