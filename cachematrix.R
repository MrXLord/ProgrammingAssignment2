# the <<- operator which can be used to assign a value to an object in an environment
# that is different from the current environment. Below are two functions that are used to create a 
#special object that stores a numeric vector and cache's its mean.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
## Returning a matrix i.e the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
