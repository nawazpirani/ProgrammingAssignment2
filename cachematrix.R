## makeCacheMatrix.R will cache the inverse of a matrix
## This script contains wo function. The first function creates a special
## matrix object that can cache its inverse.
## The second function will calculate the inverse of the stored matrix.


## The function below creates a matrix object that cn cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below calcultes that inverse of the matrix from above.
## If the inverse has already been calculated, then it will retrieve
## the values from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
