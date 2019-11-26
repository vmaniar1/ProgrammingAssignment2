## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
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

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Look into the following for functions that are helped create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Checking my functions 

source("ProgrammingAssignment2.R")
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
