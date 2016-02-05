## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. It first checks if the inverse has already been 
## computed. If the inverse has already been calculated (and the matrix 
## has not changed), then it should retrieve the inverse from the cache.
## If not, it computes the inverse, sets the value in the cache via
## setinverse function.

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

## Test my functions
##> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##> my_matrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> my_matrix$getInverse()
##NULL
##> cacheSolve(my_matrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(my_matrix)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> my_matrix$getInverse()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5