## Caching the Inverse of a Matrix.
##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that is not
## discuss here).
##
## The following pair of functions cache the inverse of a matrix.

## Usage:
##   > m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
##   > matrixAndInverse <- makeCacheMatrix(m)
##
##   > matrixAndInverse$getMatrix()
##        [,1] [,2]
##   [1,]    1    2
##   [2,]    3    4
##
##   First execution, no data in cache:
##   > cacheSolve(matrixAndInverse)
##        [,1] [,2]
##   [1,] -2.0  1.0
##   [2,]  1.5 -0.5
##
##   Second execution, retrieves matrix inverse from cache:
##   > cacheSolve(matrixAndInverse)
##   getting matrix inverse from cache
##        [,1] [,2]
##   [1,] -2.0  1.0
##   [2,]  1.5 -0.5

## Functions

## makeCacheMatrix:
##  This function creates a special "matrix" object that can cache its inverse.
##  It creates a special "vector", which is really a list containing a
##  function to:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the matrix inverse
##     4. get the value of the matrix inverse
##
##  args:
##    x: A square matrix (optional argument)
##
##  returns:
##    A matrix with getter/setter functions to handle the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # cached matrix inverse
    cachedMatrixInverse <- NULL

    # 'setter' for matrix
    setMatrix <- function(aMatrix) {
      x <<- aMatrix
      cachedMatrixInverse <<- NULL
    }

    # 'getter' for matrix
    getMatrix <- function() {
      x
    }

    # 'setter' for inverted matrix
    setMatrixInverse <- function(matrixInverse) {
      cachedMatrixInverse <<- matrixInverse
    }

    # 'getter' for inverted matrix
    getMatrixInverse <- function() {
      cachedMatrixInverse
    }

    # returns the list of functions for matrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## cacheSolve:
##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and
##  the matrix has not changed),then 'cacheSolve' retrieves the inverse from
##  the cache.
##
##  args:
##    x: A square matrix
##
##  returns:
##    The matrix inverse
cacheSolve <- function(x, ...) {
  # return a matrix inverse matrix from the cache if it has already been
  # computed
  matrixInverse <- x$getMatrixInverse()

  if (!is.null(matrixInverse)) {
    # display a message if the matrix inverse is in cache
    message("getting matrix inverse from cache")
    return(matrixInverse)
  }

  # if not in cache then compute matrix inverse
  matrix <- x$getMatrix()
  matrixInverse <- solve(matrix, ...)

  # set matrix inverse to cache
  x$setMatrixInverse(matrixInverse)

  # return matrix inverse
  return(matrixInverse)
}
