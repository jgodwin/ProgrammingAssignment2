## Put comments here that give an overall description of what your
## functions do
# Compute the inverse of a matrix, where possible return the cached
# inverse if it has already been computed.
# To use: cached <- makeCacheMatrix(matrix)
#         inverse <- cacheSolve(cached)

## Write a short comment describing this function
# Generates the caching mechanism of a given matrix, x.
# A single returned cacheMatrix can be used for multiple matrices by using $setMatrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  m <- NULL
  setMatrix <- function(newMatrix){
    if (!is.null(m) && 
        (all(dim(m) == dim(newMatrix)) && 
      all(m == newMatrix))){
      # do nothing, this is the same matrix as before
    } else {
      # otherwise, set this matrix, and reset the inverse
      m <<- newMatrix
      inverse <<- NULL
    }
  }
  getMatrix <- function() m
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  setMatrix(x)
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Solves for the inverse of the "matrix" returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getInverse()
    if (!is.null(xinv)){
      print("getting cached")
      return(xinv)
    } 
    m = x$getMatrix()
    xinv <- solve(m)
    x$setInverse(xinv)
    xinv
}

