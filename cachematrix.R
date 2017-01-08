## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a list containing functions for acting on a matrix
#  4 wrapped functions:

#  0. create cached inverse object and sets it to NULL
#  1. Set the value of the matrix, Set cached inverse to NULL
#  2. Gets stored value of matrix
#  3. Sets the value of the cached inverse
#  4. Gets the stored value of the cached inverse

# Called/created using someMatrix <- makeCacheMatrix(x = matrix())

makeCacheMatrix <- function(x = matrix()) {
  
  # Set cached inverse to NULL
  cachedInverse <- NULL
  
  #  1. Set the value of the matrix, Set cached inverse to NULL  
  # Called using someMatrix$setMatrix(x = matrix())
  
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    # New Matrix means new inverse
    cachedInverse <<- NULL
   }
  
  #  2. Gets stored value of matrix
  # Called using someMatrix$getMatrix()
  
  getMatrix <- function() x
  
  #  3. Sets the value of the cached inverse
  # called by cacheSolve in the form someMatrix$setCachedInverse(inverseMatrix)
  
  setCachedInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix
  
  #  4. Gets the stored value of the cached inverse    
  #  called by cacheSolve in the form someMatrix$getCachedInverse()
  
  getCachedInverse <- function() cachedInverse

  list(setMatrix=setMatrix, getMatrix=getMatrix, setCachedInverse=setCachedInverse, getCachedInverse=getCachedInverse)  

}


# cacheSolve generates the inverse of the cached matrix and stores it for reuse
# Performs 2 functions:
# 1. Reads the stored value of the inverse
# 2. if the value does not exist - generate and store the value

# Called using cacheSolve(someMatrix)

  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # 1. Reads the stored value of the inverse
  
  inverseMatrix <- x$getCachedInverse()
  
  # Test to see if cached inverse is Null - then calculate the cached inverse and store it as cachedInverse
  # 2. if the value does not exist - generate and store the value  
  if (is.null(inverseMatrix)) {
    matrixToSolve <- x$getMatrix()
    inverseMatrix <- solve(matrixToSolve)
    x$setCachedInverse(inverseMatrix)
  }
  # return the value
  
  inverseMatrix
}
