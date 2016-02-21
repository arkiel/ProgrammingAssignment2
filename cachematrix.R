## Create 2 functions: 
## First function creates a "matrix" object that can cache its inverse. 
## A second function computes the inverse of the "matrix" object created by the first function. 

## The first function "makeCacheMatrix" creates a "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(solve) matrixinverse <<- solve
  getInverseMatrix <- function() matrixinverse
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The second function "cacheSolve" computes the inverse of the special matrix returned by the first function. 

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getInverseMatrix()
  
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  
  data <- x$getInverseMatrix()
  matrixinverse <- solve(data, ...)
  x$setInverseMatrix(matrixinverse)
  matrixinverse
}
