
## Create an object able to store a matrix, and also its inverse 
makeCacheMatrix <- function(matrixInput = matrix()) {
  
  ## initialise values
  invMatrix <- NULL
  invMatrixLastInput <- NULL
  
  ## getter & setter
  set <- function(y) {
    matrixInput <<- y
    invMatrix <<- NULL
  }
  get <- function() matrixInput
  
  ## set: Matrix inverse
  setInverse <- function(inverse) {
    invMatrix <<- inverse
  }
  
  ## get: Matrix inverse
  getInverse <- function() {
    invMatrix
  }
  
  ## 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function to find the inverse of the matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## tries to retrieve the inverse of 'x' from the cache
  m <- x$getInverse()
  
  ## if there is data available from the cache, then returns it  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise, we'll calculate it... first step, get the input matrix
  data <- x$get()
  
  ## verify if it is a square matrix
  if (ncol(data) == nrow(data)) {
    
    ## yes, solve it
    m <- solve(data)
    
  } else {
    
    ## no, not a square matrix...
    message("error, the matrix is not square!")
    return
  }
  
  ## and we put the inverse value at the cache
  x$setInverse(m)
  
  ## and we also return this value
  m
  
}
