## This function creates a special "matrix" object that can cache its inverse.

## Initialize the inverse property
## Get method for the matrix
## Then set the inverse of the matrix
## Get method for the inversed matrix

makeCacheMatrix <- function(x = matrix()) {

  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) 
   
  i <<- solveMatrix
  getInverse <- function() i
    
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i       
}
