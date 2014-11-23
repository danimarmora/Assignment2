## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

## To start process for caching inverse of matrix 
  m <- NULL
  
## To set matrix
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  
## To get matrix
  get <- function() x {
## To set its inverse 
  setInverse <- function(inverse) m<<- inverse
## To get and return the inverse 
  getInverse <- function() m
 
## To return list of methods 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  ## To get the inverse matrix 
  m <- x$getinverse()
  
  ## To get the result of the inverse if it has already been computed
  if(!is.null(inv)) {
    message("getting cached data.")
    return(m)
  }
  
  ## To get the matrix from object
  data <- x$get()
  
  ## To compute the inverse 
  m <- solve(data)
  
  ## To set its value in the cache
  x$setinverse(inv)
  
  ## To return matrix
  m
}
