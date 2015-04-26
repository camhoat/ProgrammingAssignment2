## Step 1: makeCacheMatrix creates a special matrix object, so later cacheSolve 
## can calculate the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  
  get <- function() x
  
  setinverse<- function(inverse) inverse_x <<- inverse
  
  getinverse <- function() inverse_x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Step 2: cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse has
##already been calculated, then the cachesolve will return the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## try to find a copy in the cache first
  inverse_x <- x$getinverse()
  if (!is.null(inverse_x)) {
    message("getting inverse matrix from cache")
    return(inverse_x)
  } 
  
  data <- x$get()
  
  inverse_x <- solve(data, ...)
  
  x$setinverse(inverse_x)
  
  return(inverse_x)
  
}
