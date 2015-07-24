## The following two functions can be used to create a matrix 
## and calculate its inverse with the ability to cache the 
## inverse matrix if the original matrix remains unchanged.

## This function creates a special "matrix" object
## that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## setMatrix() function to store a matrix
  setMatrix <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
  ## getMatrix() function enables retrieval of the matrix
  getMatrix <- function() x
  
  ## setInverse() function is used by the cachesolve() function 
  ## to cache the value of the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## getInverse() function is used by the cachesolve() function 
  ## to retrieve the value of the cached inverse matrix
  getInverse <- function() inv
  

  list(set = setMatrix, get = getMatrix,
       setinverse = setInverse,
       getinverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  
    ## if already calculated use cache
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## get the data
    data <- x$get()
    
    ## caculate the inverse
    inv <- solve(data, ...)
    
    ## store in cache
    x$setinverse(inv)
    
    ## return the inverse
    inv
}
