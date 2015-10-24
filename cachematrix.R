
##cachematrix.R
## A cache matrix object is created to repeatedly solve the inverse of the matrix while it only calaculates the inverse once
##
## Example:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Changes the matrix that is being cached
##  M <- cacheMatrix$get()  # Returns the matrix that is being cached
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {           ## cacheMatrix object for an invertible matrix
cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function        


cacheSolve <- function(x, ...) {               ## This function is to return the inverse of a cacheMatrix object
        ## Return a matrix that is the inverse of 'x'     
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
        
        
}
