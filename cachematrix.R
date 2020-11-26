## The following functions create a matrix object, calculate the inverse, 
## cache the calculated inverse. The inverse is loaded from cache if available.

## Makes a matrix object and commits its inverse to cache

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieves the inverse of the matrix from the cache if available, otherwise 
## calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
  }
