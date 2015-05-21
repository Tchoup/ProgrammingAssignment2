## These 2 functions work in tandem to create a special "matrix" and calculate it's inverse and check whether the inverse
## has been previously calculated and cached. If so, it will return the cached value and not calculate the inverse anew.
## the makeCacheMatrix creates the special "matrix," and calculates and caches the inverse.
## This function assumes that all matrices created are invertible. 

makeCacheMatrix <- function (x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}