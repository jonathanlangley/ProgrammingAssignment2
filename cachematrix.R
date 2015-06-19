##The following two functions cache and compute the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed) then the cachesolve 
## should retrieve the inverse from the cache

##Assumption: Matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data....")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...)
  x$setinv(inverse)
  inverse
}