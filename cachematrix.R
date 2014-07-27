## The following two functions perform two functions for a given matrix:
##
## 1.- Storing in cache the value of the matrix and its inverse
## 2.- Computing the inverse of the matrix in an intelligent way, in the sense that first
##     it will be checked whether the inverse has already been computed (exists in cache)
##     and if so the value will be directly returned.

## The following function creates a list containing information about the matrix 
## and its inverse (setting the matrix value, getting the matrix value, setting the 
## matrix inverse and getting the matrix inverse). This will be useful because the first time
## the inverse of a matrix is computed, the value will be stored in the last element of the list
## so that it can be re-used whenever it is asked again, which will avoid that R has to 
## perform all the inverse computation again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function first checks if the result of inverting the given matrix already
## exists in cache and if so returns that result. If it does not exist, it computes the
## inverse of the given matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
