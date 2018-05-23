## The two functions toghter solves the inversion of a given matrix.
## It stores the result once calculated and thus preventing repeating calculations.

## This function creates an object, which stores the result.

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y){
    x <<- y
    v <<-  NULL
  }
  get <- function() x
  setinv <- function(inv) v <<- inv
  getinv <- function() v
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

## This function calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  v <- x$getinv()
  if(! is.null(v)){
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinv(v)
  v
}