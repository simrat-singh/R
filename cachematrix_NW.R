makeCacheMatrix1 <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set <- function(inv_data) inv <<- inv_data
  getinv <- function() inv
  list(set = set, get = get, setinv = set, getinv = getinv)
}

cacheSolve1 <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #m <- mean(data, ...)
  m<-solve(data)
  x$setinv(m)
  m
}