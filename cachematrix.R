## makeCacheMatrix takes a matrix as an argument and creates
## a list containing a function that sets or gets the value of the matrix
## and sets or gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve will receive a special matrix created by makeCacheMatrix
## and return the inverse of the matrix used by makeCacheMatrix to 
## create the special matrix. It will set the inverse value in the cache
## of the special matrix.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
