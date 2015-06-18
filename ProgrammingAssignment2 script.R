#makeCacheMatrix and cacheSolve are functions that can compute the inverse of
#a matrix repeatedly instead of computing the inverse each time.

#makeCacheMatrix sets and gets the value of a matrix, and does the same
#for its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverve=setinverse, getinverse=getinverse)
}

#cacheSolve solves for the inverse of the matrix above, excluding the
#inverses that have already been computed.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Repeated Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}