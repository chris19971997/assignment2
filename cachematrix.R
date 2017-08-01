## These two functions together can calculate the inverse
## of a inversable matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat <<- inverse
  getinverse <- function() invMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinverse(invMat)
  invMat
}
