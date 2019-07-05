## Two functions that are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(solveNewMat)invert <<- solveNewMat
  getinverse <- function() invert
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve compute the inverse of the special matrix returned by makeCacheMatrix
## cacheSolve will retrieve the calculated inverse from the cache if the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("getting cached matrix")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinverse(invert)
  invert
}
