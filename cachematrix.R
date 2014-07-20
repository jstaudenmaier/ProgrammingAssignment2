## This pair of functions creates a method for caching the inverse of a 
#  matrix rather than computing it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL
  set <- function(y) {
    x <<- y
    myInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) myInverse <<- inverse
  getinverse <- function() myInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
#  `makeCacheMatrix` above. If the inverse has already been calculated 
#  (and the matrix has not changed), then it retrieves the inverse from 
#  the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
