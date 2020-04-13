## Creates a matrix, calculates its inverse and sets them both for posterior use

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(z) {
    x <<- z
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(x) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if inverse matrix is set, if so, posts it without calculating again, otherwise, 
## calculates the inverse matrix and posts it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- x$get()
  i <- solve(i)
  x$setinverse(i)
  i
}
