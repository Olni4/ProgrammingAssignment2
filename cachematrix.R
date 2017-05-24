## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix.

## First function is used to set & get value of the  matrix/of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  The following function cacheSolve returns the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  } 
}
