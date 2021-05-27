makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
#this function SET only creates the working matrix that should be inverted.
#the first time the code runs, SET is not used, because x is passed as argument.  
#if the argument y is used, inv will receive NULL, therefore, NOT cached anymore
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  
#This inv has nothing to do with inv from makeCacheMatrix because they are at
#different environments  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
#x$setInverse(inv) will update the inv variable inside the makeCacheMatrix
#environment. This occurs because <<- is used (inv <<- inverse) will change
  x$setInverse(inv)
  
  inv
}
