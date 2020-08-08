# I simply set the input x as a matrix
## and then set the solved value "m" as a null
## then I changed every reference from "mean" to "solve"

makecashematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() {inv}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Same here, changed "mean" to "solve" and "m" to "s"

cachesolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
