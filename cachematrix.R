## makeCacheMatric receives a square invertible matrix and builds
## a cacheable object that can be resolved from previous computation

## cacheSolve computes the solve of a matrix or retrives
## previously computed value from cache

## makeCacheMatrix
## Receives a square matrix with same rows and cols
## and construct a cacheable object with 4 functions

makeCacheMatrix <- function(x = matrix()) {
  # First verify that x is square
  if (nrow(x) != ncol(x)) {
    message("Matrix is not square")
  } else {
  	m <- NULL
   set <- function(y) {
   	x <<- y
	m <<- NULL
   }
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

  }
}


## cacheSolve
## verifies if the value of a previously computed solve(matrix)
## is available in a higher environment then it uses it
## or if not present compute the value from the matrix itself

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}