## Function used to perform matrix inversion and be cached. If an inversion of  the matrix exists
## the function will retrieve it from cache

## Creates a list of functions for matrix inversion. This utilises the scopping capability of R

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## function which assigns the matrix to cache
    x <<- y
    m <<- NULL
  }
  get <- function() x ## retrieves matrix from cache
  setinvert <- function(invert) m <<- invert ## inverted matrix
  getinvert <- function() m ##retrievesinverted matrix from cache
  list(set = set, get = get, ## creates the list of functions
       setinvert = setinvert,
       getinvert = getinvert)
}


## function to check existence of an inverted matrix and performs inversion 
## if matrix does not exist

cacheSolve <- function(x, ...) {
       
  m <- x$getinvert()
  if(!is.null(m)) { ## checks  for inverted matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## inverts the matrix
  x$setinvert(m)
  m
}


## to get inverse of matrix x type "cacheSolve(makeCacheMatrix(x))"

