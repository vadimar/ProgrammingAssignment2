## Test me with the following statements:
## A <- matrix(1:4, nrow=2, ncol=2)
## cacheSolve(makeCacheMatrix(A))


## Creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Calculates the inverse matrix using function makeCacheMatrix.
## It first checks whether the value of inversed matrix is cached.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise it calculates the inverse matrix and puts the result of calculation in the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}
