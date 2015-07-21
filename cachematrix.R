## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeVector creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inv
# get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
# In this function the inverse of the matrix will be returned
# it checks if a inverse is already assigned to m. If yes it will return m.
# If not it will compute the inverse and cache it in m.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
