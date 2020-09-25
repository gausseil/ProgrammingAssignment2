## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores the input matrix and initializes the inverse matricx as null 

## makeCacheMatrix takes a matrix as input and creates a list object with functions to set 
## and get the matrix and set and get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve uses the makeCacheMatrix output list and returns the inverse matrix, either from the cache if it exists
## or calculates it and stores it in the output list object so as not to calculate it again for the same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
