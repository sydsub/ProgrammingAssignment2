## makeCacheMatrix() creates an R object that stores an invertible matrix and  
## it's inverse.cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order to retrieve the inverse matrix from the cached value that is 
## stored in the makeCacheMatrix() object's environment.IF the cached value is empty, it 
## computes the inverse of the matrix. 

## makeCacheMatrix() creates 4 functions set(), get(), setinverse(), and getinverse() and 
## returns the functions within a list to the parent environment


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() takes in the argument from the output of makeCacheMatrix()
## and returns the cached inverse matrix. IF the cached value is NULL,
## it computes the inverse of the matrix and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
