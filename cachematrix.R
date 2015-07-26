## Matrix inversion can be a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than computing it over and over again.

### makeCacheMatrix creates a list of functions 
### that can cache the inverse of a matrix.
### The set function changes the vector stored in the main function
### The get function returns the vector x stored in the main function
### The setinverse function stores the value of the input in a variable 
### The getinverse function returns m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}

### cacheSolve checks to see if the inverse is already in cache.  
### If not in cache, cacheSolve computes the inverse of the matrix 
### and puts it into cache

cacheSolve <- function(x, ...) {
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
