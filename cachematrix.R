## makeCacheMatrix creates a special "vector" as a list containing a function to -
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This functions produce the inverse of the matrix from makeCacheMatrix function
## It first checks if the inverse has already been calculated. 
## If it has, it skips the computation and sets the value from the cache. 
## Otherwise it will compute the inverse of the matrix using the 'solve' function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}

