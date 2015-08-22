# These two functions below are used to calculate the inverse of a matrix. If the matrix inverse has not
# previously been calulated then it will be calculated using the R 'solve' function. If the inverse has 
# previously been calculated, it will be retrieved from the cache. This method avoids uneccessary 
# calculation of the matrix inverse.

# makeCacheMatrix creates a list containing functions to
#    set the value of the matrix
#    get the value of the matrix
#    store the value of the matrix inverse in the cache
#    retrieve the value of the matrix inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The following function calculates the inverse of a matrix created with the above function.
# However, first it checks to see if the matrix inverse has already been calculated and stored in the cache.
# If the inverse has already been calculated and stored in the cache, then it returns the stored value. Otherwise,
# it calculates the inverse of the matrix and sets/stores this value in the cache via the setinv function.


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
  m
}