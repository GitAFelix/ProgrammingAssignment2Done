## makeCacheMatrix handles four basic functions get and set matrix and inverse
## The get functions return the already stored variables whereas the set functions
## store the variables in the cache,

makeCacheMatrix <- function(x = matrix()) {
  # Variables is initialised
  inv <- NULL
  # Defines a function which assigns a new matrix to x and clears inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Defines a function to retrieve matrix
  get <- function() x
  # Defines a function to store inverse to cache
  setinv <- function(inverse) inv <<- inverse
  # Defines a function to retrieve inverse
  getinv <- function() inv
  # Binds the above defined functions to a list
  list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}

## cacheSolve verifies whether a cache exists and returns either cache or calculates 
## new inverse, stores it in cache and returns it.

cacheSolve <- function(x, ...) {
  # Retrieves cached inverse from makeCacheMatrix function, even if null
  inv <- x$getinv()
  # Verifies whether cache is not null
  if(!is.null(inv)){
    # If so, simply returns the cached inverse
    message("getting cached data")
    return(inv)
  }
  # If cache is empty, assigns matrix to matrix cache
  data <- x$get()
  # ... and generates inverse of matrix
  inv <- solve(data, ...)
  # ... then stores the inverse in cache via setinv function of makeCacheMatrix
  x$setinv(inv)
  # Finally returns inverse
  inv
  ## Return a matrix that is the inverse of 'x'
}


#Example from Coursera for mean calculation and caching
#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}

#Example from Coursera for getting mean from above function
#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}
