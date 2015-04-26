## Functions use for R.Programming

## Creates a special "vector"

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Mean calculation of the special "vector"

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


## Creates an inverse matrix to use as cache

makeCacheMatrix <- function(x = matrix()) {
  intermedio <- NULL
  set <- function(y) {
    x <<- y
    intermedio <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) intermedio <<- solve
  getsolve <- function() intermedio
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Solve the inverse of a cache matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    datos <- x$getsolve()
    if(!is.null(datos)) {
      message("searching the data cache")
      return(datos)
    }
    intermedio <- x$get()
    datos <- solve(intermedio, ...)
    x$setsolve(datos)
    datos
}