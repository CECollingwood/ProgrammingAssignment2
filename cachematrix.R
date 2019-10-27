##'These two functions optimise potentially time-consuming calculations with complex matrices
##'by caching the inverse of the matrix.
##'The first function calculates the inverse of a given matrix, caches it into a variable 'm' 
##'and creates an object that contains both the matrix and cache.
##'The second function is applied to this object to print the inverse matrix;
##'If the object has not changed, it prints the pre-calculated value of this inverse
##'If the object has changed (e.g. by using the set function on the object), it recalculates the inverse before printing it.




## makeCacheMatrix takes the matrix passed as a parameter, calculates its inverse and caches it into a variable 'm'
##'It does this in the following steps.
##

makeCacheMatrix <- function(x = matrix()) {
  ## Step 1: Create an empty cache matrix
  m <- NULL 
  ##Step 2: Define a function that sets the matrix and empty cache.
  set <- function(y) {
    x <<- y
    m <<- NULL ## each time the object is set or altered, m is 're'set to NULL
  }
  ##Step 3: Define a function that provides access to the matrix that has been passed to makeCacheMatrix
  get <- function() x
  ## Step 4: Define a function that calculates the inverse and stores it in m.
  setInverse <- function(solve) m <<- solve
  ## Step 5: Define a function that provides access to the cache.
  getInverse <- function() m
  ## Step 6: Create an object (with associated cache) by calling these four functions in turn.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##' This function checks whether an inverse to the matrix passed has already been created and
##' stored in a cache,
##' if it has, it displays a message and returns this value
##' and if not, it calculates the inverse, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) { #cache has already been calculated for this object, and it has not been changed
      message("getting cached data")
      return(m)       #use calculated value
    }
    data <- x$get() ## 'get's the data for the matrix provided
    m <- solve(data, ...) ## calculates the inverse and now stores it in m
    x$setInverse(m) ## set m to the cache
    print(m) ## prints the value of the inverse matrix
}


