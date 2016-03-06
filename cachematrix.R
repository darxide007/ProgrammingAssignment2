
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: function that caches and retrieves both matrix x and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # cache matrix = x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Retrieve matrix x
    get <- function() x
  
    # Cache matrix inverse = m
    setMatInverse <- function(MatInv) m <<- MatInv
    
    # Retrieve matrix inverse m
    getMatInverse <- function() m
    
    list(set = set, get = get,
         setMatInverse = setMatInverse,
         getMatInverse = getMatInverse)
}


## Write a short comment describing this function
# cacheSolve: returns the inverse of a matrix. 
# STEP 1: look for cached inverse. if no cached inverse, then solve() for inverse and cache the inverse matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check for cached inverse
    m <- x$getMatInverse()
    # retrieve the cached inverted matrix and return it if it exists.
    if(!is.null(m)) {
        message("getting cached data")
        # return cached data
        return(m)
    }
    # if the cached inverse matrix does not exist, then:
    # calculate inverse and cache it.
    data <- x$get()
    m <- solve(data, ...)
    x$setMatInverse(m)
    m
}
