## cachematrix.R
##
## THIS IS CODE FOR ASIGNMENT 2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(MA = matrix()) {
                    cachedInverse <- NULL
                    set <- function(y) {
                     MA <<- y
                      cachedInverse <<- NULL
  }
                  get <- function() MA
                    setInverse <- function(inverse) cachedInverse <<- inverse
                   getInverse <- function() cachedInverse
                       list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache 

cacheSolve <- function(MA, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- MA$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
              data <- MA$get()
              invFunc <- solve(data, ...)
              MA$setInverse(invFunc)
            invFunc
}
