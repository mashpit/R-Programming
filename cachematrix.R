## This pair of functions calculate and cache the inverse of a matrix.

## The makeCacheMatrix function creates a "matrix" object that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse for that matrix has been computed previously, then this function will 
## retrieve the inverse from the cache.
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
