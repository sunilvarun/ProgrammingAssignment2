## Creates an object of type matrix embeds 4 functions within to store (cache) the 
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Calcualtes the inverse of the matrix created by the function above
## If the inverse has already been calculated for the same matrix
## then gets the result from cache. If the matrix has changed or if inverse
## has not been calculated, then it calculates it and returns the result

cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if (!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}
