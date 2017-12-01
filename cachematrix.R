## Creates an object of type matrix embeds 4 functions within to store (cache) the 
## inverse of a matrix

## Create the function and pass x as the argument and initialize it as a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initiatilise the variable i and set it as null
    i <- NULL
    
  ## Define the set function to set values needed for computation
    set <- function(y) {
      
  ## Assign the input argument passed into y to the x object in the parent environment. 
  ## This is the first part of the 4 part compuation where the matrix that needs to be worked on
  ## is passed in y through the original argument in makeCacheMatrix x. The double "<<
  ## is used to write value into an object which is a part of the parent environment of function y
        x <<- y
        
  ## Clear the value of i from cache from any previous computations
  ## The first time that the function is run, the value of i will be null anyway
  ## but when called for the 2nd time with a different matrix, it needs to be 
  ## cleared of the previous value
        i <<- NULL
    }
    
  ## get the value of x from the parent environment
  ## (That is why x is written after the function body)
    get <- function() x
    
  ## Call the the setInverse function, passing inverse as an argument
  ## the value of inverse after computation is passed back into i
  ## which is a global environment object hence the use of "<<"
    setInverse <- function(inverse) i <<- inverse
    
  ## Now get the value using the get inverse function
    getInverse <- function() i
    
  ## Write the values of all the 4 functions as a list and return to parent
  ## to be used by other code below. In this case, to be used by cacheSole
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Calcualtes the inverse of the matrix created by the function above
## If the inverse has already been calculated for the same matrix
## then gets the result from cache. If the matrix has changed or if inverse
## has not been calculated, then it calculates it and returns the result

cacheSolve <- function(x, ...) {
  
  ## Get the inverse in object i 
      i <- x$getInverse()
  
  ## Check if the inverse has already been calulated or not
      if (!is.null(i)) {
  
  ## Not Null means it has been calculated for this matrix, 
  ## return i from before and exit here
        message("getting cached data")
        return(i)
      }
      
  ## if i is null, then calculate inverse
  ## Call the get function by accessing the get element in the list which is the matrix
      data <- x$get()
  
  ## use built in solve function to calculate inverse
      i <- solve(data, ...)
      
  ## Reset the value with the new inverse
      x$setInverse(i)
  
  ## Return the new value
      i
}
