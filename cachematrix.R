## Programming Assignment # 2 for rprog-033 

## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize to NULL
  cachedMatrix <- NULL
  
  setMatrix <- function(value){
    
    # Assign the new value to the Matrix
    x <- value
    
    # Assign NULL to the cachedMatrix to flush the previously set value
    cachedMatrix <- NULL
  }
  
  getMatrix <- function(){
     return(x)
  }
  
  setInverse <- function(passedInverse){
    
    # Set inverse of 'Matrix'
    x <<- passedInverse
    x <<- NULL
    
  }
  
  getInverse <- function(){
    
    # Retreive inverse of 'Matrix'
    return(x)
  }
  
  # return the "matrix"-object which is a list of the above functions
  list(set = setMatrix, get = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  
  # if a cached value exists return it
  if(!is.null(inverse)) {
    
    message("Getting cached data")
    return(inverse)
  }
  # Else get the matrix, caclulate the inverse and store it in cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # Return inverse
  return(inverse)
}
