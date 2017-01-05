## -------------------------------------------
##
## Created by: Max
## Created date: 05/01/2016
## Description: Create list of functions for later inverse a matrix
## Return: List of functions
## Input: A matrix but need a square matrix for later
##
## Example of input: matrix(rexp(400, rate=.1), ncol=20)
##                   matrix(rexp(200, rate=.1), ncol=20) for none square matrix
##
## Modification date:
## Modify by: 
##
## -------------------------------------------



makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL  # init the cache value to null
  
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(inverse) cached <<- inverse
  getInverse <- function() cached
  
  # return teh function to teh env
  list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

## End makeCacheMatrix



## -------------------------------------------
##
## Created by: Max
## Created date: 05/01/2016
## Description: Calculate teh inverted matrix
## Return: Matrix
## Input: A matrix that needs to be a square matrix
##
## Modification date:
## Modify by: 
##
## -------------------------------------------


cacheSolve <- function(x, ...) {
  cached <- x$getInverse()
  
  # 1- create a matrix
  matrix <- x$get()
  
  # 2- Check taht the matrix is square
  # If not, basic error message management
  tryCatch( {
    # Return inversed matrix
    cached <- solve(matrix, ...)
  },
  # Error management
  error = function(error) {
    message(error, "") 
  },
  # Warning management
  warning = function(warning) {
    message(warning, "")
  })
  
  # Return the inverted matrix
  return (cached)
}

## End cacheSolve
