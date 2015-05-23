## Theses two functions work togetherin solving the inverserve
## of a matrix and accessing the inverse from a cache after the first
## call.

## A function defining getter and setters function for a matrix and it's
## inverse.  returns a list of all the functions defined

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y      ## store the matrix in another environment
    m <<- NULL   ## setting a new matrix, so the inverse must be cleared
  }
  get <- function() x   ## return the matrix
  setMatrix <- function(solve) m <<- solve   ## set the inverse matrix
  getMatrix <- function() m  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## function to calculate and return the inverse of the matrix 
## defined in the makeCacheMatrix function. The function will
## first check the cache for the inverse matrix.  If the inverse
## has not been cached, it is calculated and then cached for future
## lookups.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()              ## get the inverse cache
  if(!is.null(m)){                ## if it exists return it
    message("getting cached data")
    return(m)
  }
  data <- x$get()                 ## otherwise, get the matrix
  m <- solve(data, ...)           ## calculate the invers
  x$setMatrix(m)                  ## and save the the cache
  m
}