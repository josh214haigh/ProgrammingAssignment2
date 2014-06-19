## These functions are a direct copy of the functions given to cache and 
## retrieve the mean.
## The changes made are swapping solve for mean, setmean and getmean have
## been swapped for setinverse and getinverse, and the function takes in
## a matrix not a numeric vector

## This function creates a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the list created with 
## the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
