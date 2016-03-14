## This code creates functions to create and solve matrix inversions, first
## checking to see if the inversion has already been calculated and cached.
## If so, it returns the cached value, to save processing time that would be
## required to make the calculation again.

## The makeCacheMatrix function creates a matrix object that can hold the 
## value of its inverse in memory

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat <<- inverse 
  getinverse <- function() mat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## The cacheSolve function calculates the inverse of the matrix object
## returned by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, then it returns that value in memory.

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat)){
    message("Returning solution already in cache")
    return(mat)
  }
  dat <- x$get()
  mat <- solve(dat)
  x$setinverse(mat)
  mat
       
}
