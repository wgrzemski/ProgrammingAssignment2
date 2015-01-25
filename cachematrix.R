## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix returns a list of functions which:
# matrix.set: sets value of the matrix
# matrix.get: gets value of the matrix
# inverse.set: sets value of the matrix' inverse
# inverse.get: gets value of the matrix' inverse
makeCacheMatrix <- function(x = matrix()) {
  ## returns 'matrix' object
  
  # inv will hold inverse of matrix "x":
  inv <- NULL

  # matrix.set will set matrix to be inverted:
  set <- function(m) {
    x   <<- m
    inv <<- NULL
  }
  
  # matrix.get will get matrix for which the inverse has already been computed:
  get <- function() {
    return(x)
  }
  
  # inverse.set will set value of the inverse for the matrix:
  inverseset <- function(inverse) {
    inv <<- inverse
  }

  # inverse.get returns value of the inverse for the matrix:
  inverseget <- function() {
    return(inv)
  }
  
  # return named list:
  list(
      set        = set,
      get        = get,
      inverseset = inverseset,
      inverseget = inverseget
  )
 
}

## Write a short comment describing this function:
# cacheSolve returns inverse of matrix from matrix returned by "makeCacheMatrix"
# assumption here is that any changes to matrix have been applied by matrix.set()
# within makeCacheMatrix (i.e. inv was NULL-ified)
cacheSolve <- function(x,...)
{
  # returns a matrix that is inverse of 'x':
  # check if inverse is available, if so, cacheSolve has been called before and
  inv <- x$inverseget()
  if(!is.null(inv)) {
     return(inv)
  }
  # if it's not matrix.set() has been called without making a cache
  # get inverse and stick it in the "inv" of x
  else {
    temp     <- x$get()
    tempinv <- solve(temp,...)
    x$inverseset(tempinv)
    return(tempinv)
  }
}
################################################################################    
