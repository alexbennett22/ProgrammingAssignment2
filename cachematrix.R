## The functions below are used to create a matrix with the ability to cache/recall its inverse
## By: Alex Bennett
## Date: November 24, 2017


## ------------------------ makeCacheMatrix -------------------------------
## Purpose: To create a matrix object with two Accessors for the matrix and matrix inverse and two Modifiers for the matrix and matrix inverse
## Inputs: x - either empty or a matrix
## Outputs: A list of the 4 function linked to the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_val) inv <<- inv_val
  getinverse <- function() inv
  list(set =set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## ------------------------ cacheSolve -------------------------------
## Purpose: To get the inverse of the matrix either from cache(if run before) or to calculate the inverse
## Inputs: x - a matrix
## Outputs: inv - the inverse of the matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
  
}
