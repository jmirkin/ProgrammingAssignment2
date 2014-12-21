## These two functions work in conjunction with each other 
## to calculate the inverse of a matrix and store the result in a spectial 
## vector that is built with the makeCacheMatrix function. 


## makeCacheMatrix creates a special list that will contatin the matrix and its cached inverse
## it also contains the functions necessary for setting and getting both values

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL
  }
  get  <- function() x
  
  setinverse  <- function(inverse) i <<- inverse
  getinverse  <- function() i 
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve expects an argument in the form of the list that is returned from makeCacheMatrix

## cacheSolve checks to see if the cached inverse exists on x and returns the cached inverse if it exists.
## If the cached inverse is undefined it will calculate the inverse and store it on x and return the inverse.

cacheSolve <- function(x, ...) {
  i  <-  x$getinverse()
  if(!is.null(i)){
    message("getting  cached data")
    return(i)
  }
  
  data  <- x$get()
  i  <- solve(data)
  x$setinverse(i)
  i
}
