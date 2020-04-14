## This program is an implementation of lexical scoping check.
## When any input (matrix) provided to function cacheSolve then it checks if the inverse  
## of the matrix is being precalculated or not. If the values is being pre calculated and available in cache
## then the function returns the cached value else it calculate the value and store for next interaction.


## makeCacheMatrix function is being used to called to build the matrix. Before calling the CacheSolve function 
## if you want in your program to pre calculate the value then this function first get called with input matrix.
## As part of this function a variable has been defined with name "m" which has value null intially.
## Set function use to store the data in memory, <<- symbol is being used for that specifically.
## setsolve function calculate the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  set <- function(a) {
    x <<- a
    m <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function call the getsolve function in makeCachematrix and get "m" value. 
## If m has already a value its prestored value return.
## else the solve function get executed on input matrix and solve output get saved

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setsolve(m)
  
  m
}
