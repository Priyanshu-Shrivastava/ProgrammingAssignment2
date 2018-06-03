## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(c){
    x<<- c
    i<<- NULL
  }
  get<- function() x
  setinverse<- function(inv) i<<- inv
  getinverse<- function() i
  list(set=set, get=get, setinverse= setinverse, getinverse=getinverse)
  
}


## This function computes the inverse of special matrix object
## also get cached one if they are already present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i))
  {  message("getting cached data")
    return(i)}
  data<- x$get()
  
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
