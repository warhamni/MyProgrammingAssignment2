## Assignment2

## A pair of functions that cache the inverse of a matrix.
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y) {
    x<<-y;
    inverse<<-NULL;
  }
  get <- function() return(x);
  setinv <-function(inv) inverse<<-inv;
  getinv <- function() return(inverse);
  return(list(set= set, get=get, setinv=setinv, getinv=getinv))
}


## 2.cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above.cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse<- x$getinv() 
  if(!is.null(inverse)){
    message("Get cached data..")
    return(inverse)
  }
  data<- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
        ## Return a matrix that is the inverse of 'x'
}
