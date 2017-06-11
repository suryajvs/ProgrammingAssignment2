## makeCacheMatrix cdoes the following:
## 1. Set and Get the Value of the Matrix
## 2. Set and Get the Value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
  set<-function(y)
    {
      x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve gives the inverse of the matrix. If the inverse has been computed, it gets the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv1<-x$getinverse()
  if(!is.null(inv1)){
    message("Getting Cached Data!")
    return(inv1)
  }
  m<-x$get()
  inv1<-solve(m)
  x$setinverse(inv1)
  inv1
}
