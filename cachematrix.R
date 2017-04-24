##a pair of functions that cache the inverse of a matrix
## 

## function that construct a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      setmx<-function(mx){
        x<<-mx
        inv<<-NULL
    }
      getmx<-function() x
      setinv<-function(invmx){
         inv<<-invmx
    }
      getinv<-function() inv
  
  ##return an list object with four functions 
      list(setmx=setmx, getmx=getmx, setinv=setinv, getinv=getinv)
}


## function that computes the inverse of a matrix
##constructed by the makeCacheMatrix function
##check if a matrix inverse exists. If it exists, return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
          message("getting cached data-matrix inverse")
          return(inv)
      }
      data<-x$getmx()
      inv<-solve(data, ...)
      x$setinv(inv)
      inv  
}
