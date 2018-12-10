## The following functions help in getting the value of the inverse matrix from the cache
## makeCacheMatrix helps in creating a special matrix 

makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function()x
  setinv<- function(inversesolution) inv<<-inversesolution
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Cache Solve calculates the inverse of the matrix and checks if the inverse is already calculated
##if it is calculated it gives the inverse from cache and skips the computation 

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get
  inv<-inversesolution(data,...)
  x$setinv(inv)
  inv
}
