## Programming Assignment 2 ##
## The following functions cache the inverse of a matrix ##

## The makeCacheMatrix function below creates a matrix object that can
## cache/store the inverse of a matrix.

makeCacheMatrix<-function(x = matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The cacheSolve function computes the inverse of the matrix
## entered into makeCacheMatrix above unless it was previously
## calculated. In that case it retrieves the cached inverse.

cacheSolve<-function(x,...){
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}
