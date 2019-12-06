## Put comments here that give an overall description of what your
## functions do
## Since matrix inversion is a time-consuming task, we can cache the inverse of a matrix rather than computing it again and again.
## Both the function here do exactly that.makeCacheMatrix creates a "special" matrix object, whereas
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## Write a short comment describing this function

## This function creates a special matrix object that is actually a list of functions that set the value of the matrix(set),get the value of the matrix(get),
## set the value of the inverse of the matrix(setinv), and get the value of the inverse of the matrix(getinv).
makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<- function() x
  setinv<- function(solve) s<<- solve
  getinv<- function() s
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinv function.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s<-x$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  mat<-x$get()
  s<- solve(mat,...)
  x$setinv(s)
  s
}
