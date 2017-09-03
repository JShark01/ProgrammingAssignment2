## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL  ##Begins by setting the inverse to NULL as placeholder
  set<-function(y) {
    x<<-y
    inv<<-NULL
  } ##Defines a function to set the matrix x to a new matrix y, and resets the inverse "inv" to NULL
  get<-function() x  ##Returns matrix x
  setinv<-function(solve) inv<<-solve ##sets inv to solve
  getinv<-function() inv  ##returns the inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv) ##returns the 'special vector' containing all of the functions just defined
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


