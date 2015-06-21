

## Function which creates a special Matrix which is really a list containing several functions

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<-inv
  getinv <- function() m
  list(set = set,get= get,setinv = setinv,getinv = getinv)
  
}


## Function which calculates the inverse of a matrix but before it check if the inverse has already been calculated, if so it skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}