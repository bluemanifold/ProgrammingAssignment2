#cashes inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv #inversed

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}