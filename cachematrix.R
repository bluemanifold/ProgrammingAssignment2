#cashes inversed matrix


# Below function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize
  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  #get/set values
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv #inversed

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {

# The following function returns the inverse of the matrix. 
# Function first checks if inverse already is computed - if true, 
# it gets result and skips computation, else performs computation.
# We are assuming matrix is always invertible.

  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  #calculate
  m <- solve(data) 
  #then set inversed matrix
  x$setInv(m) 
  #function returns m
  m 
}
