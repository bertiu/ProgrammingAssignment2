## This function is a virtual function (blueprint) which defines what a real function (cachesolve) can do.

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL 
  h<- NULL
  
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
    h <<- NULL
    
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  sethash <- function(hash) h <<- hash
  gethash <- function() h
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse,
       sethash = sethash, gethash = gethash) 
}

## This function implements the functionality of makeCacheMatrix.
## Calculating the hash code which is needed to check for any changes 
## in the cached matrix must not consume more processing power than 
## calculating the inverse of the matrix. Otherwise caching the matrix 
## does not give any advantages compared to always recalculating the 
## inverse of the matrix.

cachesolve <- function(x, ...) {
  h1 <- digest::sha1(x$get(), 40)
  i <- x$getinverse() 
  if(!is.null(i)) { 
    message("getting cached data")
    h0 <- x$gethash() 
    ## Check for any changes in the cached matrix
    if (h0 == h1) {
      return(i) 
    } else {
      message("recalculate inverse matrix")
    }
  }
  data <- x$get() 
  i <- solve(data, ...)
  x$setinverse(i) 
  x$sethash(h1)
  i 
}
