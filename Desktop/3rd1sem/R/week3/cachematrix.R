## Set the matrix to ccc
## and simultaneously set the inverse matrix k to NULL. 



makeCacheMatrix <- function(ccc = matrix()) {
  k <- NULL
  
  set <- function(x) {
    ccc <<- x
    k <<- NULL
  }
  
  get <- function() ccc
  
  setInverse <- function(inv) {
    k <<- inv
  }
  
  getInverse <- function() k
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Change the matrix object to "cache".
## If the inverse matrix is not cached, it is instructed to retrieve the original matrix (c) from the cache object and compute the inverse matrix (inv) using the solve function.

cacheSolve <- function(cache, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (!is.null(cache$getInverse())) {
    message("getting cached data")
    return(cache$getInverse())
  }
  
  c <- cache$get()
  inv <- solve(c, ...)
  cache$setInverse(inv)
  inv
}


# make matrix
ccc <- makeCacheMatrix(matrix(c(4, 2, 1, 3), nrow = 2))
# get matrix
ccc$get()
#inverse
cacheSolve(ccc)


