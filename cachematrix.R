
## This function is designed for avoiding calculate the
## inverse of a matrix again and again.
## There are two functions here.makdCacheMatrix(...)
## creates a special "matrix" object that can cache its inverse.
## cacheSolve(...) computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## The only argument of this function is x.
## This function can:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 1. set the inverse of the matrix
## 1. set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inver <<- solve
  getInverse <- function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## First,it checks to see it the inverse of
## matrix has already been calculated
## If so,get it.Skip the calculation
## If not, it calculates the result and set
## the inverse int the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data,...)
  x$setInverse(inver)
  inver
}
