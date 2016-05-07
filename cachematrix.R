## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## x = inversible matrix
  
  ## using the example from the MakeVector.r
  ## create a list/vector function that:
  ## 1. set the matrix from x
  ## 2. get the matrix
  ## 3. set inverse
  ## 4. get inverse
  ## 5. create command list for function
  
  ## example :
  ## m <- NULL
  ## set <- function(y) {
  ##  x <<- y
  ##  m <<- NULL
  ## }
  ## get <- function() x
  ## setmean <- function(mean) m <<- mean
  ## getmean <- function() m
  ## list(set = set, get = get,
  ##      setmean = setmean,
  ##      getmean = getmean)
  
  ## 1
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## 2
  get <- function() x
  ## 3
  setInverse <- function(inverse) i <<- inverse
  ## 4
  getInverse <- function() i
  ## 5
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## 1. get the inverse
  i <- x$getInverse()
  
  ## 2. check if inverse is cached
  ## if it is return the inverse from cache
  if(!is.null(i)){
    return(i)
  }
  
  ## 3. calculate the inverse if not cached
  ## first attempt failed with error:
  ##    Error in as.vector(x, mode) : 
  ##      cannot coerce type 'closure' to vector of type 'any'
  ##i <- solve(x$get) -- forgot () on get function
  my.data <- x$get()
  ##i <- solve(my.data, ...) - Error in solve.default(my.data, ...) : 'a' (3 x 2) must be square
  ## problem is test data i need a matrix of square
  i <- solve(my.data, ...)
  
  ## 4. set inverse
  x$setInverse(i)
  
  return(i)
  
}


# test
# mat <- matrix(rnorm(333),nrow=3,ncol=3)
# is.square.matrix( mat )
# m <- makeCacheMatrix(mat)
# cacheSolve(m)
