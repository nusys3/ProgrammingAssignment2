## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## The inverse is initialized as NULL
  i <- NULL
  
  ## defining required functions
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  ## Return a special matrix containing functions as elements required.
  matrix(c(set, get, setInverse, getInverse), nrow=1, ncol=4, dimnames=list(c("function"), c("set","get","setInverse","getInverse")))
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check if the inverse is cached. If so, return cached one.
  i <- x[["function", "getInverse"]]()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If not cached, computes the inverse
  i <- solve(x[["function", "get"]]())
  x[["function", "setInverse"]](i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
