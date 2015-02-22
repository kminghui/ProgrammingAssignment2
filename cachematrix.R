##The pair of functions below is to cache the inverse of a matrix, which is potentially a 
##time-consuming computation. 

##The first function below creates a special "matrix" object that can cache its inverse.
##Assumption : x is an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The second function below computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been computed (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache and return a matrix that is the 
##inverse of 'x'.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
