##The pair of functions below is to cache the inverse of a matrix, which is 
##potentially a time-consuming computation. 

##The first function below creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##The second function below computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been computed (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache and return a matrix that is the 
##inverse of 'x'.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
