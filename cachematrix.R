##Functions that can cache and compute the inverse of a matrix.

## Creates a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  set <- function(y) 
    {
    x <<- y
    m <<- NULL
   }
  get <- function() x
  setinv <- function(inv)  m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

  }

cacheSolve <- function(x, ...) {
       
  m <- x$getinv()
  if(!is.null(m)) 
    {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
