## Functions in this program allows us to create a special kind of matrix. 
## This special kind of matrix can cache itself.
## The second function allows us to calculate inverse of this special matrix. 
## Once inverse is calculated, the 2nd function calls one of the sub-functions of 1st function to get/set the cached value.

## This function allows us to create a special kind of matrix. 
## This function also gets/sets the cached value.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversematrix <- function(imatrix) i <<- imatrix
  getinversematrix <- function() i
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function allows us to calculate inverse of the special matrix.
## Once inverse is calculated, this function calls one of the sub-functions of 1st function to get/set the cached value.
cacheSolve <- function(x, ...) {
  i <- x$getinversematrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversematrix(i)
  i
}
