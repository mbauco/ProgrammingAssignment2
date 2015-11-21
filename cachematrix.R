## Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly 


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverso <- NULL
  set <- function(y) {
      # <<- assing a value to an object in a different environment from the current
      x <<- y
      inverso <<- NULL
  }
  #get the matrix
  get <- function() x
  #Set the inverse matrix
  setinverso <- function(inverse) inverso <<- inverse
  #Get the inverse matrix
  getinverso <- function() inverso
  #Return the matrix with new functions
  list(set=set, get=get, setinverse=setinverso, getinverse=getinverso)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #Return a inverse matrix of x
  inv <- x$getinverse()
  #If the inverse is already calculated, return it
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  #The inverse is not calculated yet, so it calculate
  data <- x$get()
  inv <- solve(data)
  # Cache the inverse
  x$setinverse(inv)
  #return
  inv
}
