# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # This will store the inverse
  
  # Set a new matrix and reset the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() {
    x
  }
  
  # Set the inverse (called from cacheSolve)
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse
  getinverse <- function() {
    inv
  }
  
  # Return a list of functions so they can be accessed later
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # First check if the inverse has already been cached
  inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)   # Use solve() to compute the inverse
  
  # Store the calculated inverse in the cache
  x$setinverse(inv)
  
  # Return the new inverse
  inv
}
