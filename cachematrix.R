## There are two functions in this file: both are used to store and manipulate matrices 

## This function is designed to store a matrix in the cache to be used for inversion in the cachemean function below

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      temp <- function(y) {
      	x <<- y
      	inv <<- NULL
      }
      pull <- function() x
      invertMatrix <- function(inverse) inv <<- inverse
      pullInverse <- function() inv
      list(temp = temp,
      	pull = pull,
      	invertMatrix = invertMatrix,
      	pullInverse = pullInverse
      	)
}


## This function inverts the matrix created in the makeCacheMatrix function from above. If it's already been inverted, it just pulls it from the cache instead.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$pullInverse()
      if(!is.null(inv)) {
      	message("getting cached data")
      	return(inv)
      }
      mtrx <- x$pull()
      inv <- solve(mtrx, ...)
      x$invertMatrix(inv)
      return(inv)
}