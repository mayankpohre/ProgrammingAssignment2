##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ### sets the initial value of inv to NULL and use it as a placeholder for a future value. 
  set <- function(y){  ## defines a function to set the 
    x <<- y ## matrix,x,to a new matrix,y 
    inv <<- NULL  ## and resets the inverse,inv, to NULL
  }
  get <- function() x ## returns the vector x
  setInverse <- function(inverse) inv <<- inverse  ## sets the inverse, inv, to inverse
  getInverse <- function() inv  ## return the inverse, inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## returns the vector containing all of the functions we just defined
}

### The following function calculates the inverse of the matrix created with the above function. 
### However, it first checks to see if the inverse has already been calculated. 
### If so, it gets the inverse from the cache and skips the computation. 
### Otherwise, it calculates the variance of the matrix and sets the value of the variance in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse() ## gets the inverse value of the matrix previously stored and hold it into inv
  if (!is.null(inv)) {
    message("getting cached data") ## message returned if inv already in memory
    return(inv)
  }
  mat <- x$get()  ## gets the matrix
  inv <- solve(mat, ...) ## calculates the inverse if not calculated earlier for this dataset
  x$setInverse(inv)  ## prints the inverse of the matrix 'x'
  inv
}


