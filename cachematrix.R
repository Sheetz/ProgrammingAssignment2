

## make a special matrix with four functions. These functions are
## 1. set - sets the value of special matrix in cache( with <<- )
## 2. get - get the vaue of special matrix from cache.
## 3. setinverse - save the inverse of matrix in cache.
## 4. getinverse - pull value of inverse from cache.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


##  calculates the inverse of the special "matrix" created with the function makeCachematrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
