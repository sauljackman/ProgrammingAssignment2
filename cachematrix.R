## Together, these functions generate a list of outputs regarding an inputted matrix, and then return the inverse of that matrix.  If the inverse has already been solved, the inverse is pulled from the cache; otherwise, it is calculated, returned, and then cached.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## This function computes the invesre of the special matrix returned by makeCacheMatrix.  If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
