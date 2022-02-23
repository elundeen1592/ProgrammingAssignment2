############################################
#############R Programming Assignment 2
############################################

#Here I am creating a special matrix object that stores/caches the value of the inverse of a 2x2 matrix
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  creatematrix <- function(data) {
    x <<- data
    cache <<- NULL
  }
  retrievematrix <- function() {
    x
  }
  cacheinverse <- function(solve) {
    cache <<- solve
  }
  getinverse <- function() {
    cache
  }
  list(creatematrix = creatematrix, retrievematrix = retrievematrix, cacheinverse = cacheinverse, getinverse = getinverse)
}

a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2) )


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated, then the cache solve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("retrieving cached data")
    return(inverse)
  }
  data <- x$retrievematrix()
  inverse <- solve(data)
  x$cacheinverse(inverse)
  inverse
}

cacheSolve(a)

