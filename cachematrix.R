## Programming Assignment 2: Lexical Scoping
## This assignment requires us to cache the inverse of a matrix
## so that we can use the inverse from cache instead of computing 
## it again if the matrix has not changed.

## The function makeCacheMatrix creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatInverse <- function(inverse) m <<- inverse
  getmatInverse <- function() m
  list(set = set, get = get, setmatInverse = setmatInverse, getmatInverse = getmatInverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix is same), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatInverse()
  
  # check whether inverse exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # if inverse doesn't exist then compute it
  our_matrix <- x$get()
  m <- solve(our_matrix, ...)
  x$setmatInverse(m)
  m
}
