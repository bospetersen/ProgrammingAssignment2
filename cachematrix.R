## Run the function:
## > source("cachematrix.R")    Load the R program
## > a <- makeCacheMatrix()     Create functions used to handle the inversion and cache.
## > a$set(matrix(1:4, 2, 2))   Create matrix in working environment
## > cacheSolve(a)              1st run returns inverted matrix and stores it in cache.
## > cacheSolve(a)              Run agian and you will get the Cached version of the matrix. 


## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  # Stores the cached value initialize to NULL
  cache <- NULL
  
  # Create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Tnverts the matrix and stores it in cache
  setMatrix <- function(inverse) cache <<- inverse
  
  # Get the inverted matrix from cache
  getInverse <- function() cache
  
  # Return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it is created in the and it's value is stored in cache

cacheSolve <- function(x, ...) {
  
  ## Attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  # Return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    
    # Display matrix in console
    return(cache)
  }
  
  # Create matrix if it does not exist
  matrix <- x$get()
  
  # Make sure matrix is square and invertible
  # (Error handling is missing)
  
    cache <- solve(matrix, ...)
    x$setMatrix(cache)

      return (cache)
}

