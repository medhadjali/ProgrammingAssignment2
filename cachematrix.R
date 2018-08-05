## makeCacheMatrix creates a special vector containing functions to (1) Set a matrix,
## (2) get a matrix, (3) Set the inverse of a matrix and (4) Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #varibale "i", the inverse of a matrix, is set to NULL
  i <- NULL
  
  #function "set" that sets the value of a matix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #function "get" that returns the value of a matrix
  get <- function() x
  
  #function "setinv" that sets the value of the inverse of a matrix
  setinv <- function(solve) i <<- solve
  
  #function "getinv" that returns the value of the inverse of a matrix
  getinv <- function() i
  
  #return the list of functions "set", "get", "setinv" and "getinv"
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve is a function that returns the value of the inverse of a matrix. It first checks if the 
## value of the inverse has been already calculated. If it is the case, it  returns the cached value of the
## inverse without calculation. Otherwise, it porceeds with the calculation and return the value. 

cacheSolve <- function(x, ...) {
  
  ## get the value of the inverse of the matrix from the cache
  i <- x$getinv()
  
  ##return the cached inverse (in the case it is not NULL)
  if(!is.null(i)) {
    message("Getting the cached Inverse")
    return(i)
  }
  
  ##In the other case get the value of the matrix and calculate then return the value of its inverse.  
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinv(i)
  i
  
}
