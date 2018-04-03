## Put comments here that give an overall description of what your
## functions do
#Assignment 2 of R Programming
#Two functions makeCahckeMatrix and cacheSolve

## Write a short comment describing this function
#Function makeCacheMatrix creates a special "matrix" object that can 
#set and get original matrix
#And cache its inverse;
#Function returns a list

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) 
  {
    mat0 <<- y
    matinv <<- NULL
  }
  get <- function() mat0
  setinv <- function() matinv <<- solve(mat0)
  getinv <- function() matinv
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function
#Function cacheSolve takes as input output of above function
#to return cached inverse of the matrix ELSE
#create inverse of the matrix if it does NOT already exist (not cached)
#

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if(!is.null(matinv)) 
  {
    message("getting cached data")
    return(matinv)
  }
  mat0 <- x$get()
  matinv <- solve(mat0, ...)
  #print(matinv)
  x$setinv()
  matinv
}
