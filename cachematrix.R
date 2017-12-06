## R Programming assignment
## Coursera Data Science Course (2017)
##
## Assumption:
## matrix supplied is invertible


makeCacheMatrix <- function(x = matrix()) {
## This function creates a special 
## "matrix" object that can cache its inverse.
  
#create a variable "inv" that will be used to
#cache the inverse of the matrix  
  inv <- NULL            

#set the value of the vector    
  SetValue <- function(y) {
    x <<- y
    inv <<- NULL
  }
#get the value of the vector
  GetValue <- function() x
#set the value of the inverse
  SetInverse <- function(inverse) inv <<- inverse
#get the value of the inverse
  GetInverse <- function() inv
  list(setval=SetValue, getval=GetValue,
       setinv=SetInverse, getinv=GetInverse)
}


cacheSolve <- function(x, ...) {
## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
  
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
  data <- x$getval()

#Computing the inverse of a square matrix
#can be done with the solve function in R. 
#For example, if X is a square invertible 
#matrix, then solve(X) returns its inverse.  
  inv <- solve(data)
  x$setinv(inv)
  inv
}