## This program computes the inverse of a function
## However, if the contents of a mtraix are not changing, the program will not recalculate the inverse.
## Instead the program look up in the cache rather than recompute the inverse. 


## This function creates a special "matrix", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

 
 makeCacheMatrix <- function(x = matrix()) { 
    x_inverse <- NULL
    set <- function(y){
      x <<- y
      x_inverse <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(x_i) { x_inverse <<- x_i}
    get_inverse <- function () x_inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

} 


 ## The following function calculates the inverse of the special "matrix" created with the above function.
 ##However, it first checks to see if the inverse has already been calculated. 
 ##If so, it  get s the inverse from the cache and skips the computation. 
 ##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the  set_inverse  function.


 cacheSolve <- function(x, ...) { 
         ## Return a matrix that is the inverse of 'x' 
   x_inverse <- x$get_inverse()
   if(!is.null(x_inverse)) {
      message("getting cached data")
      return(x_inverse)
   }
   data <- x$get()
   x_inverse <- solve(data,...)
   x$set_inverse(x_inverse)
   return (x_inverse)
} 
