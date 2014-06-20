## Name: makeCacheMatrix & cacheSolve

## Purpose:
  ## Matrix inverse calculations can be computationally 
  ##  expensive for large matrices.
  ## The functions makeCacheMatrix and cacheSolve allow 
  ##  a user to optimize run-time.

## Description:
  ## The function makeCacheMatrix creates a special
  ##  "matrix" that can store (cache) its own inverse.
  ## Once the inverse of this special matrix is calculated
  ##  with the function cacheSolve, it can be used for future needs.


## Example usage:
# >  special.matrix<-makeCacheMatrix()
# >  special.matrix$set(matrix(c(4,3,3,2),2,2))
# >  special.matrix$get()
##       [,1] [,2]
##  [1,]    4    3
##  [2,]    3    2
# >  cacheSolve(special.matrix)
##       [,1] [,2]
##  [1,]   -2    3
##  [2,]    3   -4
# >  cacheSolve(special.matrix)
##  getting cached data
##       [,1] [,2]
##  [1,]   -2    3
##  [2,]    3   -4
    ## Check that result is inverse, 
    ## using matrix multiplication:
# > special.matrix$get() %*% cacheSolve(special.matrix)
##       [,1] [,2]
##  [1,]    1    0
##  [2,]    0    1
#-----------------------------------------------------------------------


###makeCacheMatrix(x = matrix())
##  This function takes a square, invertible matrix as 
##   an argument, and returns a list of 4 functions.
##  A unique set of four functions is generated every time a new matrix
##   is given to makeCacheMatrix, pointing to that matrix.
##  These functions retrieve and store the value of an inverse once 
##   calculated, store the value of the original matrix once set,  
##   and allow a user to reset original matrix and inverse.
##  For efficiency, no inverse calculations are performed until these 
##   functions are called by cacheSolve, as needed.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                   # Initialize cached inverse as NULL

  ## Define 4 functions:##
  set <- function(y) {        # Binds x matrix VALUE to closure
    x <<- y                   #    environment
    i <<- NULL                # Invalidates any cached value
  }
  get <- function() x         # Returns x value binded to closure, else
                              # Returns x's value from global environment
  set.inverse <- function(inverse) {  #bind (cache) inverse to closure,
    i <<- inverse                     #replacing i=NULL
  }
  get.inverse <- function() { # returns cached inverse value, else NULL 
    i
  }

  ## Store 4 functions in a list:##
  list(set = set, get = get,      # FUNCTION OUTPUT: returns list of 
       set.inverse = set.inverse, #   functions, binded to i value & x
       get.inverse = get.inverse)
}



###cacheSolve(x,...)
##  This function takes the function list output of 
##   makeCacheMatrix for a given square invertible matrix as an 
##   argument, and returns its inverse.
##  cacheSolve first checks the *cache* to see if the matrix inverse was
##   already calculated and if so, returns it. If not, it *solves* and
##   *caches* the inverse in the function list and finally, returns it.
##  Any subsequent calls of cacheSolve on that same function list, will
##   naturally find a cached value, avoiding duplicate calculation.
##  If any non-matrix object or non-invertible matrix was given to
##   makeCacheMatrix, cacheSolve will return relevant solve error.
cacheSolve <- function(x, ...) {  # x argument is list of functions from
                                  #   makeCacheMatrix
  i <- x$get.inverse()        # Pulls cached inverse value, else NULL 
  if(!is.null(i)) {           # If i is not NULL, inverse already cached
    message("getting cached data")
    return(i)                 # Return cached inverse and stop function
  }
  matrixA <- x$get()          # If i is NULL (i.e. not calculated), pull
                              #   matrix name binded to functions
  i <- solve(matrixA, ...)
  x$set.inverse(i)
  ##  Returns a matrix that is the inverse of 'x', just calculated.
  i
}
