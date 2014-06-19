## Put comments here that give an overall description of what your
## functions do

## These functions allow for cached computation of the inverse
## of matrices, i.e., if the inverse of the matrix has already
## been computed, it will not be recomputed again

## Usage: x0 is a matrix
##        x <- makeCacheMatrix(x0)  - returns a chache matrix 
##             (in fact: a list of function references, see below)
##        cacheSolve(x) - returns the inverse of x0



## Write a short comment describing this function

## Given a matrix x, this function creates a list of functions
## that allow to manipulate this matrix and its chached inverse
## In particular, via the functions contained in the list, we can
## set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv0) inv <<- inv0
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

## The argument x is the list returned by the makeCacheMatrix function
## This function returns the inverse of the matrix associated with x.

## This function uses the functions stored in the list x in order to 
## determine, if the inverse of the matrix has already been computed. 
## If yes, it is taken from the cache, 
## if not, it will be computed and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
