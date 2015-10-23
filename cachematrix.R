## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function create an "object" ("super" matrix) containing
## the value of the matrix and its inverse.
## There are 4 functions :

## 1. set       : set the value of the matrix
## 2. get       : get the value of the matrix
## 3. setinv    : set the value of the inverse
## 4. getinv    : get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## This function returns the value of the inverse of the "super" matrix created
## with the previous function.
## For this exercise, we suppose that the matrix is always invertible.

## The function will check if the value of the inverse has already been calculated.

## No   : calculate the inverse, stock it in the "super matrix" and return it.
## Yes  : print a message "getting cached data" and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
