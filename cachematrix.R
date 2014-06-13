## Programming Assignment 2
## Caching the Inverse of a Matrix
## Two functions - makeCacheMatrix and cacheSolve

## makeCacheMatrix function creates a speical matrix object which can cache
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {

    inv<-NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv<-function(inverse) inv<<- inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the "special" matrix returned
## from makeCacheMatrix function. 
## If inverse exists then this function  retrieves it and returns. 
## If inverse does not exist then this function uses solve to compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
