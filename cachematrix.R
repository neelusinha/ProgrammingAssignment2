## Programming Assignment 2
## Caching the Inverse of a Matrix
## Two functions - makeCacheMatrix and cacheSolve

## makeCacheMatrix function creates a speical matrix object which can cache
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {

    inv<-NULL # set the inv to NULL when creating a cache matrix
    set <- function(y) {
        x<<-y # use the special assignment operator to assign in any env
        inv<<-NULL
    }
    get <- function() x # returns the matrix
    setinv<-function(inverse) inv<<- inverse # sets the matrix inverse
    getinv<-function() inv # returns the matrix inverse
    list(set=set, get=get, setinv=setinv, getinv=getinv) # list of 4 functions
}


## cacheSolve function computes the inverse of the "special" matrix returned
## from makeCacheMatrix function. 
## If inverse exists then this function  retrieves it and returns. 
## If inverse does not exist then this function uses solve to compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv() # get the inverse of matrix x
    if(!is.null(inv)) { # inverse already exists in cache
        message("getting cached data")
        return(inv)
    }
    # inverse does not exist - need to compute it
    data<-x$get() # get the matrix x
    inv<-solve(data, ...) # use solve to compute inverse of data
    x$setinv(inv) # set the inverse
    inv
}
