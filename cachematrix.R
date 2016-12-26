## Put comments here that give an overall description of what your
## functions do

## this function saves the original matrix "x" and its inverse matrix "i". Other functions can retrieve the original 
## matrix x and i by using the function "set", "get", "setinverse" and "getinverse"

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) i<<-inverse
    getinverse<-function() i
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function caculate the inverse of matrix, if the inverse has been solved before, the cached copy of inverse of
## matrix will be retrieved and returned. Otherwise, a inverse of matrix is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data of inverse matrix")
        return(i)
    }
    data<-x$get()
    i<-solve(a = data, ...)
    x$setinverse(i)
    i
}

