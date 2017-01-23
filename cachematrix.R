## The code in this script can be used to compute the inverse of a square invertible matrix
## and cache the inverse, so that if needed it can be used multiple times
## with no need to compute it again


## The makeCacheMatrix() function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
     x<<-y
     i<<-NULL
    }
    get<-function() x
    setinv<-function(inv) i<<-inv
    getinv<-function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverse of a matrix from the cached value 
## that is stored in the makeCacheMatrix() object's environment.
## If no inverse is cached, it does compute the inverse itself and cache it

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i<-x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data)
    x$setinv(i)
    i
}
