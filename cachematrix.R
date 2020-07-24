## makeCacheMatrix creates list of functions to manage values related to matrix
## set sets the matrix value, get gets the value, setinv sets the value of the
## inverse matrix, getinv gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inversion) inv<<- inversion
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## CacheSolve function takes a matrix as an argument and checks if the inverted
## matrix is already calculated
## In case it was done it just takes the calculated data
## Otherwise it computes it and sets it through setinv function

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}