## These two functions allows us to cache the inverse of a matrix and retrieve 
## it from the cache whenever necessary, without having to repeat the 
## computation of the inverse

## This function takes a matrix x as input a creates a list which 
## will be used to cache the inverse of the input matrix besides storing the 
## matrix itself 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes the special 'matrix' (actually a list) created with the 
## makeCacheMatrix as an input and checks if the inverse has already been
## calculated. If so, it prints the inverse. Otherwise, it will calculate the
## inverse and update the input list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
