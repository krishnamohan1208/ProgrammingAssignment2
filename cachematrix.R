## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Matrix inverse computations take longer than expected. The intent
## of the following functions is to use cache to hold the inverse of
## matrix in the first run and return the result from cache when 
## the function is called again

## makeCacheMatrix function gets the matrix (x)

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Write a short comment describing this function
## the following function returns inv which holds the inverse
## with the help of setinverse function. before the execution
## of this function it checks whether the inverse has been calculated 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'

 inv <- x$getinverse()
    if(!is.null(inv)) {
        message("The result is fetched from Cache")
        return(inv)
    }
else
{
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
}
