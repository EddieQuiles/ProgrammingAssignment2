## The first function, makeCacheMatrix, creates a list containing 
## functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

## Utilize the above functions in order to create a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    i <-  NULL
    set <- function(m){
        x <<- m
        i <- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

## This second function, cacheSolve, computes the inverse of a special
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated and the matrix has not been changed, then cacheSolve 
## will retrieve the inverse from the chache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
