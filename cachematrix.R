## makeCacheMatrix creates a list of functions: set the matrix, get the matrix, set inverse of 
## the matrix, and gets inverse of the matrix 
## cacheSolve computes the inverse of the matrix created with makeCacheMatrix. It first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it computes the inverse and sets the inverse in the cache via the setinverse function.

## makeCacheMatrix defines functions set, get, setinverse, and getinverse 
## and returns a list of these functions. These functions are to set the matrix,
## get the matrix, set the inverse of the matrix and get the inverse of the matrix 
 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL  
        set <- function(y) {
                x <<- y  
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m 
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve tries to get the inverse of the matrix. If the inverse exists in cache, get it from cache. 
## If the matrix is not in cache, call function get to get the matrix. Then it computes the inverse 
## by calling solve and  then it sets the inverse.  

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()  
        if(!is.null(m)) {   
                message("getting cached data")
                return(m)
        }
        data <- x$get()  
        m <- solve(data, ...) 
        x$setinverse(m)   
        m  #return matrix m
}

