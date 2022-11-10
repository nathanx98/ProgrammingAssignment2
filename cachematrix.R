## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set, get, setinv, getinv
##Library(MASS) is used to calculate inverse for non squared as well as square matrices
library(Mass)
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                  ## initializing inv as NULL
    set <- function(y) {                   
        x <<- y                                     
        inv <<- NULL                        
	}
    get <- function() x           ## function to get matrix x
	setinverse <- function(inverse) inv <<- inverse      		     	getinverse <- function() { 
		inver <-ginv(x) 
		inver%*%x       ##function to obtain inverse of the matrix
	}                    
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) ## get cache data
{
        
    inv <- x$getinverse()
    if(!is.null(inv)) {		## checking whether inverse is Null
        message("getting cached data")
        return(inv) ##returns inverse value
    }
    data <- x$get()
    inv <- solve(data, ...) ## calculates inverse value
    x$setinverse(inv)
    inv
}
f<-makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$get(inv)
cacheSolve(f)
