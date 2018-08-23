## cache the inverse of a matrix
##  Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(matrix){  ## set the matrix
                x<<- matrix
                i <<- NULL
        }
        get <- function(){  ## get the matrix
                x
        }
        setinverse <- function(inverse){ ## set the inverse of the matrix
                i <<-inverse
        }
        getinverse <- function(){ ## get the inverse of the matrix
                i
        }
        list(set= set, get=get, setinverse= setinverse, getinverse=getinverse)
                } ## return a list of the methods


## Compute the inverse of the special matrix returned by "makeCacheMatrix"above. If the inverse has already been calculated (and the matrix has notchanged), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()## return a matrix that is the inverse of 'x'
        if(!is.null(m)){   
                message("getting cached data")
                return(m) ## return the inverse if its already set
        }
        data <- x$get() ##get the matrix from our object
        m <- solve(data) ## calculate the inverse using matrix multiplication
        x$setinverse(m) ## set the inverse to the object
        m## return a matrix that is the inverse of 'x'
}
