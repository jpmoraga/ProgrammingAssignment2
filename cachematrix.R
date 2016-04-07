

## This function set in cache the Matrix and its inverse of a given Matrix

makeCacheMatrix <- function(x = matrix()) 
	{
	inv <- NULL
        set <- function(y) ## Here is define the Matrix 
        	{
        	x <<- y
        	inv <<- NULL
	 	}
    	get <- function() x ## Here is return the Matrix
    	setinverse <- function(inverse) inv <<- inverse ## Here is define the Inverse 
    	getinverse <- function() inv ## Here is return the Inverse
    	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	}


## This function returns the inverse of the Matrix. First look in cache and if is not there is calculate

cacheSolve <- function(x, ...) 
	{
    	inv <- x$getinverse() ## Here is call the inverse from the cache
    	if(!is.null(inv)) ## Here is return the inverse if it exist
    		{
        	message("getting cached data.")
        	return(inv)
    		}
    	data <- x$get() ## Here is return the Matrix
    	inv <- solve(data)
    	x$setinverse(inv) ## Here is return the Inverse
    	inv        
## Return a matrix that is the inverse of 'x'
	}
