#### Matrix inversion with caching

###A test matrix can be created by
###c<-1:2
###d<-2:1
###test<-rbind(c,d)
###the inverse is 1/3(c,d) with the ones being -1 instead of 1.

####Takes an argument x of matrix type and returns a list of 4 
####functions.
makeCacheMatrix <- function(x = matrix()) 
{
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

###Computes inverse matrix or returns a cached inverse.
cacheSolve <- function(x, ...) 
{       ###If there an inverse matrix in cache returns it
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ###Else, gets the matrix and computes its inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}
