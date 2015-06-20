## Cache matrix inverse results
##	matrix inverse operation can be lengthy 
##	save results of first request for future use
## 

## makecacheMatrix creates a special "matrix" which is really a list containing a function to
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the 
##    get the value of the mean


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<-solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the matrix inverse
## if available returns saved results
## otherwise calculates results using standard R function solve 

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m    
}
