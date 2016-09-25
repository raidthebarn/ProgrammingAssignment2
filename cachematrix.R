## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        ## X = invertible matrix
        ## 1.  set the value of the matrix
        ## 2.  get the value of the matrix
        ## 3.  set the inverse value of the matrix
        ## 4.  get the inverse value of the matrix    
        
        inv = NULL
        set = function(y) {
                # assign a value to an object in an environment different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## X = output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # retrieve X from cache if the inverse has already been calculated
        if (!is.null(inv)){
                message("retrieving cached data")
                return(inv)
        }
        
        # If not cached, calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # set inverse value 
        x$setinv(inv)
        
        return(inv)
}
