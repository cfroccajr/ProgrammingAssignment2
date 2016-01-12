## This script consits of two functions: 
## The first will create a list using a matrix.  This list will keep track of 
## the matrix and its inverse.
## The second retrieves the inverse of the matrix in the object created by 
## the first function, calculating it if necessary.

## makeCacheMatrix returns a list of four functions:
## set <- sets the value of the matrix x and resets inv_x to matrix()
## set_inv <- sets the value of inv_x to the inverse of x
## get <- retrieves the value of x
## get_inv <- retrievs the value of x_inv

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- matrix()
        
        set <- function(y){
                x <<- y
                inv_x <<- matrix()
        }
        
        set_inv <- function() inv_x <<- solve(x)
        
        get <- function() x
        
        get_inv <- function() inv_x
        
        list(set = set, 
             set_inv = set_inv,
             get = get,
             get_inv = get_inv)
}


## cacheSolve takes in a makeCacheMatrix list x and returns the inverse.
## If the inverse has already been calculated then it returns the cached version.
## If it has not been cached then it sets the inverse and caches it.

cacheSolve <- function(x, ...) {

        inv <- x$get_inv()
        
        if(!identical(x$get_inv(),matrix())){
                message("Here is the cached inverse:")
                return(inv)
        }
        x$set_inv()
        inv <- x$get_inv()
        message("Here is the un-cached inverse:")
        inv
        
}
