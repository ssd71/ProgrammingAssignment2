## Creates(Returns) a special 'matrix' object that can cache it's own 
## inverse, which is a list of methods to:
##    1. set(y): Sets the matrix equal to 'y'
##    2. get(): Gets(Returns) the matrix itself
##    3. setInv(mat): Sets the inverse of the matrix equal to 'mat'
##    4. getInv(): Gets(Returns) the (cached)inverse of the matrix(if not set
##    returns NULL)


makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    
    # setter function for the matrix itself
    set <- function(y){
        x <<- y
        inv_mat <<- NULL
    }
    
    # This is a getter function for the matrix itself
    get <- function(){
        x
    }
    
    # setter function for the inverse of the matrix
    setInv <- function(mat){
        inv_mat <<- mat
    }
    
    # getter function for the inverse of matrix
    getInv <- function(){
        inv_mat
    }
    
    # return a list of interface functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Special function for getting the inverse of a special `matrix` created
## by the makeCacheMatrix function, that also caches the calculated inverse
## and uses said cached inverse matrix when present
## It's general behaviour is as follows:
## Returns the inverse of the special 'matrix' created by makeCacheMatrix
##      -- If the matrix has not been modified:
##          It returns the inverse from cache
##      -- If the matrix has been modified:
##          It computes the inverse again and returns it

cacheSolve <- function(x, ...) {
    
    inv_mat <- x$getInv() # Fetches data(or a NULL value) from the cache
    
    
    # As the set(y) function from makeCacheMatrix sets the
    # cached matrix to NULL when the matrix is modified, 
    # this condition is sufficient to confirm if the matrix 
    # has been modified
    
    
    if(!is.null(inv_mat)){ # checks to see if the cached matrix is null
        
        
        message("getting cached data")
        return(inv_mat) # returns the cached inverse
        
        
    }
    
    # The following code runs only if the cached matrix is NULL
    
    orig_mat <- x$get() # fetches the (modified) matrix
    
    inv_mat <- solve(orig_mat,...) # calculates the inverse of the matrix
    
    x$setInv(inv_mat) # sets the cached inverse matrix to the calculated
                      # inverse matrix
    
    inv_mat # returns calculated inverse
}
