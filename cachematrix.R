## Creates(Returns) a special 'matrix' object that can cache it's own inverse
## which is a list of methods to:
##    1. set(y): Sets the matrix equal to 'y'
##    2. get(): Gets(Returns) the matrix itself
##    3. setInv(mat): Sets the inverse of the matrix equal to 'mat'
##    4. getInv(): Gets(Returns) the (cached)inverse of the matrix(if not set
##    returns NULL)


makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y){
        x <<- y
        inv_mat <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(mat){
        inv_mat <<- mat
    }
    getInv <- function(){
        inv_mat
    }
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Returns the inverse of the special 'matrix' created by makeCacheMatrix
##      -- If the matrix has not been modified:
##          It returns the inverse from cache
##      -- If the matrix has been modified:
##          It computes the inverse again and returns it

cacheSolve <- function(x, ...) {
    inv_mat <- x$getInv()
    if(!is.null(inv_mat)){
        message("getting cached data")
        return(inv_mat)
    }
    orig_mat <- x$get()
    inv_mat <- solve(orig_mat,...)
    x$setInv(inv_mat)
    inv_mat
}
