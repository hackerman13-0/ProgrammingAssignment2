## Put comments here that give an overall description of what your
## functions do

## The First Function is the Cache matrix function and with this
## We are making a matrix to get its inverse, assuming that its invertable always 
## This function i have modeled it similar to the MakeVector in the 
## Readme markdown file.

makeCacheMatrix <- function(x = matrix()) {

        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)

}


## The CacheSolve function is used to compute the inverse matrix 
## Or if in the case of the matrix being obtained from above is already 
## Inversed, then we just return the matrix as Is

cacheSolve <- function(x, ...) {
          j <- x$getInverse()
        
        if(!is.null(j)){
                
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
        
}
