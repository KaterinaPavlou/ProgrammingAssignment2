## The following functions are used to create a matrix that can
## be saved in the environment's "cache" along with its inverse
## and retrieve a new or retrieve an existing matrix 

## makeCacheMatrix takes as an input a matrix 
## and constructs a new object that can save the matrix and its
## inverse in the environment's cache
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        #reset the matrix and its inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #retrieve original matrix
        get <- function() x
        
        #save the inverse of the matrix in i cache
        setInverse <- function(inverse) i <<- inverse
        
        #retrieve the inverse of the matrix saved in cache
        getInverse <- function() i
        
        #return the new object of this special matrix as a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve takes as an input a matrix and if its inverse exists
## in cache it will return it else it will compute it, save it to cache
## for the next time that is needed and return the computed inverse.
## If the input matrix cannot be inverted this will throw an error
cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() #attempt to get the inverse from cache
        
        #if the inverse is in cache return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #else compute the inverse of the special matrix
        message("adding data to cache")
        matrixToBeInverted <- x$get()
        inv <- solve(matrixToBeInverted, ...)
        
        #save it to the cache
        x$setInverse(inv)
        
        #output it
        inv
}
