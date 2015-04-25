##computes the inverse of a matrix and cache it

## makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        
        ##inver is the inverse of the matrix you defined,that is,'x'
        
        ##set:  set the value of the original matrix
        ##get:  return the defined matrix 'x'
        ##setmatrix:    set the value of the inverse of 'x'
        ##getmatrix:    return the value of the inverse of 'x'
        
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse_matrix) inver <<- inverse_matrix
        getmatrix <- function() inver
        
        ##This function returns a list of four functions
        
        f_list <- list(set = set, get = get,
                        setmatrix = setmatrix,
                        getmatrix = getmatrix)
        return(f_list)
}


##cacheSolve: This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above. 

##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        
        ##'x' is a list of four functions
        ## Return a matrix that is the inverse of 'x'
        
        inver <- x$getmatrix()
        
        ##if inver already cantains a value that's not null
        ##then there's no need to calculate
        
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
        ##calculate the inverse of the matrix
        
        data <- x$get()
        inver <- solve(data)
        x$setmatrix(inver)
        return(inver)
}