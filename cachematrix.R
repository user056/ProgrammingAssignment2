##  This function will cache the inverse of a matrix in the parent environment

## This function creates a special "matrix" object that can cache its inverse.
## It creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        matrix.inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix.inverse <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(solve) matrix.inverse <<- solve
        getMatrixInverse <- function() matrix.inverse
        list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix.inverse <- x$getMatrixInverse()
        if(!is.null(matrix.inverse)) {
                message("reading from cache")
                return(matrix.inverse)
        }
        message("calculating the inverse")
        data <- x$get()
        matrix.inverse <- solve(data, ...)
        x$setMatrixInverse(matrix.inverse)
        matrix.inverse
}



# Main function for test cases
new_matrix_one <- makeCacheMatrix()
new_matrix_one$set(matrix(1:4, 2, 2))
new_matrix_one$get()
new_matrix_one$getMatrixInverse() # matrix inverse not calculated at this point. NULL is displayed.
cacheSolve(new_matrix_one) # calculating the inverse
cacheSolve(new_matrix_one) # reading from cache
cacheSolve(new_matrix_one) # reading from cache
new_matrix_one$getMatrixInverse()  # matrix inverse allready calculated at this point
cacheSolve(new_matrix_one)# reading from cache
cacheSolve(new_matrix_one)# reading from cache
