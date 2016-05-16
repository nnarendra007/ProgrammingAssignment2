# The functions help in calculating the inverse of the matrix. The calculated inverse
# is stored in cache and is not calculated again in case it needs to be retreived.

# makeCacheMatrix(x = matrix())
# This function builds a special list containing the functions to build a matrix,
# access the matrix, set its inverse and get its inverse
# Arguments: x <- Matrix which is needed to build the special list

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve <- function(x, ...) 
# The function calculates the inverse of the matrix.
# In case the inverse is calculated, it takes it from the cache.
# Else, it will calculate and return the result

cacheSolve <- function(x, ...) 
{
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
