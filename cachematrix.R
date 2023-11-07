##Overall, the code gets a matrix as input and stores it in a special list.
##The inverse of the given matrix would be stored as cache data.

##If the data(Matrix) is identical as before, the function uses the cache data 
##instead of going through computations again

## This function's output is a list which contains 4 functions to 
## 1- Set the value of the given matrix
## 2- Get the value of the given matrix
## 3- Set the inverse of the matrix
## 4- Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverted_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(matrix) inverted_matrix <<- matrix
    get_inverse <- function() inverted_matrix
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Checks if there is a cache data. If not, computes the inverse of the matrix

cacheSolve <- function(x , ...) {
    inverted_matrix <- x$get_inverse()
    if(!is.null(inverted_matrix)) {
        message("Getting cached data!")
        return(inverted_matrix)
    }
    data <- x$get()
    inverted_matrix <- solve(data , ...)
    x$set_inverse(inverted_matrix)
    inverted_matrix
}

## Examples of 3 invertible matrices

ex1 <- makeCacheMatrix(matrix(c(2,2,3,2) , 2 , 2))
ex2 <- makeCacheMatrix(matrix(c(2,2,7,8) , 2 , 2))
ex3 <- makeCacheMatrix(matrix(c(1,-2,2,-4,1,6,2,3,8) , 3 , 3))

cacheSolve(ex1)
