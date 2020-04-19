###The cachematrix.R file contains two functions, makecachematrix() and cachesolve().
#The first function in the file, makecachematrix() creates an R object that stores 
#a matrix and its inverse.The second function, cachesolve() requires an argument that is
#returned by makecachematrix() in order to retrieve the matrix inverse from the cached value
#that is stored in the makecachematrix() object's environment.


#The key concept to understand in makeCacheMatrix() is that it builds a set of functions 
#and returns the functions within a list to the parent environment. That is,
# a <- makeCacheMatrix(matrix(1:4, 2, 2))
#results in an object, a, that contains four functions: set(), get(), setinverse() and,
# getinverse() It also includes the two data objects, x and i

makeCacheMatrix <- function(x = matrix()) {
        if (ncol(x)==nrow(x) && det(x)!=0) {
                i <- NULL
                set <- function(y){
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) i <<- solve
                getinverse <- function() i
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                
        }else{
                return(message("The matrix is'n invertible."))
        }
        
}


## Without cacheSolve(), the makeCacheMatrix() function is incomplete. Why?
#As designed, cacheSolve() is required to populate or retrieve the matrix inverse 
#from an object of type makeVector().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse
        if (!is.null(i)) {
                message("getting cached data")
                i
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}

