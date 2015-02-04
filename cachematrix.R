## This set of functions contain two different function. 
## The first function creates a special "matrix" object 
## that can cache its inverse. The second function computes
## the inverse of the special "matrix" returned by the first 
## function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## This function creates a special "matrix" object that 
## can cache its inverse.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
                         ## define m to be NULL
                             m <- NULL 
                         ## Define a function set. Give the value y to x. Reset m to be NULL
                           set <- function(y) {  
                                      x <<- y
                                      m <<- NULL
                                  }
                         ## Define a function get. Return the value of x.
                                   get <- function() {
                                           x
                                          }
                         ## Define a function setinverse. Calculate the inverse matrix of x and
                         ## give the value to m.
                            setinverse <- function(solve) {
                                            m <<- solve
                                          }
                         ## Define a function getinverse. Get the value of m.
                            getinverse <- function() {
                                            m
                                          }
                         ## Define a list contains the 4 functions.
                                list(set = set, get = get,
                              setinverse = setinverse,
                              getinverse = getinverse)
                           
                    }


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
                        ## Get the getinverse function from the list.
                      m <- x$getinverse()
                        ## Test whether the value is NULL. If no, print out m.
                     if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                     }
                     ## If m is empty, calculate the inverse matrix of the input.
                          data <- x$get()
                             m <- solve(data, ...)
                             x$setinverse(m)
                             m
                     
                     ## Return a matrix that is the inverse of 'x'
                       
}
