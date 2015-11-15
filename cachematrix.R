# To validate:
# m <- makeCacheMatrix()
# m$set(matrix(c(0,2,2,0), 2, 2)) or an alternative invertible matrix up to the memory capacity of the host computer.
# m$get(m)
# cacheSolve(m)
# cacheSolve(m)
# In this syntax, the variable "m" can be any letter.
#
# See test results below source code.
#
# Helpful Sources : 
# Assignment examples
# Class Discussion Forum at https://class.coursera.org/rprog-004/forum/thread?thread_id=153

# Overall, these functions create variables, some of which are defined as functions,
# some as nested functions, and others as cached variables, and they use the functions 
# to move these data in and out of the cache environment and to perform operations on these data.

# makeCacheMatrix receives a matrix variable, and sets variables and functions in memory, 
# and returns a list of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        local_m <- NULL                                         ## Initialize the local m to NULL so we can tell when cacheSolve has run at least once.  
        set <- function(y) {                                    ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache.
                cache_x <<- y                                   ## Put the initial matrix from the command line into cache as cache_x
                cache_m <<- NULL                                ## Initialize caache_m to NULL so we can tell when cacheSolve has run at least once.          
        }
        get <- function() cache_x                               ## Create function to get/return the matrix passed in the command line call to '$set
        set_cache_m <- function(local_m) cache_m <<- local_m    ## Create function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m.        
        get_cache_m <- function() cache_m                       ## Create function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL
        list(set = set, get = get,
             set_cache_m = set_cache_m,
             get_cache_m = get_cache_m)
}

# cacheSolve function receives a variable that is a matrix that is expected to have been defined as makeCacheMatrix(),
# as in m <- makeCacheMatrix(), and then populated with an invertible matrix using the m$set() function that is nested 
# in makeCacheMatrix(). In this syntax, the variable "m" can be any letter. j See validation instruction, above.

# cacheSolve returns the inverted form of the submitted matrix.
 
# When cacheSolve is called, cacheSolve checks to see if there already exists a non-NULL value for m in cache. 

# If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.  

# If cacheSolve does not find an existing non-NULL value for m in cache, cacheSolve gets the commandline values for m, inverts the matrix 
# in m, and sets the value of m in the cache environment to the just-computed inverted matrix.

# cacheSolve then evaluates the ending matrix so as to return it.

cacheSolve <- function(x) {                     ## Receive makeCacheMatrix from the caller.
        local_m<- x$get_cache_m()               ## Get the value for m in the cache environment and put it in a local m.
        if(!is.null(local_m)) {                 ## Check to see if m is NULL.  
                message("getting cached data")  ## If m is not NULL, return the value of m with a message.
                return(local_m)
        }                                       ## If we get to this line, m was NULL
        startingmatrix <- x$get()               ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to startingmatrix.                         
        endingmatrix <- solve(startingmatrix)   ## Use solve() to invert the startingmatrix.  Assign the result to endingmatrix.
        x$set_cache_m(endingmatrix)             ## Call nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in endingmatrix
        endingmatrix                            ## Evaluate endingmatrix so as to return it to caller/console if cache_m is non NULL.
}