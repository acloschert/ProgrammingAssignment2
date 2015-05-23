## This is a pair of functions that allow a user to cache the inverse of a matrix 
## and call up that cached inverse if the matrix has not changed since the first computation.

## To use these functions to find the inverse of a matrix do the following after calling the functions:
## 1. set 'x <- makeCacheMatrix()' in order to...
## 2. set the matrix to be evaluated: 'x$set(matrix)'
## 3. execute the second function to find the inverse (or call it from cache): 'cacheSolve(x)'

makeCacheMatrix <- function(x = matrix()) {
      ## This function defines four functions: 'set', 'get', 'setinv', and 'getinv'. 
      ## It builds them and then outputs a list with each function in it named. 
      ##See purpise of list below.
      inv <- NULL ## Sets 'inv' to value 'NULL'.
      set <- function(y) {    ## This defines 'set' as a function with variable 'y' that -- when executed -- 
            x <<- y           ## has 'x' as 'y' globally and 'inv' as 'NULL' globally.
            inv <<- NULL 
      } 
      get <- function() x ## 'get' will report back the input matrix when called from 'x$get()' (see above).
      setinv <- function(solve) inv <<- solve ## finds the inverse of the matrix when called from 
                                              ## 'cacheSolve()'
      getinv <- function() inv ## Retrieves the inverse from cache when called from 'cacheSolve()'
      list(set = set, get = get, ## This is listing out 'set', 'get', 'setinv', and 'getinv', 
           setinv = setinv,      ## and naming the outputs with those names at the same time.
           getinv = getinv)      ## This allows the user to call 'x$set()' (see above).
}

## 'cacheSolve()' returns a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
      inv <- x$getinv() ## looks in the cache for data toput into 'inv'
      if(!is.null(inv)) { ## If the 'inv' is not NULL...
            message("getting cached data") ## show this message, and...
            return(inv) ## return 'inv'
      }
      data <- x$get() ## Define 'data' as the input matrix.
      inv <- solve(data, ...) ## Defone 'inv' as the inverse of 'data' (the input matrix).
      x$setinv(inv) ## Finds the inverse of the input matrix.
      inv ## Reports the inverse of the matrix.
}
