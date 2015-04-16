## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a data structure which stores a matrix and can store its inverted matrix in inv variable. 
## The data structure is a list and it has 4 methods:
## 1. set - it sets the matrix values and clears inv variable which stores inverted matrix
## 2. get - returns the matrix
## 3. setinv - sets inv variable to a given matrix (intentionally inverted matrix to the one stored in the structure)
## 4. getinv - returns the inv variable
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinv <- function(extinv) inv <<- extinv
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
      
}


## Write a short comment describing this function
## The cacheSolve function gets the inverted matrix from the given structure (constructed with makeCacheMatrix).
## Then it checks if the got value is NULL
## If it is not NULL it is returned
## Otherwise the inverted matrix to the given one is computed, it is saved in inv variable and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
