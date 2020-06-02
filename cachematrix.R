## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## COMMENT STARTS -- BJSLAGT --
## this creates a special matrix, holding a cached version of the inverse matrix
## If the cache hasn't been set yet, a getinverse will give NULL
## COMMENT ENDS -- BJSLAGT
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv<<- i
  getinverse <- function() inv
  list(set = set, get = get , setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## COMMENT START -- BJSLAGT --
## Function checks if the cache of the inverse matrix exists,
## if so it retrieves the cache, otherwise it calculates it and stores
## it in the cache
## COMMENT END -- BJSLAGT

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    return(inv)
  }
  data<-x$get()
  inv<-solve(x)
  x$setinverse(inv)
  inv
}
