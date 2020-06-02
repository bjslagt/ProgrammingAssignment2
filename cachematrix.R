## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## COMMENT STARTS -- BJSLAGT --
## this creates a special matrix, holding a cached version of the inverse matrix
## If the cache hasn't been set yet, a getinverse will give NULL
## COMMENT ENDS -- BJSLAGT
makeCacheMatrix <- function(x = matrix()) {

  #cached inverse to NULL
  inv <- NULL
  
  set <- function(y){
    #if we set a new matrix, we need to NULL the cache
    x <<- y
    inv <<- NULL
  }

  #get function to retrieve matrix
  get <- function() x
  
  #set function to set the inverse matrix
  setinverse <- function(i) inv<<- i
  
  #get function to get the inverse matrix
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
  
  #get the cached inverse (if existing)
  inv <- x$getinverse()
  
  #if it didn't exist the inverse was NULL
  if(!is.null(inv)){
    #print("Return CACHED version")
    #removed the print statement to not provide unneccesary output on the console
    
    return(inv)
  }
  
  #get original matrix
  data<-x$get()
  
  #calculate inverse using solve()
  inv<-solve(data, ...)
  
  #then set the cache
  x$setinverse(inv)
  
  #and return the inverse
  inv
}