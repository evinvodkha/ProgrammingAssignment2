## This function creates a special matrix object that can cache
## its inverse
## It returns a list containing a function to :
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv_cache <- NULL         # initialize the cache to NULL
  
  set<-function(y){         # enable to change the value of the matrix
    x <<- y 
    inv_cache  <<- NULL     # the matrix being changed, the cache is set to NULL
  }
  
  get <- function() x       # retrieves the matrix value
  
  setInv <- function(inv) inv_cache <<-inv  # change the inverse value
  
  getInv <- function() inv_cache            # retrieves the inverse value
  
  list(set=set,get=get,setInv=setInv,getInv=getInv) # returns a list of functions 
  
}


## Calculate the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        
  inv<-x$getInv()            # retrieves the cache value
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)             # if there is a value in the cache, returns it and exit the function
  }
  data<-x$get()             # otherwise, get the matrix value
  inv<-solve(data,...)      # calculates the inverse
  x$setInv(inv)             # saves the inverse in cache
  inv                       # returns the inverse
}
