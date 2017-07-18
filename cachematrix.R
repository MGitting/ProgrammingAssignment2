#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ##makeCacheMatrix is a function contains 4 subfunctions in it: set,get,setinv,getinv
  
  #First one is "set" function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Second one is "get" function
  get <- function() x   #let "get" to be "x". put whatever in "x" to "get". i.e.  get = x
  
  
  #Third one is "setinv" function
  setinv <- function(inv) m <<- inv   #setinv is a function, its argument is "inv". 
  #giving setinv an argument "10", at the same time, 10 will be passed to variable m. 
  
  
  #Last one is "getinv" function
  getinv <- function() m    #getinv = m
  
  
  #put these 4 variables in a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## check the matrix in above function. exist: retreive; not exist: calculate
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  # to see whether the value have already exist
  if(!is.null(m)) {
    message("getting cached data")
    return(m)    #if "return" do execute, then the rest of the code won't be executed
  }
  
  #if not exist, calculate the mean and put it into setmean (which pass it to variable m)
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
