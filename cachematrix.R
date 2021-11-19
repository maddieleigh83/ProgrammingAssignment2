## comments in the function

## This makes a chaeable matrix which can then be inputted into the other 
## function, cachesolve() which will set then get the cached values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #this initializes the inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  #this is a function to get matrix x
  setInv <- function(inverse) {inv <<- inverse}
  getInv <- function() {inv} #this func is to obtain inverse of the matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## below is the chachesolve() function which is for getting and setting the 
## inv. matrix value

cachesolve <- function(x, ...) {  #this gets the cached data
  inv <- x$getInv()
  if(!is.null(inv)) {  #checks if the inverse is NULL
    message("getting cached data")
    return(inv)  #returns the inverse of the value
  }
  dat <- x$get()
  inv <- solve(dat, ...)  #this calculates the inv value
  x$setInv(inv)
  inv  # Return a matrix that is the inverse of 'x'
}
