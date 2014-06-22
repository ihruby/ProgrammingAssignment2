##  A matrix object that can cache its inverse once it is calculated 
##  and a matching inverse function that makes use of the functionality 

## Creates a CacheMatrix from a normal matrix
##
##  x <- c(1,2,3,4)
##  dim(x) <- c(2,2)
##  
##  > class(x)
##  [1] "matrix"
##  > y <- makeCacheMatrix(x)
##
##  get(x) return underlying value
##  set(x) sets a new underlying value (and clears cached calculation)
##
makeCacheMatrix <- function(x = matrix()) {
  inv_value <- NULL
  # set new value
  set <- function(y) {
    x <<- y
    inv_value <<- NULL
  }
  # return existing matrix 
  get <- function() x
  setinv <- function(inverse) {
    inv_value <<- inverse
  }
  
  # get cached value of inverse
  getinv <- function() { 
    inv_value
  }
  
  # function called by R to show what this object is
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Uses the functionality of CacheMatrix to return matrix inverse values 
## that are calculated only when the data is chaged.

## argument must be a cacheMatrix (as created with the makeCacheMatrix function)

cacheSolve <- function(x, ...) {
  inv_value <- x$getinv()
  if(!is.null(inv_value)) {
    message("getting cached data")
    return(inv_value)
  }
  data <- x$get()
  inv_value <- solve(data, ...)
  x$setinv(inv_value)
  inv_value
}
