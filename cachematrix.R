## Put comments here that give an overall description of what your
## functions do

## The first function is used to get the special object that stores a matrix, which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## This function is to get the inverse of the special matrix set by the function
## above. It first to check if the inverse has already be calculated. If so, 
## it gets the inversefrom the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setinver function.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
