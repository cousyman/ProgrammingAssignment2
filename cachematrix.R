## These are a pair of functions that cache the inverse of a matrix
## The discussion forums were very helpful in finding material on this
## project.


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize variables 
  m <- NULL
  
  #Set the data
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Retrieve the data
  get <- function() x
  
  #Set the inverse
  setinv <- function(solve) m <<- solve
  
  #Retrieve the inverse
  getinv <- function() m
  
  #Compile the functions into a list for easy retrieval
  list(set=set,get=get,setinv = setinv, getinv = getinv)

}


## This function computes the inverse, and stores it. If the inverse has already
## been cached, then it will report, getting cached data.

cacheSolve <- function(x, ...) {
  ##Check to see if the inverse has aready been cached.
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Retrieve the data.
  data <- x$get()
  
  #Solve for the inverse of the matrix and store the answer.
  m <- solve(data,...)
  
  #Set the inverse of the matrix.
  x$setinv(m)
  
  #Report the index of the matrix.
  m
}
