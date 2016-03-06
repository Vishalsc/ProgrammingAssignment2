## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## @x: a square invertible matrix.
## This function creates a special "matrix" object that can cache its inverse.
## This list is used as the input to cacheSolve().


makeCacheMatrix <- function(x = matrix())
  { 
  
  inv <- NULL         # Initialized as NULL
  set <- function(y)  
    
  {
    x <<- y           # use `<<-` to assign a value to an object in an environment 
    inv <<- NULL      # different from the current environment.
    
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

  }

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
  {
  inv<- x$getinv()
  
  if (!is.null(inv))   # if the inverse has already been calculated get it from the cache 
  {                    #and skips the computation.
  message("getting cached data")
  return(inv)
  }
  
  data<- x$get()       # otherwise, calculates the inverse
  inv<- solve(data, ...)
  x$setinv(inv)        # sets the value of the inverse in the cache via the setinv function.
  inv                  # Return a matrix that is the inverse of 'x'
  
  }
