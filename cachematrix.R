## Two functions that together invert a matrix and cache its inverse for quick future access.

## M an inverstible square matrix. 
## Set vM <- makeCacheMatrix(M).
## The inverse of M is computed as cacheSolve(vM).  
## After the inital call cacheSolve(vM), all subsequent calls cacheSolve(vM) will load the already computed inverse, running much faster.
## A message will output to indicate that the cached version of the inverse is being used.


## makeCacheMatrix returns a list of four functions which will do the actual work of inverting M.
## Once the inverse is computed, its value is stored in the scope of makeCacheMatrix for quick future access

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(I){
    x <<- I
    inv <<- NULL
  }
  get <-function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve calls the functions output by makeCacheMatrix, first computing the inverse to M, then storing that inverse.
## If the inverse has already been computed and stored, it is simply returned without computation.

cacheSolve <- function(x, ...) {
  inv <-x$getinv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
