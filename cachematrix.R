## the makecache matrix takes matrix x as an argument which is a square invertible matrix .
## the function set would set the matrix, get would get the matrix returned. 
##the functions set_inverse and get_inverse would set and get the inverse of the matrix.



makeCacheMatrix <- function (x = matrix()){
    mat <- NULL
    set <- function(a) {
      x <<- a 
      mat <<- NULL
    }
    get <- function()
    {
    x }
    set_inverse <- function(inverse)
    { 
    mat <<- inverse
    }
    get_inverse  <- function(){
    mat }

    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) #this would be an input to solve
}



## Here x is o/p of makecachematrix which is an argument to cachesolve 

cacheSolve <- function (x, ...){
  mat <- x$get_inverse() ##this will get inverse of the input of makecachematrix
  if(!is.null(mat)){ ##this if inverse is already calculated get the cached data
    message("Pulling Cached data")
    return(mat)
  }
  mat_data <- x$get()## if there is no cache, calculate the inverse
  mat <- solve(mat_data,...)
  x$set_inverse(mat)
  return(mat)
}
