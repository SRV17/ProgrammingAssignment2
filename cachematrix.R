## the makecachematric takes matrix x as an argument 
## the function set and get would set and get the matrix . 
##The function set_inverse and get_inverse would set and get the inverse of the matrix 



makeCacheMatrix <- function (x = matrix()){
    mat <- NULL
    set <- function(a) {
      x <<- a 
      mat <<- NULL
    }
    get <- function() { x }
    set_inverse <- function(inverse) { mat <<- inverse}
    get_inverse  <- function() { mat }

    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)## this would be an input to cachesolve function
}

cacheSolve <- function (x, ...){ ##here x would be the o/p of makecahcematrix
  mat <- x$get_inverse() ##inverse of the matrix which is input to makecachematrix
  if(!is.null(mat)){ ## if the inverse is already computed get from the cache data
    message("Pulling Cached data")
    return(mat)
  }
  mat_data <- x$get() ## calculates if it is not in cache
  mat <- solve(mat_data,...)
  x$set_inverse(mat)
  return(mat)
}

