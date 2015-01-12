## These functions cache and compute inverse matrix.
## (So you don't waste time on computing inverse matrix again if it has been computed)

## This function makes an object(list), which is convenient
## for caching current matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix<-NULL
  
  set<-function(new_matrix){  
    x<<-new_matrix
    inverse_matrix<<-NULL
  }
  
  get<-function() x
  
  set_inv<-function(new_inverse_matrix)
    inverse_matrix<<-new_inverse_matrix
  
  get_inv<-function() inverse_matrix
  
  list(set = set, get = get, set_inverse = set_inv, get_inverse = get_inv)
}


## This function retrieves the inverse matrix 
## from the cache (if it has been computed earlier)
## otherwise computes inverse matrix and retrives it

cacheSolve <- function(x, ...) {
  inverse_matrix<-x$get_inverse()
  if (!is.null(inverse_matrix))
    return(inverse_matrix)
  x$set_inverse(solve(x$get(), ...))
  return(x$get_inverse())
}