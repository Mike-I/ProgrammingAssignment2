### Put comments here that give an overall description of what your
### functions do


## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Function caches inverse matrix of a given one.

### Write a short comment describing this function

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

## setting default input of function to be of a matrix format
makeCacheMatrix <- function(x = matrix()) { 
  ## variable in which the inverse matrix will be stored
  m <- NULL                             
  ## function that: if the matrix is new - resets m to NULL, else - retrives inverse from the cache
    set <- function(y) {                
    x <<- y                             
    m <<- NULL                        
    }
  ## function that returns value of the matrix argument  
    get <- function() x                     
  ## function that sets value of m in parent environment
    set_inverse <- function(inverse) m <<- inverse  
  ## function that returns the value of m
    get_inverse <- function() m                     
  ## set names to call the functions with $
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)  
}

### Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  m_get <- x$get()
  # if m_get is a square invertible matrix, then solve(data) returns its inverse
  m <- solve(m_get)
  x$set_inverse(m)
  m      
}
