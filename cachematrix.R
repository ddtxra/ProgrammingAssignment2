## Solves the inverse matrix faster, using the cache if possible 

## Creates a special matrix, which is a list containing 4 differents functions to operate on the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {

  # in the beginning cached inverse is null
  cachedInv <- NULL
  
  # set value of the matrix
  set <- function(y) {
    # Assigns the value of the matrix
    x <<- y
    # matrix has changed, therefore inverse matrix is cleared
    cachedInv <<- NULL 
  }
  
  # get value of matrix
  get <- function() x
  
  # set the cached inverse matrix
  setinverse <- function(inverse) cachedInv <<- inverse
  
  # returns the cached inverse matrix
  getinverse <- function() cachedInv
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Check is the inverse matrix is in cache, if it is it returns the value in cache otherwise it computes it
cacheSolve <- function(x, ...) {
  
  # tries to get the inverse matrix
  inverseMatrix <- x$getinverse()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inverseMatrix)) {
    
    message("hit the cache yeah!")
    # Returns the inverse matrix in cache
    return(inverseMatrix)
  
  }else {

    message("computing the inversed matrix...")
    # if it is not in cache gets the matrix

    matrix <- x$get()
    
    # compute inverse of matrix
    inverseMatrix <- solve(matrix, ...)
    
    # puts the inverse matrix in cache
    x$setinverse(inverseMatrix)
    
    # returns the inverse matrix
    return(inverseMatrix)
    
  }
  
}


