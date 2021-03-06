# The following function 'makeCacheMatrix' can be used
# to create an object that stores a matrix.
# In addition, it creates an object method 'inv()' that can compute 
# the inverse of the matrix, which must be invertible, and store it 
# inside the object (cache). Subsequent calls to the inv() method will 
# retrieve the inverse matrix from the cache unless a new matrix has 
# been stored in the object since the last call to inv(). 

# Note that the function 'cacheSolve' is inside 
# the 'makeCacheMatrix' function.

## ---- cache inverse matrix function ----

makeCacheMatrix <-  function(){
        # initialize local variables
        X = matrix()
        invX <- NULL
        # store new matrix 
        # (and discard previous, including inverse)
        newX <- function(y){
                X <<- y
                invX <<- NULL
        }
        setInvX <- function() invX <<- solve(X)
        # return inverse of matrix X, from cache if possible
        cacheSolve <- function() {
                if(!is.null(invX)){
                        message("retrieving inverse matrix from cache")
                        return(invX)
                }
                setInvX()
                invX
        }
        # auxiliary functions
        getX <- function() X
        getInvX <- function() invX
        clearCache <- function() invX <<- NULL
        # return list of object methods
        list(newX = newX, getX = getX, setInvX = setInvX,
             getInvX = getInvX, clearCache = clearCache,
             inv = cacheSolve)
}

## ---- check correct functionality ----

ma <- makeCacheMatrix()
set.seed(33)
sm <- matrix(sample.int(100, 25, replace=T), 5, 5)
ma$newX(sm)
ma$getX()
ma$getInvX()
ma$inv()
ma$inv()
sm <- matrix(sample.int(100, 25, replace=F), 5, 5)
ma$newX(sm)
ma$getX()
ma$inv()
ma$inv()
ma$clearCache()
ma$inv()
