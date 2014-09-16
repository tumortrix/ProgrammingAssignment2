##Function below will be used as a source list of functions
##it creates a list of 4 functions that will be used in other function

makeCacheMatrix <- function(x = matrix()) { 
        
## m will be used to store the inverse matrix (result)        
        m <- NULL 
## this is used to check if the matrix is different from previously calculated
## if it's different, then it will 'drop' the stored inverse matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

##this is a 'blank' function, used to return the input matrix (initial)
        get <- function() x 
##This takes the calculated inverse matrix and assigns to 'global' m variable
        setInv <- function(Inv) m <<- Inv 
## 'blank' function that returns the value of m (inverse matrix)
        getInv <- function() m
##Finally, it creates a list with 4 functions, which will be called in the
##large function below.
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## Second function evaluates if the inverse matrix has been solved
## to retrieve that value. If not, then it will calculate the matrix.

cacheSolve <- function(x, ...) {
## executes the function getInv, defined above. In this case it will assign
## the 'global' variable m to the 'local' variable m
## if it has not been calculated, then it will return NULL.
        
        m <- x$getInv() 
        
##Then it checks if 'm' IS NOT NULL (if the inverse already exists)
        
        if(!is.null(m)) { 
                
##If m is not null (inverse exists), it returns the stored matrix
                
        message("getting cached data") 
        return(m) 
        }

##If m was null, it will assign the input matrix to variable 'data' 
        data <- x$get() 

##Then it calculates the inverse matrix of 'data' (which is the input)
## and assigns the result to variable 'm'
        m <- solve(data, ...) 

##Next, it will assign the calculated inverse matrix to the
## 'global' variable m, so next time we call this function, 
## the variable m won't be NULL
        x$setInv(m) 

##Finally, we return the inverse matrix, which is stored in m
        m 
}
