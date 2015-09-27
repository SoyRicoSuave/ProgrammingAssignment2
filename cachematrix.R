## These functions are used to create a list that stores a matrix and its inverse
## By doing this, the inverse will only need to be calculated once for the matrix
## all subsequent calls will return the stored value, therefore saving calculation
## time.

##function which creates a list that can store (cache) the inverse of a matrix
## @x- matrix
makeCacheMatrix <- function(x = matrix()) 
{
    ## m is the pointer to the cached inverse
    ## when created, will set the pointer to NULL as no inverse has been calculated
    m <- NULL
    ##used to change the saved matrix in x to a new matrix
    ##also will reset the chached inverse by setting the pointer to NULL
    set <- function (y) 
    {
        x <<- y
        m <<- NULL
    }
    ##function which returns the matrix x
    get <- function() x
    ##used to change the m pointer to the input value m_inverse
    ## therefore, storing the inverse of the matrix 
    setinverse <- function(m_inverse) 
    {
        m <<- m_inverse
    }
    ##function which returns the inverse, or NULL if m is not set
    getinverse <- function()
    {
        m
    }
    ##initiate the list which allows for calling of the created fucntions
    list(set = set, get = get, setinverse =setinverse, getinverse = getinverse)
}


##function which returns the inverse of a cached matrix
##if the inverse is not cached, it will solve and store it
## @x- list created from makeCacheMatrix
## @return- returns the inverse of the matrix
cacheSolve <- function(x, ...) 
{
    m <- x$getinverse() ##sets m to the value of the cached inverse
    if(!is.null(m)) ##if m is not a NULL pointer
    {
        message("getting cached data")
        return(m) ##returns the cached inverse
    }
    #otherwise...
    data <- x$get() ##sets "data" to the matrix stored in makeCacheMatrix
    m<-solve(data, ...) ##takes the inverse of the matrix and stores it in m
    x$setinverse(m) ##caches the now solved inverse into x
    return(m)
}
