## Basically there are two functions included within this R file.
## Whenever loops are being implemented in R to perform some computations such as
## inverse of a matrix, then in such cases, R might take some amount of 
## computational time particularly if those matrix are of greater size, thus 
## implementation of a "Cache" to store the computed results can be really 
## efficient and save us lot of time, if the input matrix remains unchanged, 
## thereby each time we need to perform inverse on the same input matrix, then 
## we can simply check in the cache for the result instead of performing the 
## whole computation all over again, thereby making the loop or process faster.



## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Here,we write a function 'cacheSolve' to calculate the inverse of the input
## matrix. Before computing the inverse(considering we input an invertable matrix)
## the function checks if the inverse has already been computed and if its cached.
## If yes, then the function provides the inverse matrix, thereby skipping the
## computation. Otherwise, it calculates the inverse of the input matrix and stores
## the inverse in the cache using the 'setinverse' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
