## Put comments here that give an overall description of what your functions do

#This routine solve the inverse of a matrix, storing the results and the matrix inside the object assigned (factory design)

## Write a short comment describing this function
#makeCacheMatrix is a function to store the matrix and the inverse to be used to another function. it does NOT calculate anything - as the name of the function says.

makeCacheMatrix <- function(X = matrix()) {
    X_inv<-NULL  #initialize in actual enviroment the variable which stores inverse matrix
    set<-function(new_X){  # function to store a new matrix and reset inv
        X <<- new_X
        X_inv <<- NULL
    }
    get<-function(){ #Function to get matrix
        return(X)
    }  
    set_inv<-function(res){ #Function to store the result in inv
        X_inv<<-res
    }  
    get_inv<-function() { #Function to get inverse
        return(X_inv)
    }  
    list(get=get,set=set,getinv=get_inv,setinv=set_inv)
}


## Write a short comment describing this function
# cacheSolve calculates and stores the inverse matrix if the matrix created by makeCacheMatrix has never been calculated. Else, shows the stored value calculated anytime before.

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X' if already calculated. If not, calculate and store
    X_inv <- X$getinv()  #get X_inv from makeCacheMatrix func
    if (!is.null(X_inv)) {  #if inv is already solved then return the cached value in makeCacheMatrix func
        message('Return cached data')
        return(X_inv)
    }
    else{  #if it's not solved
        X_inv <- solve(X$get(),...)  # assign X_inv
        X$setinv(X_inv)  #Store the inverse inside makeCacheMatrix function
        return(X_inv)  #return inverse
    }
}
