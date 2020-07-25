## The two functions are used to invert a matrix and to cache the inverted
## matrix value so that the costly operation is not repeated whenever cacheSolve
## is called with the same matrix

## This function creates a matrix wrapper object that is checked whether the
## matrix being passed to cacheSolve has already been inverted.
## It uses a flag inverted which is used to track whether the matrix has been
## inverted already

makeCacheMatrix <- function(x = matrix()) {
        inverted = FALSE
        set <- function(m){
                x<<-m
                inverted <<- FALSE
        }
        get <- function() x
        invert_matrix <- function(inverted_matrix){
                x<<-inverted_matrix #change the value of the matrix to the inverted one
                inverted <<- TRUE
        }
        #Can get the inverted matrix using the same get function

        is_inverted <- function() inverted

        list(
                set = set,
                get = get,
                invert_matrix = invert_matrix,
                is_inverted = is_inverted
        )
}

## The function receives a matrix wrapped in a makeCacheMatrix object and checks
## whether is has already been inverted. If it is already inverted it returns the
## cached value, otherwise it inverts it and changes the inverted flag to TRUE

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(x$is_inverted()){
                message("Matrix was already inverted, returning cached value")
                return (x$get())
        }
        message("Inverting matrix for the first time")
        matrix = x$get()
        inv_matrix = solve(matrix)
        x$invert_matrix(inv_matrix) #Set inverted matrix value
        inv_matrix
}
