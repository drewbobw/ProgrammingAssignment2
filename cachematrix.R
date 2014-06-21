# Function creates a c style class that allows the user
# to cache the inverse of a matrix to avoid the process
# of having to recaclute the inverse repeatedly.

## makeCacheMatrix creates a "class" that allows the user
## to interact with the function cachceSolve (below).
## Input: None to the main function; Set subfunction takes a square 
## matrix. setinverse takes a matrix as well.
##Output: Main fucntion retrns an object with 4 
## subfunctions (get, set, setinverse, getinverse). Get returns 
## the original matrix passed to set. getinverse returns inverse

makeCacheMatrix <- function(x = matrix()) 
{
        #invmat -- inverse matrix
        #mat -- base matrix
        invmat <-NULL;
        mat <- NULL;
        #=================================
#         expects temp to be a square matrix. 
#         Will accept non-square matricies 
#         but they'll cause problems elsewhere 
#         sets value of mat to input unless no changes. 
#         mat will be returned in get()
        set <- function(temp)
        {
#               First round of error checking                        
                 if (identical(temp, mat)) {
                         message("Matrix already enterd");
                         return();
                   }
           mat<<- temp;
           invmat<<-NULL;
        }
        #==================================
        #other accessor functions
        get <-function() {return(mat);}
        setinverse <- function(solve) {invmat <<- solve;}
        getinverse <- function() {return(invmat);}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse);

}

# cacheSolve expects an object inpt of the makeCacheMatrix type
# It will take the object and calculate the inverse of the
# matrix (stored by calling "set"). A better function name
# might be returnCachedSolved. Prior to calculating the inverse
# it checks to see if a value is already stored in inpt. If so 
# function returns the cached value. If function cannot invert the 
# matrix it sets the value of invmat to null and returns a warning
#
#
# Input: object of class makeCacheMatrix. Optional inputs for the sovlve 
# function passed via ...
# Output: If an inverted matrix already exist, returns that matrix. Otherwise
# it outputs 

cacheSolve <- function(inpt, ...) 
{
        if (class(inpt)!="list")
        {
                warning("Parameter mismatch. cacheSolve expects input of type 
                        makeCacheMatrix");
                return();       
        }
        
        solved <- inpt$getinverse();
        #=================================================
                #solved matrix already exists, returned cached value
                if (!is.null(solved))
                {
                        message("Returning Cached Value");
                        return(inpt$getinverse());    
                }
        #=================================================
        #matrix has not been solved, solve and return
        tosolve<-inpt$get();
        solved <- try(solve(tosolve, ...), silent=TRUE)
        #==============================================================
        #Attempt to solve matrix has thrown an error. Return error description
        if (class(solved)=="try-error")
        {       #attempting to invert the matrix has thrown an error
                if (grepl("square", solved, fixed=TRUE))
                 {      warning("Non-square matrix passed. Non-invertable");
                        inpt$setinverse(NULL);
                       
                 }
                else if (grepl("singular", solved, fixed=TRUE))
                {
                        warning("Singular matrix passed. Non-invertable");
                        inpt$setinverse(NULL);
                         
                }
                else
                {
                        warning("Non-invertable matrix");
                        inpt$setinverrse(NULL);
                           
                }
        }
        #==============================================================
        else if (class(solved)=="matrix")
        {
                #Matrix successfully inverted. Set values and exit function
                inpt$setinv(solved);
                #solved;
                message("Matrix inverted");
                
        }
        #==============================================================
        else
        {
                #something weird happend
                message("Something weird happend. No values set.");
                return (); 
        }
        #==============================================================
}
#EOF