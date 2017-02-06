# STA 141 HW 1 
# Problem 2

# Write a function that takes a sequence (vector) of 0 or 1 as input
#       returns the starting locations of runs of 0’s and runs of 1’s,
#       where the length of a run is set to an integer K ≥ 1

# function name - Loc_0and1
# Two arguements - X is the input vector, which contains 0's or 1's
#                - K is the length of a run
# Output is a list of length two
#       First element is a vector states all the starting locations of 0's
#       Second element is a vector states all the starting locations of 1's
#       If no run is found, return 0 as the location.

Loc_0and1 <- function(X, K) {
        
        if (sum(X %in% c(0,1)) != length(X)) {
                stop("X can only contain either 0 or 1!")
        }
        if (K > length(X)) {
                stop("K must be smaller or equal to the length of input vector!")
        }
        
        # Initializing a list that can store the result
        location = list("Location of Runs of 0's" = 0, "Location of Runs of 1's" = 0)
         
        j = 0
        for (i in 1: (length(X)-K+1) ) {
                if (X[i] == 0) {
                        check = ( X[i:(i+K-1)] == 0)
                        if (sum(check) == K) {
                                j = j + 1
                                location[[1]][j] = i
                                }
                }
        }
        
        l = 0
        for (i in 1: (length(X)-K+1) ) {
                if (X[i] == 1) {
                        check = ( X[i:(i+K-1)] == 1)
                        if (sum(check) == K) {
                                l = l + 1
                                location[[2]][l] = i
                        }
                }
        }
       
        return(location)
}