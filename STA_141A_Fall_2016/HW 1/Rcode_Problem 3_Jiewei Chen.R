# STA 141 HW 1 
# Problem 3

# Write a function that takes a sequence (vector) of 0 or 1 as input 
#       and returns the starting location of all subsequences 
#       that start and end with prespecified motifs (vectors of 0’s and 1’s).

# function name - Loc
# Three arguements - X is the input vector, which contains 0's or 1's
#                - S is the starting motif
#                - E is the ending motif
# Output is a data frame containing the starting/ending locations and  
#       the length of all subsequences that start and end with 
#       prespecified motifs.
#       First column is the starting points.
#       Second column is the starting points.
#       Third column is the length of subsequence.
#       If no run is found, return 0 as the location.

Loc <- function(X, S, E) {
        # if starting or ending motif is longer than the input sequence
        #       return an error message
        if ( min(length(S), length(E)) > length(X) ) {
                stop("The starting or ending motif should be smaller or as equal to the input vector")
        }
        # Initializing the output
        location = data.frame("Start Loc" = 0, "End Loc" = 0, "Length" = 0)
        # Initializing a vector that contains all the starting locations of Starting Motif
        Loc_S = numeric(0)
        j = 0
        for (i in 1: (length(X)-length(S)+1) ) {
                if ( identical(X[i:(i+length(S)-1)],S) ) {
                        j = j + 1
                        Loc_S[j] = i
                }
        }
        
        # Initializing a vector that contains all the starting locations of Ending Motif
        Loc_E = numeric(0)
        l = 0
        for (i in 1: (length(X)-length(E)+1) ) {
                if ( identical(X[i:(i+length(E)-1)],E) ) {
                        l= l + 1
                        Loc_E[l] = i
                }
        }

        # Now we have all the starting locations where we can find the Starting or Ending Motif 
        ind = 1 # index of location
        for (i in 1:length(Loc_S) ) {
                for (j in 1: length(Loc_E)) {
                       if ( Loc_E[j] >= Loc_S[i] && ((Loc_E[j]+length(E)-1) >= (Loc_S[i]+length(S)-1)) ) {
                                location[ind,1] = Loc_S[i]
                                location[ind,2] = Loc_E[j]
                                location[ind,3] = Loc_E[j] + length(E) - Loc_S[i]
                                ind = ind + 1
                }
                 
                }
        }
        return(location)
}




