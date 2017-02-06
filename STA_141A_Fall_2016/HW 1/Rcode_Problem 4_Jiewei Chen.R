# STA 141 HW 1 
# Problem 4

# Markov Chain a
#       π00 = 0.5 π01 = 0.5
#       π10 = 0.5 π11 = 0.5

Chain_A = Markov(10000, 0.5, 0.5)

# Markov Chain b
#       π00 = 0.8 π01 = 0.2
#       π10 = 0.1 π11 = 0.9

Chain_B = Markov(10000, 0.8, 0.1)

# ========================
# 4a
# For Chain a, the starting location of runs of 0’s and 1’s of lengths up to 10
A = sapply( 1:10, function(i) {Loc_0and1(Chain_A, i)} )
# Answer is a list which contains the result of the starting location of runs 
#       of 0’s and 1’s of lengths up to 10. 
# To better present the result, I created a matrix that contains the length of 
#       the vectors of the starting location.The i(th) row represents the number of 
#       starting locations can be found in the runs of 0's or 1's with length i.
Start.Loc.0 = rep(0, 10)
Start.Loc.1 = rep(0, 10)
for (i in 1:10) {
        Start.Loc.0[i] = length( A[,i][[1]] )
        Start.Loc.1[i] = length( A[,i][[2]] )
        Result_A = cbind(Start.Loc.0, Start.Loc.1)
}
Result_A
# Answer
#       Start.Loc.0 Start.Loc.1
#  [1,]       5063        4937
#  [2,]       2578        2452
#  [3,]       1301        1200
#  [4,]        646         585
#  [5,]        338         280
#  [6,]        176         131
#  [7,]         92          62
#  [8,]         51          30
#  [9,]         28          16
#  [10,]        16           8


# For Chain b, the starting location of runs of 0’s and 1’s of lengths up to 10
B = sapply( 1:10, function(i) {Loc_0and1(Chain_B, i)} )
# Answer is a list which contains the result of the starting location of runs 
#       of 0’s and 1’s of lengths up to 10. 
# To better present the result, I created a matrix that contains the length of 
#       the vectors of the starting location. The i(th) row represents the number of 
#       starting locations can be found in the runs of 0's or 1's with length i.
Start.Loc.0 = rep(0, 10)
Start.Loc.1 = rep(0, 10)
for (i in 1:10) {
        Start.Loc.0[i] = length( B[,i][[1]] )
        Start.Loc.1[i] = length( B[,i][[2]] )
        Result_B = cbind(Start.Loc.0, Start.Loc.1)
}
Result_B
# Answer
#       Start.Loc.0 Start.Loc.1
#  [1,]        3287        6713
#  [2,]        2624        6050
#  [3,]        2096        5446
#  [4,]        1674        4916
#  [5,]        1349        4440
#  [6,]        1085        4005
#  [7,]         875        3606
#  [8,]         700        3248
#  [9,]         557        2928
#  [10,]        448        2626


# Yes. The patterns between runs of 0’s and runs of 1’s in Chain A and B are quite 
#       different. 
#       First, for both of them, as the length of the run getting longer, the possible 
#       starting locations we can find in Markov chain becomes less. Because the chance
#       of finding longer consecutive 0 or 1 becomes less.
#       Secondly, by comparing the two matrix I got. It can be observed that for 
#       Chain A, the number of runs of 0's and 1's are much smaller than those of 
#       Chain B. For example, for Chain A, the number of finding continueing 0's and 1's
#       are 16, and 8, respectively. For Chain B,the number of finding ten continueing 
#       0's and 1's are 448, and 2626, respectively.

# ========================
# 4b
# For Chain A
Loc_n (Chain_A, c(0,0,0,0), c(1,1,1,1), 200) 
#       for more detail about Function Loc_n, please refer to the code below.
length(Loc_n (Chain_A, c(0,0,0,0), c(1,1,1,1), 200))
# The answer is a vector with length 43.
head((Loc_n (Chain_A, c(0,0,0,0), c(1,1,1,1), 200)))
# [1]  421  958 1262 1489 1818 1918
tail(Loc_n (Chain_A, c(0,0,0,0), c(1,1,1,1), 200))
# [1] 8693 8807 9450 9462 9692 9790

# For Chain B
Loc_n (Chain_B, c(0,0,0,0), c(1,1,1,1), 200)
length(Loc_n (Chain_B, c(0,0,0,0), c(1,1,1,1), 200))
# The answer is a vector with length 814.
head((Loc_n (Chain_B, c(0,0,0,0), c(1,1,1,1), 200)))
# [1] 10 11 12 13 14 15
tail(Loc_n (Chain_B, c(0,0,0,0), c(1,1,1,1), 200))
# [1] 9657 9658 9659 9660 9661 9685

# -------------------------
## Code for function Loc_n
# This function that takes a sequence (vector) of 0 or 1 as input and 
#       returns the starting location of all subsequences with specified length
#       that start and end with prespecified motifs (vectors of 0’s and 1’s).

# function name - Loc_n
# Two arguements - X is the input vector, which contains 0's or 1's
#                - S is the starting motif
#                - E is the ending motif
#                - n is length of the subsequence required
# Output is a numeric vector containing the starting location of 
#       all subsequences that start and end with prespecified motifs
#       If no run is found, return 0 as the location.

# Loc_n <- function(X, S, E, n) {
#        # if starting or ending motif is longer than the input sequence
#        #       return an error message
#        if ( min(length(S), length(E)) > length(X) ) {
#                stop("The starting or ending motif should be smaller or as equal to the input vector")
#        }
#        
#        location = numeric(0)
#        j = 0
#        # subsetting the vector with length n
#        for (i in 1: (length(X)-n+1) ) {
#                # subvector with length n
#                Y = X[i:(i+n-1)]
#                # subsetting the start motif
#                W = Y[1:length(S)]
#                # subsetting the last motif
#                Z = Y[(n-length(E)+1):n]
#                check1 = ( W == S )
#                check2 = ( Z == E )
#                if (sum(check1) == length(S) && sum(check2) == length(E)) {
#                        j = j + 1
#                        location[j] = i
#                }
#        }
#        return(sort(location))
# }
# -------------------------


# ========================
# 4c
# It can be observed that for Chain A, the number of runs of 0's and 1's are 
#       much smaller than those of Chain B. For Chain A, the number of finding
#       ten continueing 0's and 1's are 16, and 8, respectively. For Chain B,  
#       the number of finding ten continueing 0's and 1's are 448, and 2626, 
#       respectively. I think this difference is due to the difference in the 
#       probabilities of these two Markov Chains. In Chain A, the probability 
#       of getting 1 or 0 is the same, no matter what the previous number is.
#       So it behaves like random sequence, of which the probability of getting 
#       1's and 0's is the same at a given position. However, for Chain B, the 
#       probabilities of getting 1's and 0's are dependent on the previous number
#       in the sequence. According to the transition probability matrices of B, 
#       there is much higher chance to get 1 right after 1, or get 0 right after 0.
#       This is why we could find much more consecutively 0's or 1's from Chain B.
# 













