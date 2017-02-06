# STA 141 HW 1 
# Problem 1

# Markov chain: Write a function to generate a sequence of random variables {Xi}
#       taking values 0 and 1, following the Markov Chain probability model

# function name - Markov
# Two arguements - n is the length of the sequence generated
#                - P00 is the probability of the case X(i+1) = 0 when Xi = 0
#                - P10 is the probability of the case X(i+1) = 0 when Xi = 1

# Output is a vector containing the Markov Chain

Markov <- function (n, P00, P10) {
        
        if (P00 < 0 || P00 > 1 || P10 < 0 || P10 > 1) {
                stop("Probability cannot be negative or larger than 1!")
        }
        
        if (n <= 0 || !identical(round(n), n) ) {
                stop("n must be a positive integer!")
        }
        
        P01 = 1 - P00
        P11 = 1 - P10
        M = rep(0, n)
        M[1] = sample(c(0,1), size = 1, prob = c(0.5, 0.5))
        
        for (i in 2:n) {
                
                if (M[i-1] == 1) {
                        M[i] = sample(c(0,1), replace =  TRUE, size = 1, prob = c(P10, P11))
                }
                else if (M[i-1] == 0) {
                        M[i] = sample(c(0,1), replace = TRUE, size = 1, prob = c(P00, P01))
                }
                        
        }
        
        return(M)
}
