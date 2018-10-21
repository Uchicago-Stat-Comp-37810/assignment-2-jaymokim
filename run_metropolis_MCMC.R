# Metropolis algorithm
run_metropolis_MCMC <- function(startvalue, iterations){
    chain = array(dim = c(iterations+1,3))    # an empty chain
    chain[1,] = startvalue    # filling the first row with the startvalue of parameters
    for (i in 1:iterations){    # for loop, iterating "interations" times
        proposal = proposalfunction(chain[i,])    # "random walk" of starting values
        
        probab = exp(posterior(proposal) - posterior(chain[i,]))    # defining probability
        if (runif(1) < probab){
            chain[i+1,] = proposal    # if accepted, the current chain is moved to proposal chain
        }else{
            chain[i+1,] = chain[i,]    # if not accepted, the current chain stays at the previous chain
        }
    }
    return(chain)    # returning the chain with number of iteration + 1 sets of values including the starting value
}
