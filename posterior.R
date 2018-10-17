# Posterior distribution
posterior <- function(param){
    return (likelihood(param) + prior(param))
}    # defining the posterior distribution
