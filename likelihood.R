# Deriving the likelihood funcion from the model
likelihood <- function(param){
    a = param[1]
    b = param[2]
    sd = param[3]
    
    pred = a*x + b
    singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
    sumll = sum(singlelikelihoods)
    return(sumll)   
}    # defining the likelihood function
