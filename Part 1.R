# Creating Test Data
trueA <- 5    # slope
trueB <- 0    # intercept
trueSd <- 10    # true sd
sampleSize <- 31    # sample size

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)    # x-values with a size of a sample size
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)    # y-values according to the model

plot(x,y, main="Test Data")    # A scatterplot of the created x-values and dependent values according to the model.

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

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}    # a function that takes x and returns the sum of likelihoods with slope = x
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )    # a sum of likelihoods for different values of slope
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")    # plot of the likelihood profile of the slope a

# Prior distribution
prior <- function(param){
    a = param[1]
    b = param[2]
    sd = param[3]
    aprior = dunif(a, min=0, max=10, log = T)
    bprior = dnorm(b, sd = 5, log = T)
    sdprior = dunif(sd, min=0, max=30, log = T)
    return(aprior+bprior+sdprior)
}    # defining the prior distribution

# Posterior distribution
posterior <- function(param){
    return (likelihood(param) + prior(param))
}    # defining the posterior distribution

# Metropolis algorithm
proposalfunction <- function(param){
    return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}    # generating a random value for each parameters within a reasonable range

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
    return(chain)    # returning the chain with 1001 sets of values including the starting value
}

startvalue = c(4,0,10)    # setting the start value
chain = run_metropolis_MCMC(startvalue, 10000)    # getting the chain by starting with the startvalue and iterating for 10000 times

burnIn = 5000    # number of burning iterations
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))    # Accepance rate

# Summary
summaryfunc <- function(chain, burnIn, trueA, trueB, trueSd) {
    par(mfrow = c(2,3))    # dividing the plotting screen to 2 rows and 3 columns
    hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )    # histogram of posterior of a (slope)
    abline(v = mean(chain[-(1:burnIn),1]))    # making a line indicating the mean value of a
    abline(v = trueA, col="red" )    # making a red line indicating the true value of a
    hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")    # histogram of posterior of b (intercept)
    abline(v = mean(chain[-(1:burnIn),2]))    # making a line indicating the mean value of b
    abline(v = trueB, col="red" )    # making a red line indicating the true value of b
    hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")    # histogram of posterior of sd
    abline(v = mean(chain[-(1:burnIn),3]) )    # making a line indicating the mean value of sd
    abline(v = trueSd, col="red" )    # making a red line indicating the true value of sd
    plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )    # plot showing the chain values of a (slope)
    abline(h = trueA, col="red" )    # making a red line indicating the true value of a
    plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )    # plot showing the chain values of b (intercept)
    abline(h = trueB, col="red" )    # making a red line indicating the true value of b
    plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )    # plot showing the chain values of sd
    abline(h = trueSd, col="red" )    # making a red line indicating the true value of sd
}

summaryfunc(chain = chain, burnIn = burnIn, trueA = trueA, trueB = trueB, trueSd = trueSd)

# for comparison:
summary(lm(y~x))    # comparing the methodf of MCMC with the method of linear regression
