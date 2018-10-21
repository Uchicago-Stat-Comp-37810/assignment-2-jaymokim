source("likelihood.R")
source("slopevalues.R")
source("prior.R")
source("posterior.R")
source("proposalfunction.R")
source("run_metropolis_MCMC.R")
source("summaryfunc.R")

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

# Example: plot the likelihood profile of the slope a
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )    # a sum of likelihoods for different values of slope
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")    # plot of the likelihood profile of the slope a

startvalue = c(4,0,10)    # setting the start value
chain = run_metropolis_MCMC(startvalue, 10000)    # getting the chain by starting with the startvalue and iterating for 10000 times

burnIn = 5000    # number of burning iterations
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))    # Accepance rate

summaryfunc(chain = chain, burnIn = burnIn, trueA = trueA, trueB = trueB, trueSd = trueSd)

# for comparison:
summary(lm(y~x))    # comparing the methodf of MCMC with the method of linear regression

# compare_outcomes function
compare_outcomes <- function(n) {
    alpha = runif(10, min=0, max=10)
    beta = rnorm(10, sd = 5)
    sigma = runif(10, min=0, max=30)
    for(i in 1:10) {
        ch <- run_metropolis_MCMC(startvalue = c(alpha[i], beta[i], sigma[i]), iterations = n)
        print(c(mean(ch[, 1]), sd(ch[, 1])))
    }
}
