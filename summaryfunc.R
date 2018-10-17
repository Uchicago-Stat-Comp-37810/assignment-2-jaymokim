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

