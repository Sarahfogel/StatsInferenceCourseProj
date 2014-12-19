#===========================Simulation==========================================
# Simulate the sum of 40 random exponentials with lambda=0.2, 1000 times

    set.seed(59)    #Make this reproducible by setting the starting point
    
    N<-1000         #Set number of simulations
    
    means<-c()      #Initialize a vector to hold the means of the samples of 40 variates
    
    for (i in 1:N) {
        p<-rexp(40, rate=.2)
        means[i]<-mean(p)
    }

#==========================Question 1===========================================
#Show where the distribution is centered at and compare it to the theoretical 
#  center of the distribution.


#The theoretical center of the distribution of sample means is the same as the 
#  center of the distribution that the sample comes from, so it's also 1/lambda=5

    theoretical.mean<-5

    hist(means, col="steelblue", main="Sample Means of 40 Exponential(0.2) Random Variates", prob=T)
    abline(v=mean(means), lty=1, lwd=3)
    abline(v=theoretical.mean, lty=2, col="red")
    legend(legend=c("Sample Mean", "Theoretical Mean"), x="topright", lty=c(1,2),
           lwd=c(3,1), col=c("black", "red"))

   theoretical.mean-mean(means)


#=========================Question 2 =============================================
#Show how variable it is and compare it to the theoretical variance of the distribution.


# The theoretical variance of the distribution of sample means is sigma^2/n, where sigma^2
#    is the variance of the distribution where the sample came from and n is the
#    number of observations in each sample.  In this case that is 1/(n*lambda^2).
#    Therefore, the theoretical standard deviation of the distribution of sample means is
#    1/(sqrt(40)*.2)


    theoretical.sd<-1/(sqrt(40)*.2)


    hist(means, col="steelblue", main="Sample Means of 40 Exponential(0.2) Random Variates", prob=T)
    abline(v=mean(means), lty=1, lwd=3)
    abline(v=theoretical.mean, lty=2, col="red")
    abline(v=mean(means)+sd(means), lty=1, lwd=3, col="purple4")
    abline(v=mean(means)-sd(means), lty=1, lwd=3, col="purple4")
    abline(v=theoretical.mean+theoretical.sd, lty=2, col="yellowgreen")
    abline(v=theoretical.mean-theoretical.sd, lty=2, col="yellowgreen")

    legend(legend=c("Sample Mean", "Sample Mean +/- Sample SD" ,"Theoretical Mean", 
                    "Theoretical Mean +/- Theoretical SD"), 
           x="topright", lty=c(1,1,2,2), lwd=c(3,3,1,1), col=c("black","purple4", "red", "yellowgreen"))


#==============================Question 3 ======================================
#Show that the distribution is approximately normal.

#Compare with a qq plot
    qqnorm(means)
    qqline(means)

#Plot the histogram with the actual kernal density estimate and the theoretical
# normal density
    hist(means, col="steelblue", main="Sample Means of 40 Exponential(0.2) Random Variates", prob=T)
    lines(density(means), lwd=3)
#Create normal density
    xfit<-seq(min(means), max(means), length = 40)
    yfit<-dnorm(xfit, mean=mean(means), sd=sd(means))
#Add that to plot
    lines(xfit, yfit, lty=2, col="red")

#Add legend
    legend(legend=c("Kernal Density Estimate", "Theoretical Normal Density"), 
           x="topright", lty=c(1,2), lwd=c(3,1), col=c("black", "red"))


