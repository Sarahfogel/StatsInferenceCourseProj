#===========================Simulation==========================================
# Simulate the sum of 40 random exponentials with lambda=0.2, 1000 times
#This uses a for loop, could do it with apply faster but I don't have time to learn that right now

    set.seed(59)    #Make this reproducible by setting the starting point
    
    N<-10000
    
    sums<-c()
    
    for (i in 1:N) {
        p<-rpois(40, lambda=.2)
        sums[i]<-sum(p)
    }

#==========================Question 1===========================================

    hist(sums)
