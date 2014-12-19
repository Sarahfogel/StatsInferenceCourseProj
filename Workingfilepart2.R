#========================Load data==============================================
library(datasets)
data(ToothGrowth)

#=======================Question 1==============================================
#Load the ToothGrowth data and perform some basic exploratory data analyses 
dim(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)


par(mfrow=c(1,2))
hist(ToothGrowth$len)
hist(ToothGrowth$dose)
table(ToothGrowth$supp)
par(mfrow=c(1,1))

library(lattice)
bwplot(ToothGrowth$len~ToothGrowth$supp)
bwplot(ToothGrowth$len~as.factor(ToothGrowth$dose))


.#=========================Question 2=============================================
#Provide a basic summary of the data.

#This data set shows the length of teeth in each of 10 guinea pigs at each of 
#  three dose levels of Vitamin C with each of two delivery methods.

summary(ToothGrowth)

#=============================Question 3========================================
#Use confidence intervals and/or hypothesis tests to compare tooth growth by supp
#and dose. (Only use the techniques from class, even if there's other approaches 
#worth considering)

Tooth.split.supp<-split(ToothGrowth$len, f=list(ToothGrowth$supp))
Tooth.split.dose<-split(ToothGrowth$len, f=list(ToothGrowth$dose))
Tooth.split.both<-split(ToothGrowth$len, f=list(ToothGrowth$supp, ToothGrowth$dose))

#Since it is unclear which guinea pig is which in this data, we cannot use a paired ttest
t.test(Tooth.split.supp$OJ, Tooth.split.supp$VC)
#Output shows that the 95% CI for the difference between the means contains 0, 
#  so we cannot be sure the two groups are different

t.test(Tooth.split.dose$`0.5`, Tooth.split.dose$`1`)
t.test(Tooth.split.dose$`1`, Tooth.split.dose$`2`)
t.test(Tooth.split.dose$`0.5`, Tooth.split.dose$`2`)

#=====================Question 4=================================================
#State your conclusions and the assumptions needed for your conclusions. 

#t intervals assume that data are iid normal, though it works well whenever the 
#distribution is roughly symmetric and mound shaped - which the length variable is

