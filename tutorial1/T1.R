##########################################################
# 2.1 Read data into R #
##########################################################

#set the working directory, where did you save the "lowbwt.txt"
setwd("C:\\Users\\CZ\\Desktop\\tutorial")

workdat <- read.table("lowbwt.txt", header=T)

fix(workdat)

x <- workdat$gestage

y <- workdat$sbp

#first thing
plot(x,y, main="", ylab="blood pressure", xlab="age")

n <- length(x)

#############################################################
# 2.2 Fitting a Simple Linear Regression Model using Direct Calculation #
#############################################################
##########################
# Calculate LSE of b0 and b1 #
##########################
b1 <- (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*(mean(x))^2)

b0 <- mean(y)-b1*mean(x)

#plot the fitted line
abline(a=b0, b=b1)

yhat <- b0+b1*x

points(x,yhat,cex=1,col=2,pch=19)

resid <- y-yhat

##########################
# Check Residuals #
##########################

sum(resid)

sum(resid*x)

sum(resid*yhat)

##########################
# Estimate sigma^2 #
##########################
s2 <- sum(resid^2)/(n-2)

##########################
#Test H_0: b1=0 #
##########################
sxx <- sum(x^2) - n*(mean(x))^2
se.b1 <- sqrt(s2/sxx)

tstats <- b1/se.b1
tstats

pvalue <- 2*(1-pt(tstats, n-2))
pvalue

##########################
#Analysis of Variance #
##########################
SST <- var(y)*(n-1)
SST
SSE <- sum(resid^2)
SSE
SSR <- sum((yhat-mean(y))^2)
SSR
SSE+SSR
Fstats <- (SSR/1)/(SSE/(n-2))
Fstats

tstats^2
#############################################################
# 2.3 Fitting a Simple Linear Regression Model using lm( ) function #
#############################################################

fm <- lm( sbp ~ gestage, data = workdat )
summary(fm)

#############################################################
# 2.4 Confidence Intervals and Prediction #
#############################################################

##############################
# 95% CI for b1 #
##############################
1.2644+qt(0.025, n-2)*0.4362

1.2644-qt(0.025, n-2)*0.4362

#############################################################
# Suppose we randomly select a new child from the population of low birth #
# weight infant with gestational age 31 weeks. #
#############################################################
##############################
# Predicted sbp for this child #
##############################
beta.fit <- coef(fm)
yhat.p <- beta.fit[1] + beta.fit[2]*31

##############################
# 95% CI for Prediction #
##############################
sigma2hat <- deviance(fm)/(n-2)

se.yhat.p <- sqrt( (1+1/n+(31-mean(x))^2/sxx)*sigma2hat)

yhat.p+ c(1, -1)*qt(0.025, n-2)*se.yhat.p


##########################
# Other things #
##########################

#a contour plot
library(MASS)
x <- rnorm(1000)
y <- rnorm(1000)
par(mfrow=c(1,2))
plot(x,y)
contour(dd <- kde2d(x,y))

#the difference between beta and beta_hat
#repeat the following code multiple times, will you get the same b0 and b1 estimate each time?
x <- 1:10
y <- 20+0.5*x+rnorm(length(x))
plot(x, y)
n <- length(x)
b1 <- (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*(mean(x))^2)
b0 <- mean(y)-b1*mean(x)
abline(a=b0, b=b1)

