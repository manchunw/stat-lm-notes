setwd("/Users/YEYING/Desktop/STAT331/tutorial")

workdat <- read.table("lowbwt2.txt", header=T)

# dimension of workdat shown
dim(workdat)

head(workdat)

# extract only column 3, 4, 6 and all the data into X
X <- workdat[,c(3,4,6)]
dim(X)
X <- as.matrix(cbind(1, X))
dim(X)

n <- nrow(X)
p <- ncol(X)-1
n
p

Y <- workdat$headcirc

Ybar <- mean(Y)

#############################################################
# Multiple Linear Regression Model using matrix operations
#############################################################

#############################################################
# LSE
beta <- solve(t(X) %*% X) %*% t(X) %*% Y

beta
#############################################################
# Hat matrix
H <- X %*% solve(t(X) %*% X) %*% t(X)

# Idempotent
max(H %*% H - H)

# symmetric
max(t(H) - H)

Yhat <- H %*% Y

#############################################################
# ANOVA

SSE <- as.numeric(t(Y-Yhat) %*% (Y-Yhat))
SSR <- as.numeric(t(beta) %*% t(X) %*% Y) -n*Ybar^2
SST <- sum((Y-Ybar)^2)

SSE+SSR
SST

#############################################################
# Estimate sigma^2

sigma2 <- as.numeric(SSE/(n-p-1)) 
sigma <- sqrt(sigma2)

#############################################################
# Estimate V(beta)

sigma2 * solve(t(X) %*% X)

#############################################################
# Fit a Multiple Linear Regression Model using lm() function
#############################################################

fm <- lm(headcirc~gestage+birthwt+toxemia, data=workdat)
tmp <- summary(fm)
tmp

#alternative of lm: only show estimate of each beta i
coef(fm)

##############################
# 95% CI for b1 

tmp$coefficients

tmp$coefficients[2,2]

coef(fm)[2] + c(-1,1)*qt(0.975,n-p-1)*tmp$coefficients[2,2]
coef(fm)[2] + c(-1,1)*1.96*tmp$coefficients[2,2]

#############################################################
# Estimate V(beta)

tmp$cov.unscaled      
tmp$sigma^2 * tmp$cov.unscaled 

#############################################################
# for loop
#############################################################

# Recall SLR one time
x <- 1:10
n <- length(x)

y <- 20+0.5*x+rnorm(length(x))
b1 <- (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*(mean(x))^2)
b0 <- mean(y)-b1*mean(x)

#############################################################
# simple for loops

for(i in letters){
    cat(i)
}

for (i in 1:10){
	print(i)
}

# SLR multiple times
niter <- 500

beta1 <- rep(0, niter)

for(i in 1:niter){
    y <- 20+0.5*x+rnorm(n)
    beta1[i] <- (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*(mean(x))^2)
}

summary(beta1)
