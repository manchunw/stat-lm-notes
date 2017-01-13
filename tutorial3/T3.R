# where is your house.txt?
setwd("/Users/YEYING/Desktop/STAT331/tutorial")

# you need to install car package first by typying:
# install.packages("car")
library(car)

hp <- read.table("house.txt", header=T)

head(hp)

dim(hp)

hp <- hp[,c("Price", "Size", "New", "Taxes")]

#############################################################
# Look at data
plot(hp$Size, hp$Price, xlab="Size", ylab="Price")

# all the graph
pairs(hp)

# with trend lines
scatterplotMatrix(hp)

#############################################################
# Linear model
fit<-lm(Price~Size+New+Taxes, data=hp)

summary(fit)

plot(fitted(fit), rstudent(fit), xlab="fitted", ylab="studentized residual")

#############################################################
# Transformation
#############################################################

library(MASS)
boxcox(fit)

fit2 <- lm(sqrt(Price)~Size+New+Taxes, data=hp)

summary(fit2)

#############################################################
# Model diagnostics
#############################################################

#############################################################
# Residual plots
plot(hp$Size, rstudent(fit2), xlab="Size", ylab="residuals")

plot(hp$Taxes, rstudent(fit2), xlab="Taxes", ylab="residuals")

plot(fitted(fit2), rstudent(fit2), xlab="fitted", ylab="residuals")

#############################################################
# Residual plots with case number
plot(fitted(fit2), rstudent(fit2), type="n", xlab="fitted", ylab="studentized residual")
text(fitted(fit2), rstudent(fit2))
abline(h=c(-2, 2), lty=2)
abline(h=c(-3, 3), lty=1, col="red")

#############################################################
# QQ plot
qqnorm(rstudent(fit2))
qqline(rstudent(fit2))
#
plot(fit2, which=2)
#
qqPlot(fit2)

#############################################################
# Added variable plot
avPlots(fit2)

#############################################################
# An interactive function that plots the case number for leverage
hat.plot <- function(fit) {
	p <- length(coefficients(fit))
	n <- length(fitted(fit))
	plot(hatvalues(fit), main="Index Plot of Hat Values")
	abline(h=c(2,3)*p/n, col="red", lty=2)
	identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit2)

#############################################################
# Cook's D
plot(fit2, which=4)

plot(fit2, which=5)
