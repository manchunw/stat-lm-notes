setwd("C:/Users/Kelvin/Documents/Homework/Coursework/yr3sem1/STAT 331")
copier = read.table("Copier.txt",header=T)
x = copier$Serviced
y = copier$Minutes
### (a)
plot(x,y, main="", xlab="Number of copiers serviced",
ylab="Minutes", pch=19,cex=1.4)
### (b)
n=length(x)
b1 = (sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*(mean(x))^2)
b0 = mean(y)-b1*mean(x)
print(paste("y=",round(b1, digits = 3),
"x",round(b0, digits = 3),sep=""))
### (c)
b1
### (d)
yhat = b0+b1*x
resid = y-yhat
s2 = sum(resid^2)/(n-2)
sxx = sum(x^2) - n*(mean(x))^2
se.b1 = sqrt(s2/sxx)
t_star = qt((1-(1-0.95)/2),df=n-2)
lower = round(b1-t_star*se.b1,digits=3)
upper = round(b1+t_star*se.b1,digits=3)
print(paste("(",lower,",",upper,")",sep=""))
### (e)
tstats = b1/se.b1
pvalue = 2*(1-pt(tstats, n-2))
pvalue ## p-value approach
tstats > t_star ## critical value approach
tstats
t_star
### (f)
### (g)
yp = b1*5+b0
yp
se.yp = sqrt( (1+1/n+(5-mean(x))^2/sxx)*s2)
lower = round(yp-t_star*se.yp,digits=3)
upper = round(yp+t_star*se.yp,digits=3)
print(paste("(",lower,",",upper,")",sep=""))