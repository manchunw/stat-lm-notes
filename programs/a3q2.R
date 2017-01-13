math=read.delim("C://Users/Kelvin/Documents/Homework/Coursework/yr3sem1/STAT 331/math.txt")

attach(math)

GPA.temp = GPA[1]
region.temp = region[1]
group=1

for (i in 2:length(income))
	{
if (GPA.temp!= GPA[i] | region.temp != region[i])
		{
	GPA.temp = GPA[i]
	region.temp = region[i]
	group=c(group,group[length(group)]+1)
		}	else
			{
		group=c(group,group[length(group)])
			}

	}

k=max(group)
n=length(income)

group.mean=numeric(0)

for (j in 1:k)
	{
group.mean=c(group.mean,mean(income[group==j]))	
	}

SSE.beta = 0

for (j in 1:k)
	{
SSE.beta=SSE.beta + sum((income[group==j]-group.mean[j])^2)
	}

beta=c()
beta[1]=coef(lm(income~GPA+region))[1]
beta[2]=coef(lm(income~GPA+region))[2]
beta[3]=coef(lm(income~GPA+region))[3]
SSE.betaA=0
for (i in 1:length(income))
	{
SSE.betaA=SSE.betaA + sum((income[i]-beta[1]-beta[2]*GPA[i]-beta[3]*region[i])^2)
	}

fitStat=((SSE.betaA-SSE.beta)/(k-3))/(SSE.beta/(n-k))
SSE.betaA
SSE.beta
SSE.betaA-SSE.beta
k-3
n-k
fitStat
qf(0.95,k-3,n-k)
fitStat>qf(0.95,k-3,n-k)


interaction=c()
for (i in 1:length(income))
	{
interaction[i]=GPA[i]*region[i]
	}
math$interaction=interaction
beta[1]=coef(lm(income~GPA+region+interaction))[1]
beta[2]=coef(lm(income~GPA+region+interaction))[2]
beta[3]=coef(lm(income~GPA+region+interaction))[3]
beta[4]=coef(lm(income~GPA+region+interaction))[4]
SSE.betaA=0
for (i in 1:length(income))
	{
SSE.betaA=SSE.betaA + sum((income[i]-beta[1]-beta[2]*GPA[i]-beta[3]*region[i]-beta[4]*interaction[i])^2)
	}
fitStat=((SSE.betaA-SSE.beta)/(k-3))/(SSE.beta/(n-k))
SSE.betaA
SSE.beta
SSE.betaA-SSE.beta
k-4
n-k
fitStat
qf(0.95,k-4,n-k)
fitStat>qf(0.95,k-4,n-k)
fit2=lm(income~GPA+region+interaction)
summary(fit2)
plot(fitted(fit2), rstudent(fit2), type="n", xlab="fitted", ylab="studentized residual")
text(fitted(fit2), rstudent(fit2))
abline(h=c(-2, 2), lty=2)
plot(fit2,which=4)
cooks.distance(fit2)
which(cooks.distance(fit2)>qf(0.05,4,n-4))

install.packages("car")
library(car)
crPlots(fit2)
