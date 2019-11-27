#------------------------------------------------------------------------------#
#------------------   Bayesian Statistics   -----------------------------------#
#------------------   Bivariate Regression  -----------------------------------#
#------------------------------------------------------------------------------#

# Set Working Directory
setwd("Daten")

# packages
library(rjags)

# Loading data
load("Bayes_Student_Survey.RData")

#
# Task: Estimate the effect of the size of friendship network on pol. efficacy
#

#------------------------------------------------------------------------------#
# OLS regression
#------------------------------------------------------------------------------#


# Simple plot
par(mfrow=c(1,2))
hist(dat$poleff)
hist(dat$friend)

plot(log(dat$friend+1)  , dat$poleff  , 
     type="p",main="Student Survey",
     xlab="No of friends",ylab="Pol. Efficacy")

plot(jitter(log(dat$friend+1))  , jitter(dat$poleff)  , 
     type="p",main="Student Survey",
     xlab="No of friends",ylab="Pol. Efficacy")

# OLS Regression
ols.out <- lm(poleff ~ log(friend+1), data=dat)
summary(ols.out)

abline(reg=ols.out, col="red")


#------------------------------------------------------------------------------#
# Regression via rjags
#------------------------------------------------------------------------------#

# JAGS Modell
reg.model <- "model{
  for (i in 1:N){
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + beta1 * x[i]
  }
  
  beta0 ~ dnorm(0,0.0001)
  beta1 ~ dnorm(0,0.0001) 
  
  tau ~ dgamma(0.001,0.001) 
  sigma <- 1/sqrt(tau)
}"

y <- dat$poleff
x <- log(dat$friend+1)
N <- length(y)

jags.data <- list(y=y,x=x,N=N)

# three different intial values for beta1
jags.inits.1 <- list(beta1=32)
jags.inits.2 <- list(beta1=50)
jags.inits.3 <- list(beta1=-10)

jags.reg <- jags.model(file=textConnection(reg.model),
                       inits=list(jags.inits.1,jags.inits.2,jags.inits.3),
                       data=jags.data, n.chains=3)

#update(jags.reg, 2000)
jags.reg.out <- coda.samples(jags.reg,
                             variable.names=c("beta0","beta1","sigma"),
                             n.iter=500, thin=1)
summary(jags.reg.out)
plot(jags.reg.out)

# for comparison with OLS results
summary(ols.out)

