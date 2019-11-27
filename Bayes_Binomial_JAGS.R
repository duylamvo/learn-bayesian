#------------------------------------------------------------------------------#
#------------------           Bayesian Statistics   ---------------------------#
#------------------       Beta Binomial Model                ------------------#
#------------------------------------------------------------------------------#

# packages
library(rjags)

# ---------------------------------------------------------
# check your prior
a <- 1
b <- 1

beta.dist <- function(x) dbeta(x,a,b)
curve(beta.dist,0,1,ylab="Density")


# ---------------------------------------------------------
# JAGS Model
beta.binom.model <- "model{
   y~dbin(p,N)      # Likelihood
   p ~ dbeta(1,1) # Prior
}"


jags.data <- list(y = 60,N=100)

# Running JAGS
jags.binom <- jags.model(textConnection(beta.binom.model),
                         data=jags.data, n.chains=3)

update(jags.binom, 1000)

jags.out <- coda.samples(jags.binom,
                         variable.names= c("p"),
                         n.iter=10000, thin=1)
# more n.iter then the table(p>0.5) will more stable

# Simple description of posterior
summary(jags.out)
plot(jags.out)

# Which percentage of posterior p>0.5 ?
p <- unlist(jags.out)

mean(p); sd(p); quantile(p, pr=c(0.025, 0.975))
table(p>0.5)
# rate
table(p>0.5) / length(p)

par(mfrow=c(1,2))   # if you like to show two figures in a row and two columns
hist(p)
plot(density(p))


