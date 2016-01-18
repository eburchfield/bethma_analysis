require(dplyr)
require(gdata)
require(ggplot2)
require(lme4)
require(rjags)
require(ggmcmc)
require(string)
require(BEST)
require(foreign)
require(arm)

#data
y <- c1$ADP1_A1
#yes = 1; no = 2
y <-ifelse(y==1, 1, 0)
vname <- "Have you heard of bethma?"
it <- c("Minor", "Major")
n <- length(y)

dc.names <- as.vector(c1$HI4)
uq <- unique(dc.names)
itc <- c(0,1,0,1,1,0,0,0,0,1,0,0,1,0,1)
n.dc <- length(uq)
dc <- rep(NA, n.dc)
for (i in 1:n.dc){
  dc[dc.names == uq[i]] <- i
  sample.size <- as.vector(table(dc))
}


model_string <- "model{

#level-1 likelihood
for (i in 1:n){
y[i] ~ dbern(mu[i])  
logit(mu[i]) <- a[dc[i]] + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i]
}

#level-1 prior
b1 ~ dnorm(0, .0001)
b2 ~ dnorm(0, .0001)
b3 ~ dnorm(0, .0001)
b4 ~ dnorm(0, .0001)

#level-2 likelihood
for (j in 1: n.dc ){
a[j] ~ dnorm(g0 + g1*u[j], tau.a)
}

#level-2 prior
g1 ~ dnorm(0, .0001)
g0 ~ dnorm(0, .0001)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)
}"

#initialize variables
inits <- function(chain) {
  list (a=rnorm(n.dc), b1 = rnorm(1), b2 = rnorm(1), b3 = rnorm(1), b4 = rnorm(1), 
        g0 = rnorm(1), g1 = rnorm(1), 
        sigma.a = runif(1)) }

#create dataframe
data <- list(n = n, n.dc = n.dc, y = y, dc = dc, 
             #individual level
             x1 = c1$Standardized_SES, x2 = c1$owner, 
             x3 = c1$female, x4 = c1$head_end, 
             #group level
             u = c1$irrigtype)

#tell JAGS parameters to report back
parameters <- c("a", "b1", "b2", "b3", "b4", "g1", "g0", "sigma.a")

#compile jags model
model <- jags.model(textConnection(model_string),
                              data = data, 
                              inits = inits,
                              n.chains = 3,
                              n.adapt = 100)

#take 2000 random samples of each of the 3 chains
update(model, n.iter = 5000)
model_outcome <- coda.samples(model, variable.names = parameters, n.iter = 5000)

#plot results
plot(model_outcome[,'b3'])

#diagnosing mixing of chains, we want good overlap of chains
samples <- ggs(model_outcome, family = '(sigma|b).*')
ggs_traceplot(samples) + theme_bw() + theme(legend.position='none', strip.background = element_blank())

#diagnosing aucotorrelation
auto.plot <- ggs_autocorrelation(samples, family = "sigma.a") +
  theme_bw() + theme(legend.position = 'none', strip.background = element_blank())
auto.plot

#if we see autocorrelation, we can thin the MC by telling it to remember only every fourth iteration
# thin.steps = 4
# model_outcome_ac <- coda.samples(model_outcome, variable.names = parameters,
#                                 n.iter = 2000, thin = thin.steps)
# auto.plot.thinned <- ggs_autocorrelation(ggs(model_outcome_ac), family = 'sigma.a') +
#   theme_bw() + 
#   theme(legend.position='none', strip.background = element_blank())
# print(auto.plot)
# print(auto.plot.thinned)

#gelman-rubin scale reduction factor (how much better would predictions be with infinite number of iterations)
gelman.diag(model_outcome)


traceplot(model_outcome)

