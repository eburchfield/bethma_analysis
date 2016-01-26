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
require(shinystan)
require(stats)

c1 <- as.data.frame(load("C:\Users\Emily Burchfield\Box Sync\WF\Survey\c1_data_full.Rda"))
dc.names <- as.vector(c1$HI4)
uq <- unique(dc.names)
n.dc <- length(uq)
dc <- rep(NA, n.dc)
for (i in 1:n.dc){
  dc[dc.names == uq[i]] <- i
  sample.size <- as.vector(table(dc))
}
c1$dc <- dc

df <- c1[!is.na(c1$ADP1_B1),]
y <- df$ADP1_B1
y <-ifelse(y==1, 1, 0)
n <- length(y)


model_string <- "model{
#level-1 likelihood
for (i in 1:n){
y[i] ~ dbin(mu[i], 1) 
#p.bound[i] <- max(0, min(1, mu[i])) #381 gelman
logit(mu[i]) <- a[dc[i]] + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i]
+ b5*x5[i] + b6*x6[i] + b7*x7[i] + b8*x8[i]
}

#if any additional priors in likelihood of y[i], specify here
b1 ~ dt(0,.1, 1) 
b2 ~ dt(0,.1, 1) 
b3 ~ dt(0,.1, 1) 
b4 ~ dt(0,.1, 1) 
b5 ~ dt(0,.1, 1) 
b6 ~ dt(0,.1, 1) 
b7 ~ dt(0,.1, 1) 
b8 ~ dt(0,.1, 1) 
#level-2 likelihood
for (j in 1: n.dc ){
a[j] ~ dt(g0, tau.a, 1)  
}
#level-3 hyperlevel (SL)
g0 ~ dt(0, .001, 1) 
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)  
}"



#initialize variables
inits <- function(chain) {
  list (a=rnorm(n.dc), b1 = rnorm(1), b2 = rnorm(1),
        b3 = rnorm(1), b4 = rnorm(1), b5 = rnorm(1),
        b6 = rnorm(1), b7 = rnorm(1), b8 = rnorm(1),
        g0 = rnorm(1), sigma.a = runif(1)) }

#create dataframe
data <- list(n = n, n.dc = n.dc, y = y, dc = dc, 
             #individual level
             x1 = c1$agrowell_user, x2 = c1$major_flag,
             x3 = c1$female, x4 = c1$sinhalese,
             x5 = c1$Standardized_SES, x6 = c1$owner, 
             x7 = c1$head_end, x8 = c1$fo)

#tell JAGS parameters to report back
parameters <- c("a", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "g0", "sigma.a")

#compile jags model
model <- jags.model(textConnection(model_string),
                    data = data, 
                    inits = inits,
                    n.chains = 3,
                    n.adapt = 1000)

#take 2000 random samples of each of the 3 chains
update(model, n.iter = 10000)
model_outcome <- coda.samples(model, variable.names = parameters, n.iter = 10000)
my_sso <- as.shinystan(model_outcome)
my_sso <- launch_shinystan(my_sso)




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

