require(dplyr)
require(gdata)
require(ggplot2)
require(rjags)
require(ggmcmc)
require(string)
require(BEST)
require(foreign)
require(arm)
require(shinystan)
require(stats)

#kruschke says standardize predictors to reduce AC in MCMC sampling
#interactions
#gelman 381
#change in n chains, n adapt changes results

#intercept is male minor minority farmer who doesn't own land or an agrowell, 
#doesn't cultivate in the HE or major systems, and doesn't participate in FO

#...of individuals that have heard of bethma...
#data setup
df <- c1[!is.na(c1$ADP1_B1),]
y <- df$ADP1_B1
y <-ifelse(y==1, 1, 0)
n <- length(y)

dc.names <- as.vector(c1$HI4)
uq <- unique(dc.names)
n.dc <- length(uq)
dc <- rep(NA, n.dc)
for (i in 1:n.dc){
  dc[dc.names == uq[i]] <- i
  sample.size <- as.vector(table(dc))
}


model_string <- "model{

for (i in 1:n){
  y[i] ~ dbern(p[i])
  #dbin(p[i], 1)
  #p[i] <- max(0, min(1, yhat[i])) #bounded probability
  logit(p[i]) <- a[dc[i]] + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i]
  + b5*x5[i] + b6*x6[i] + b7*x7[i] + b8*x8[i]
}

b1 ~ dnorm(0,.0001) 
b2 ~ dnorm(0,.0001) 
b3 ~ dnorm(0,.0001) 
b4 ~ dnorm(0,.0001) 
b5 ~ dnorm(0,.0001) 
b6 ~ dnorm(0,.0001) 
b7 ~ dnorm(0,.0001) 
b8 ~ dnorm(0,.0001) 

for (j in 1: n.dc ){
a[j] ~ dnorm(mu.a, tau.a)  
}

mu.a ~ dunif(0,100)
tau.a <- pow(sigma.a , -2)  #use half-normal when # groups small
sigma.a ~ dunif (0, 100)  #Gelman (2006)
}"

#dnorm(0,.0001) | (0,1) creates half normla prior

#initialize variables
inits <- function(chain) {
  list (a=rnorm(n.dc), b1 = rnorm(1), b2 = rnorm(1),
        b3 = rnorm(1), b4 = rnorm(1), b5 = rnorm(1),
        b6 = rnorm(1), b7 = rnorm(1), b8 = rnorm(1),
        mu.a = runif(1), sigma.a = runif(1)) }

#create dataframe
data <- list(n = n, n.dc = n.dc, y = y, dc = dc, 
             x1 = c1$agrowell_user, x2 = c1$major_flag,
             x3 = c1$female, x4 = c1$sinhalese,
             x5 = c1$Standardized_SES, x6 = c1$owner, 
             x7 = c1$head_end, x8 = c1$fo)

#tell JAGS parameters to report back
parameters <- c("a", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "mu.a", "sigma.a")

#compile jags model
model <- jags.model(textConnection(model_string),
                    data = data, 
                    inits = inits,
                    n.chains = 5,
                    n.adapt = 1000)

#take 2000 random samples of each of the 3 chains
update(model, n.iter = 10000)
model_outcome <- coda.samples(model, variable.names = parameters, n.iter = 10000)
my_sso <- as.shinystan(model_outcome)
my_sso <- launch_shinystan(my_sso)

