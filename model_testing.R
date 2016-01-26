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
require(lme4)

#c1 <- as.data.frame(load("C:\\Users\\Emily Burchfield\\Box Sync\\WF\\Survey\\c1_data_full.Rda"))
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

#test model fit
M1 = glmer(y ~ agrowell_user + major_flag + female + sinhalese + Standardized_SES + owner + head_end + 
            fo + (1|HI4), family = binomial(link = "logit"), data = df)

#parameter values taken from MLM results
b1.f <- fixef(M1)[2]
b2.f <- fixef(M1)[3]
b3.f <- fixef(M1)[4]
b4.f <- fixef(M1)[5]
b5.f <- fixef(M1)[6]
b6.f <- fixef(M1)[7]
b7.f <- fixef(M1)[8]
b8.f <- fixef(M1)[9]
mu.a.f <- fixef(M1)[1]
sigma.a.f <- .8 #random

#create fake data
x1.f <- rbinom(n, 1, mean(c1$agrowell_user))
x2.f <- rbinom(n, 1, mean(c1$major_flag))
x3.f <- rbinom(n, 1, mean(c1$female))
x4.f <- rbinom(n, 1, mean(c1$sinhalese))
x5.f <- rnorm(n, 1, mean(!is.na(c1$Standardized_SES)))
x6.f <- rbinom(n, 1, mean(c1$owner))
x7.f <- rbinom(n, 1, mean(c1$head_end))
x8.f <- rbinom(n, 1, mean(c1$fo))

#fake y
a.f <- mu.a.f + rnorm(n.dc, 0, sigma.a.f)
y.f <- rep(NA, n)
for (i in 1:n){
  linpred <- a.f[dc[i]] + b1.f*x1.f[i] + b2.f*x2.f[i] + b3.f*x3.f[i] + b4.f*x4.f[i] + b5.f*x5.f[i] + b6.f*x6.f[i] + 
    b7.f*x7.f[i] + b8.f*x8.f[i] + rnorm(1, 0, sigma.a.f)
  pi <- exp(linpred)/(1 + exp(linpred))
  y.f[i] <- rbinom(n = n, size=1, prob = pi) 
}

#L1 variables
a.f <- rep(NA, n.dc)
for (j in 1:n.dc){
  a.f[j] <- rnorm(1, mu.a.f, sigma.a.f)
}

y.fake <- rep(NA, n)
for (i in 1:n) {
  y.fake[i] <- rnorm(1, a.f[dc[i]] + b1.t*c1$agrowell_user[i] + b2.t*c1$major_flag[i] + b3.t*c1$female[i] + b4.t*c1$sinhalese[i] + 
                       b5.t*c1$Standardized_SES[i] + b6.t*c1$owner[i] + b7.t*c1$head_end[i] + b8.t*c1$fo[i], sigma.a.t)
}


inits.f <- function(chain) {
  list(a.f = rnorm(n.dc), b1.f = rnorm(1), b2.f = rnorm(1), b3.f = rnorm(1), b4.f = rnorm(1),
       b5.f = rnorm(1), b6.f = rnorm(1), b7.f = rnorm(1), b8.f =rnorm(1),
       mu.a.f =runif(1), sigma.a.f = runif(1))
}

data.f <- list(n = n, n.dc = n.dc, y.f = y.f, dc = dc, 
                  x1.f = x1.f, x2.f = x2.f, x3.f = x3.f,
               x4.f = x4.f, x5.f = x5.f, x6.f = x6.f,
               x7.f = x7.f, x8.f =x8.f)

model_string_f <- "model{
#level-1 likelihood
for (i in 1:n){
y.f[i] ~ dbin(mu[i], 1) 
#p.bound[i] <- max(0, min(1, mu[i])) #381 gelman
logit(mu[i]) <- a.f[dc[i]] + b1.f*x1.f[i] + b2.f*x2.f[i] + b3.f*x3.f[i] + b4.f*x4.f[i]
+ b5.f*x5.f[i] + b6.f*x6.f[i] + b7.f*x7.f[i] + b8.f*x8.f[i]
}

#if any additional priors in likelihood of y[i], specify here
b1.f ~ dt(0,.1, 1) 
b2.f ~ dt(0,.1, 1) 
b3.f ~ dt(0,.1, 1) 
b4.f ~ dt(0,.1, 1) 
b5.f ~ dt(0,.1, 1) 
b6.f ~ dt(0,.1, 1) 
b7.f ~ dt(0,.1, 1) 
b8.f ~ dt(0,.1, 1) 

#level-2 likelihood
for (j in 1: n.dc ){
a.f[j] ~ dt(mu.a.f, tau.a.f, 1)  
}
#level-3 hyperlevel (SL)
mu.a.f ~ dt(0, .001, 1) 
tau.a.f <- pow(sigma.a.f , -2)
sigma.a.f ~ dunif (0, 1)
}
"

parameters.fake <- c("a", "b1.f", "b2.f", "b3.f", "b4.f", "b5.f", "b6.f", "b7.f", "b8.f", "mu.a.f", "sigma.a.f")

#compile jags model
model.f <- jags.model(textConnection(model_string_f),
                    data = data.f, 
                    inits = inits.f,
                    n.chains = 3,
                    n.adapt = 1000)

#take 2000 random samples of each of the 3 chains
update(model.f, n.iter = 10000)
model_outcome_f <- coda.samples(model.f, variable.names = parameters.fake, n.iter = 10000)
gelman.diag(model_outcome_f)

my_sso <- as.shinystan(model_outcome_f)
my_sso <- launch_shinystan(my_sso)
#compare posterior distributions to actual parameters


#pulling out data
post.fake <- as.matrix (model_outcome_f)
fake.mean.g0 <- mean ( post.fake [,'mu.a.f'])
fake.hdi.50.g0 <- hdi( post.fake [,'mu.a.f'], credMass =0.50)
fake.hdi.95.g0 <- hdi( post.fake [,'mu.a.f'], credMass =0.95)
fake.mean.sigma.a <- mean ( post.fake [,'sigma.a.f'])
fake.hdi.50.sigma.a <- hdi( post.fake [,'sigma.a.f'], credMass =0.50)
fake.hdi.95.sigma.a <- hdi( post.fake [,'sigma.a.f'], credMass =0.95)
fake.mean.b <- mean ( post.fake [,'b1.f'])
fake.hdi.95.b <- hdi( post.fake [,'b1.f'], credMass =0.95)
fake.hdi.50.b <- hdi( post.fake [,'b1.f'], credMass =0.50)
fake.mean.b2 <- mean ( post.fake [,'b2.f'])
fake.hdi.95.b2 <- hdi( post.fake [,'b2.f'], credMass =0.95)
fake.hdi.50.b2 <- hdi( post.fake [,'b2.f'], credMass =0.50)
fake.mean.b3 <- mean ( post.fake [,'b3.f'])
fake.hdi.95.b3 <- hdi( post.fake [,'b3.f'], credMass =0.95)
fake.hdi.50.b3 <- hdi( post.fake [,'b3.f'], credMass =0.50)
fake.mean.b4 <- mean ( post.fake [,'b4.f'])
fake.hdi.95.b4 <- hdi( post.fake [,'b4.f'], credMass =0.95)
fake.hdi.50.b4 <- hdi( post.fake [,'b4.f'], credMass =0.50)
fake.mean.b5 <- mean ( post.fake [,'b5.f'])
fake.hdi.95.b5 <- hdi( post.fake [,'b5.f'], credMass =0.95)
fake.hdi.50.b5 <- hdi( post.fake [,'b5.f'], credMass =0.50)
fake.mean.b6 <- mean ( post.fake [,'b6.f'])
fake.hdi.95.b6 <- hdi( post.fake [,'b6.f'], credMass =0.95)
fake.hdi.50.b6<- hdi( post.fake [,'b6.f'], credMass =0.50)
fake.mean.b7 <- mean ( post.fake [,'b7.f'])
fake.hdi.95.b7 <- hdi( post.fake [,'b7.f'], credMass =0.95)
fake.hdi.50.b7 <- hdi( post.fake [,'b7.f'], credMass =0.50)
fake.mean.b8 <- mean ( post.fake [,'b8.f'])
fake.hdi.95.b8 <- hdi( post.fake [,'b8.f'], credMass =0.95)
fake.hdi.50.b8 <- hdi( post.fake [,'b8.f'], credMass =0.50)
fake.mean.a <-rep (NA , n.dc )
fake.hdi.50.a <- cbind ( lower = rep (NA , n.dc ),upper = rep (NA , n.dc ))
fake.hdi.95.a <- cbind ( lower = rep (NA , n.dc ),upper = rep (NA , n.dc ))
#for (i in 1: n.dc ) {
 # fake.mean.a[i] <-mean(post.fake[paste('a.f[',i,']', sep='')])
  #fake.hdi.95.a[i,] <- hdi( post.fake [paste ('a.f[',i,']', sep='')],credMass =0.95)
}

#based on this analysis all but b1 and b3 are within 50 percent; others 95 %
