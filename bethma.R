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

#POOLED MEANS
dc.names <- as.vector(c1$HI4)
uq <- unique(dc.names)
itc <- c(0,1,0,1,1,0,0,0,0,1,0,0,1,0,1)
n.dc <- length(uq)

dc <- rep(NA, n.dc)
for (i in 1:n.dc){
  dc[dc.names == uq[i]] <- i
  sample.size <- as.vector(table(dc))
}

#complete pooling
ybarbar = mean(y)

sample.size.jittered <- sample.size*exp(runif(n.dc, -.1, .1))
dc.mns <- tapply(y, dc, mean)
dc.vars <- tapply(y, dc, var)
dc.sd <- mean(sqrt (dc.vars [!is.na(dc.vars)] ))/sqrt(sample.size)
dc.sd.sep <- sqrt(tapply(y, dc, var)/sample.size)

# #plot
# frame1a <- data.frame ( x = sample.size.jittered , y = dc.mns,
#                         y.min = dc.mns - dc.sd,
#                         y.max = dc.mns + dc.sd )
# 
# p1a <- ggplot(frame1a , aes(x = x, y = y)) +
#   labs(x = "DC sample size", y = vname, title = "No pooling") +
#   geom_hline(yintercept = ybarbar) +
#   geom_pointrange(aes(ymin = y.min, ymax = y.max, y=y)) +
#   geom_text(aes(label = it), size = 4, hjust = 1 , vjust = 1)
# theme_bw() 
# print(p1a)


#partial pooling estimates from MLM
M0 <- lmer(y ~ 1 + (1|dc))

#jags with same model
model_string <- "model{
for (i in 1:n){
y[i] ~ dnorm (a[dc[i]], tau.y)
}
tau.y <- pow(sigma.y , -2)
sigma.y ~ dunif (0, 100)
for (j in 1: n.dc ){
a[j] ~dnorm (mu.a , tau.a)
}
mu.a ~ dnorm (0, .0001)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)
}"

inits <- function(chain){
  list (a=rnorm(n.dc), mu.a = rnorm(1),
        sigma.y = runif(1), sigma.a = runif(1))
}

#prepare list of data to pass to JAGS model
#building dataframe (jagsname = localvariable)
n <- length(y)
data <- list(n=n, n.dc = n.dc, y=y, dc = dc)

#tell jags parameter names
parameters <- c("a", "mu.a", "sigma.y", "sigma.a")

#compile model and initialize data/parameters
nopred.model <- jags.model(textConnection(model_string),
                           data = data,
                           inits = inits,
                           n.chains = 3,
                           n.adapt = 1000)

update(nopred.model, n.iter = 2000)  #throws away 1st half

nopred <- coda.samples(nopred.model,
                       variable.names = parameters,
                       n.iter = 2000)

#discuss results from#
summary(nopred)
#ggmcmc plotting outputs


#get data from jags
post.nopred <- as.matrix(nopred)  #belief in parameters given data
mean.a.nopred <- rep(NA, n.dc)  #mean for each county
sd.a.nopred <- rep(NA, n.dc)  #sd for each county
for (i in 1:n.dc) {
  mean.a.nopred[i] <- mean(post.nopred[ , paste ('a[',i,']', sep='')])
  sd.a.nopred[i] <- sd(post.nopred [ , paste ('a[',i,']', sep='')])
}


frame1b <- data.frame ( x = sample.size.jittered ,
                        y = mean.a.nopred,
                        y.min = mean.a.nopred - sd.a.nopred,
                        y.max = mean.a.nopred + sd.a.nopred)

p1b <- ggplot(frame1b , aes(x = x, y = y, label=uq)) +
  labs(x = "DC sample size", y = vname, title = "MLM") +
  geom_hline(yintercept = ybarbar) +
  geom_pointrange(aes(ymin = y.min, ymax = y.max, y=y)) + 
  geom_text(aes(label = uq), size = 3, hjust = 1 , vjust = 1, angle = 90)


print(p1b)  







######################################
##TWO level1 PREDICTOR##
####################################
#need to updtae plotting

#jags with same model
model_string <- "model{
for (i in 1:n){
y[i] ~ dnorm (y.hat[i], tau.y)
y.hat[i] <- a[dc[i]] + b1 * x1[i] + b2 * x2[i]
}
b1 ~ dnorm(0, .0001)
b2 ~ dnorm(0, .0001)
tau.y <- pow(sigma.y , -2)
sigma.y ~ dunif (0, 100)

for (j in 1: n.dc ){
a[j] ~dnorm (mu.a , tau.a)
}
mu.a ~ dnorm (0, .0001)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)
}"

#initialize variables
predictor.inits <- function(chain) {
  list (a=rnorm(n.dc), b1 = rnorm(1), b2 = rnorm(1), mu.a = rnorm(1),
        sigma.y = runif(1), sigma.a = runif(1))
}

#prepare dataframe to pass to JAGS
n <- length(y)
pred1.data <- list(n = n, n.dc = n.dc, y = y, dc = dc, x1 = c1$HH2_D.01, x2 = c1$HH_J.1)

#tell JAGS parameters to report back
pred1.parameters <- c("a", "b1", "b2", "mu.a", "sigma.y", "sigma.a")

#compile jags model
mlm.pred1.model <- jags.model(textConnection(model_string),
                              data = pred1.data, 
                              inits = predictor.inits,
                              n.chains = 3,
                              n.adapt = 100)

#take 2000 random samples of each of the 3 chains
update(mlm.pred1.model, n.iter = 5000)

mlm.pred1.pred <- coda.samples( mlm.pred1.model, variable.names = pred1.parameters, n.iter = 5000)

#get data back from jags and make useful
post.pred <- as.matrix(mlm.pred1.pred)
alphabarbar <- mean(post.pred[,"mu.a"])
mean.a.pred <- rep(NA, n.dc)
sd.a.pred <- rep(NA, n.dc)
for (i in 1:n.dc){
  mean.a.pred[i] <- mean(post.pred [ , paste ('a[',i,']', sep='')])
  sd.a.pred[i] <- sd ( post.pred [ , paste ('a[',i,']', sep='')])
}

#plot results
plot(mlm.pred1.pred[,'b1'])

#diagnosing mixing of chains, we want good overlap of chains
samples <- ggs(mlm.pred1.pred, family = '(sigma|b).*')
ggs_traceplot(samples) + theme_bw() + theme(legend.position='none', strip.background = element_blank())


#diagnosing aucotorrelation
auto.plot <- ggs_autocorrelation(samples, family = "sigma.a") +
  theme_bw() + theme(legend.position = 'none', strip.background = element_blank())
auto.plot

#if we see autocorrelation, we can thin the MC by telling it to remember only every fourth iteration
thin.steps = 4
mlm.pred1.pred2 <- coda.samples(mlm.pred1.model, variable.names = pred1.parameters,
                                n.iter = 2000, thin = thin.steps)
auto.plot.thinned <- ggs_autocorrelation(ggs(mlm.pred1.pred2), family = 'sigma.a') +
                                           theme_bw() + 
                                           theme(legend.position='none', strip.background = element_blank())
print(auto.plot)
print(auto.plot.thinned)

#gelman-rubin scale reduction factor (how much better would predictions be with infinite number of iterations)
gelman.diag(mlm.pred1.pred[,c("mu.a", "sigma.a", "sigma.y", "b1", "b2")])
#for PRSF > 1, uncertainty could reduce by a factor of PRSF - 1... i.e. 1.12 means more iterations could make 
#paramete estimate 12% more accurate





######################################
#adding group-level predictors
#####################################

######################################
##TWO level1 PREDICTOR##
####################################
#need to updtae plotting

#jags with same model
model_string <- "model{
for (i in 1:n){
y[i] ~ dnorm (y.hat[i], tau.y)
y.hat[i] <- a[dc[i]] + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i]
}
b1 ~ dnorm(0, .0001)
b2 ~ dnorm(0, .0001)
b3 ~ dnorm(0, .0001)
b4 ~ dnorm(0, .0001)
tau.y <- pow(sigma.y , -2)
sigma.y ~ dunif (0, 100)

for (j in 1: n.dc ){
a[j] ~ dnorm(g0 + g1*u[j], tau.a)
}
g1 ~ dnorm(0, .0001)
g0 ~ dnorm(0, .0001)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)
}"

#initialize variables
predictor.inits <- function(chain) {
  list (a=rnorm(n.dc), b1 = rnorm(1), b2 = rnorm(1), b3 = rnorm(1), b4 = rnorm(1), 
        g0 = rnorm(1), g1 = rnorm(1), 
        sigma.y = runif(1), sigma.a = runif(1))
}

#prepare dataframe to pass to JAGS
pred1.data <- list(n = n, n.dc = n.dc, y = y, dc = dc, 
                   x1 = c1$Standardized_SES, x2 = c1$owner, 
                   x3 = c1$sinhalese, x4 = c1$diverse_flag, u = c1$irrigtype)

#tell JAGS parameters to report back
pred1.parameters <- c("a", "b1", "b2", "b3", "b4", "g1", "g0", "sigma.y", "sigma.a")

#compile jags model
mlm.pred1.model <- jags.model(textConnection(model_string),
                              data = pred1.data, 
                              inits = predictor.inits,
                              n.chains = 3,
                              n.adapt = 100)

#take 2000 random samples of each of the 3 chains
update(mlm.pred1.model, n.iter = 5000)

mlm.predl2.pred <- coda.samples( mlm.pred1.model, variable.names = pred1.parameters, n.iter = 5000)

#get data back from jags and make useful
post.pred <- as.matrix(mlm.predl2.pred)
#alphabarbar <- mean(post.pred[,"mu.a"])
mean.a.pred <- rep(NA, n.dc)
sd.a.pred <- rep(NA, n.dc)
for (i in 1:n.dc){
  mean.a.pred[i] <- mean(post.pred [ , paste ('a[',i,']', sep='')])
  sd.a.pred[i] <- sd ( post.pred [ , paste ('a[',i,']', sep='')])
}

#plot results
plot(mlm.predl2.pred[,'b1'])

#gelman-rubin scale reduction factor (how much better would predictions be with infinite number of iterations)
gelman.diag(mlm.predl2.pred[,c("sigma.a", "sigma.y", "b1", "b2", "b3", "b4", "g1", "g0")])
#for PRSF > 1, uncertainty could reduce by a factor of PRSF - 1... i.e. 1.12 means more iterations could make 
#paramete estimate 12% more accurate

#traceplot(mlm.predl2.pred)

