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


#test model fit
M1 = lmer(y ~ agrowell_user + major_flag + female + sinhalese + Standardized_SES +
            owner + head_end + fo + (1|HI4), data = c1, REML = "False", family = binomial(link = "logit")) 

b1.t <- fixef(M1)[2]
b2.t <- fixef(M1)[3]
b3.t <- fixef(M1)[4]
b4.t <- fixef(M1)[5]
b5.t <- fixef(M1)[6]
b6.t <- fixef(M1)[7]
b7.t <- fixef(M1)[8]
b8.t <- fixef(M1)[9]
mu.a.t <- fixef(M1)[1]
sigma.a.t <- 1.5 #random

a.t <- rep(NA, n.dc)
for (j in 1:n.dc){
  a.t[j] <- rnorm(1, mu.a.t, sigma.a.t)
}

y.fake <- rep(NA, n)
for (i in 1:n) {
  y.fake[i] <- rnorm(1, a.t[dc[i]] + b1.t*c1$agrowell_user[i] + b2.t*c1$major_flag[i] + b3.t*c1$female[i] + b4.t*c1$sinhalese[i] + 
                       b5.t*c1$Standardized_SES[i] + b6.t*c1$owner[i] + b7.t*c1$head_end[i] + b8.t*c1$fo[i], sigma.a.t)
}

data.fake <- list(n = n, n.dc = n.dc, y = y.fake, dc = dc, 
                  x1 = c1$agrowell_user, x2 = c1$major_flag,
                  x3 = c1$female, x4 = c1$sinhalese,
                  x5 = c1$Standardized_SES, x6 = c1$owner, 
                  x7 = c1$head_end, x8 = c1$fo)

#tell JAGS parameters to report back
parameters.fake <- c("a", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "mu.a", "sigma.a")

#compile jags model
model.fake <- jags.model(textConnection(model_string),
                         data = data, 
                         inits = inits,
                         n.chains = 3,
                         n.adapt = 100)

#take 2000 random samples of each of the 3 chains
update(model.fake, n.iter = 3000)
model_outcome_fake <- coda.samples(model.fake, variable.names = parameters.fake, n.iter = 3000)
my_sso <- as.shinystan(model_outcome_fake)
my_sso <- launch_shinystan(my_sso)

