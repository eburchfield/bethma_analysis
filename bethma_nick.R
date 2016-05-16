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

#add cauchy prior
#probably drop ui
#include other relevant variables, build full model 
#p. 375 GELMAN

#what characterizes a farmer who practiced bethma within a community 
#how bethma farmers compare to those not engaging in bethma within communities (so how does ADP1_C1-which would need to be made into a binary factor-predict yield). 
#Finally, we need to figure out why, among the communities where bethma was practiced in 2014 (7, 8, 13, 20), there is variation in the number of farmers who engaged in bethma. 

c1 <- c1[c1$irrigtype == 1,]

#COMMUNTIY-LEVEL ANALYSES

#n per each of the six major communities
c1 %>% group_by(HI4) %>% summarise(cnt = n())
#major community ID:  7, 13, 15, 19, 23, 25

#ID subset of "bethma" communities

heard_of <- c1 %>% group_by(HI4, ADP1_A1) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
#everyone in communities 7, 13, 15, and 19 had heard of bethma
#in 23 91% of people had not heard of bethma; in 25, 82 percent of people head not heard of bethma

ever_practiced <- c1 %>% group_by(HI4, ADP1_B1) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
# 1 == YES, 2 == NO
#majority ever practiced bethma:  7 (71%), 13 (80%), 15 (91 %), 19 (54%)
#majority not practiced bethma:  23 and 25 lots of NAs and only a few YES responses

y_2014 <- c1 %>% group_by(HI4, ADP1_C1) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
#ADP1_C1 == 2 is Yala 2014
#majority practiced bethma 2014: 7 (71%), 13 (50%)
#majority no bethma 2014: 15 (85%), 19 (34%), 23 and 25 NAS


#create plot with bm non bethma in different colors with SD bars and mean 
#note that communities with no bethma did have some people doing bethma and vice versa
#variables on which to compare communities:  
#SES
#FO participation
#AW ownership
#land size
#primary occupation (?)
#owner
#experience
#ofc cultivation?

#what describes the communities in which people had not heard of bethma


#of the communities that have heard of bethma (full saturation), what describes individuals who have never practiced bethma?

#...versus those that have practiced bethma?




#during a particular SEASON, instance of bethma, communities that had heard of AND practiced bethma in the past engaged
#anything special about the two communities that practiced bethma during the drought compared to the others?



#what happened in the other communities?


#within bethma communities, what are the differences between bethma and non-bethma individuals


#within nonbethma communities, what are the differences between bethma and non-bethma individuals





y <- c1$ADP1_B1  
vname <- "Have you ever practiced bethma?"

#histograms
b1 <- c1 %>% group_by(HI4, ADP1_B1) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))

bar1 <- ggplot(b1, aes(x = c1$ADP1_B1, y = b1$freq, fill = c1$HI4)) +
  labs(x = "", y = "Percent", title = vname) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank())

print(bar1)



#DATA SETUP
#select subset of farmers in major system communities 
c1<- c1[!is.na(c1$ADP1_B1),]

y <- c1$ADP1_B1
#yes = 1; no = 2
y <-ifelse(y==1, 1, 0)
vname <- "Have you ever practiced bethma?"
it <- c("Minor", "Major")
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
#level-1 likelihood
for (i in 1:n){
y[i] ~ dbin(mu[i], 1) 
p.bound[i] <- max(0, min(1, mu[i])) #381 gelman
logit(mu[i]) <- a[dc[i]] + b1[dc[i]]*x1[i] + b2[dc[i]]*x2[i] + b3[dc[i]]*x3[i] + b4[dc[i]]*x4[i] +
b5[dc[i]]*x5[i] + b6[dc[i]]*x6[i] + b7[dc[i]]*x7[i] 
}

#if any additional priors in likelihood of y[i], specify here

#level-2 likelihood
for (j in 1: n.dc ){
a[j] ~ dt(g0, tau.a,1) #not goj, g1j
b1[j] ~ dt(b01, .001, 1) #could do prior for spread
b2[j] ~ dt(b02, .001, 1)  #instead of fixing variance, could make tau.b, etc... to allow for correlatoin
b3[j] ~ dt(b03, .001, 1) #build all of these as hyperpriors
b4[j] ~ dt(b04, .001, 1)
b5[j] ~ dt(b05, .001, 1)
b6[j] ~ dt(b06, .001, 1)
b7[j] ~ dt(b07, .001, 1)
}
#level-3 hyperlevel (SL)
g0 ~ dt(0, .001, 1)
b01 ~ dt(0, .001, 1)
b02 ~ dt(0, .001, 1)
b03 ~ dt(0, .001, 1)
b04 ~ dt(0,.001, 1)
b05 ~ dt(0, .001, 1)
b06 ~ dt(0, .001, 1)
b07 ~ dt(0, .001, 1)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif(0, 100)  
}"

#initialize variables
inits <- function(chain) {
  list (a=rnorm(n.dc), b01 = rnorm(1), b02 = rnorm(1),
        b03 = rnorm(1), b04 = rnorm(1), b05 = rnorm(1),
        b06 = rnorm(1), b07 = rnorm(1), 
        g0 = rnorm(1), sigma.a = runif(1)) }

#create dataframe
data <- list(n = n, n.dc = n.dc, y = y, dc = dc, 
             x1 = c1$agrowell_user, 
             x2 = c1$female, x3 = c1$sinhalese,
             x4 = c1$Standardized_SES, x5 = c1$owner, 
             x6 = c1$head_end, x7 = c1$fo)

#tell JAGS parameters to report back
parameters <- c("a", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b01", "b02", "b03", "b04", "b05", "b06", "b07", "g0", "sigma.a")

#compile jags model
model <- jags.model(textConnection(model_string),
                              data = data, 
                              inits = inits,
                              n.chains = 3,
                              n.adapt = 1000)

#take 2000 random samples of each of the 3 chains
update(model, n.iter = 5000)
ADP_B1_outcome <- coda.samples(model, variable.names = parameters, n.iter = 5000)
my_sso <- as.shinystan(ADP_B1_outcome)
my_sso <- launch_shinystan(my_sso)

#DIAGNOSTICS
#diagnosing mixing of chains, we want good overlap of chains
samples <- ggs(model_outcome, family = '(sigma|b).*')
ggs_traceplot(samples) + theme_bw() + theme(legend.position='none', strip.background = element_blank())

#diagnosing aucotorrelation
auto.plot <- ggs_autocorrelation(samples, family = "sigma.a") +
  theme_bw() + theme(legend.position = 'none', strip.background = element_blank())
auto.plot

#gelman-rubin scale reduction factor (how much better would predictions be with infinite number of iterations)
gelman.diag(model_outcome)



