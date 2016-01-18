
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

#glm
fit.1 <- glm(y ~ c1$Standardized_SES, family=binomial(link="logit"))
jitter.binary <- function(a, jitt=.05){
  ifelse(a==0, runif(length(a), 0, jitt), runif(length(a), 1-jitt, 1))
}
y.jitter <- jitter.binary(y)
plot(c1$Standardized_SES, y.jitter)
curve(invlogit(coef(fit.1)[1] + coef(fit.1)[2]*x), add =T)

#coeff interpretation (p. 90 gelman)
pr_no_x = invlogit(coef(fit.1)[1])  #63 percent chance of switching with no SES (illogical interpretation here)
by_4 = coef(fit.1)[2]/4 #adding 1 to SES increases chance of hearing of bm by 6 percent

#additional predictor
fit.2 <- glm(y ~ c1$Standardized_SES + c1$agrowell_user, family=binomial(link="logit"))


