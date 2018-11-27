surv <- read.csv("Data/survivalsize.csv")
str(surv)
summary(glm(survival ~ 1+relative_size, data=surv, family = binomial))
lregsurv <- glm(survival~1, data=surv, family=binomial)

plot(surv$survival)

plot(surv$relative_size, surv$survival)
exp(0.56+2.8)
exp(0.56)


coefficients(lmsurv)
1/(1+exp(-coefficients(lregsurv)))
plogis(coefficients(lregsurv))


lmsurvS <- glm(survival~1 + relative_size, data=surv, family=gaussian)
lregsurvS <- glm(survival~1 + relative_size, data=surv, family=binomial)

summary(lmsurvS)
summary(lregsurvS)

plot(lmsurvS)
plot(lregsurvS)

plot(surv$relative_size, surv$survival, ylim=c(-0.2,1.2))
abline(lmsurv, col="red")

plot(surv$relative_size, surv$survival, ylim=c(-0.2,1.2))
datforpred <- data.frame(relative_size=seq(from=-3,to=4, by=0.1))
datforpred$prob <- predict(lregsurvS, newdata = datforpred, type = "response")
lines(datforpred$relative_size, datforpred$prob, col="red")
        
library(ggplot2)
ggplot(surv, aes(x = relative_size, y=survival))+geom_point()+stat_smooth(method = "glm", method.args = list(family = "binomial"))


nbp <- 300
x <- rnorm(n = nbp,mean = 30, 3.4 )
sex <- sample(c(0,1), size = nbp, replace = TRUE)

y <- -1 + 0.35*(x-30) - 0.1*sex + sex*(x-30)*0.35 + rnorm(n=nbp,mean=0, 0.05)

plogis(y)
plot(x,plogis(y))
obs <- sapply(plogis(y), FUN =function(x) rbinom(1,1,x))
plot(x,obs)
summary(glm(obs ~ 1 + x*sex, family = binomial()))
presencedat <- data.frame(survival=obs, weight=x, sex=c("Female","Male")[sex+1])

write.csv(presencedat, file = "Data/survivalweight.csv")
