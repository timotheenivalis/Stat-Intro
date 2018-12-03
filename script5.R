surv <- read.csv("Data/survivalsize.csv")
plot(surv$relative_size, surv$survival)

summary(lm(survival ~ 1, data=surv))
summary(glm(survival ~ 1, data=surv, family=binomial))

lmss <- lm(survival ~ 1 + relative_size, data=surv)
glmss <- glm(survival ~ 1 + relative_size, data=surv, family=binomial)

summary(lmss)
summary(glmss)

plogis(0.5610)
plogis(0.5610+1*2.8078)
plogis(0.5610-2*2.8078)

plot(lmss)
plot(glmss)


plot(surv$relative_size, surv$survival)
abline(lmss, col="red")
library(ggplot2)
ggplot(surv, aes(x=relative_size, y=survival))+
  geom_point()+stat_smooth(method="lm")

ggplot(surv, aes(x=relative_size, y=survival))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"))

newdat <- data.frame(relative_size=seq(from=-2.2, to=3.5, length.out = 100))
newdat$predictionsurvival <- predict(object = glmss, type = "response", newdata = newdat)
plot(surv$relative_size, surv$survival)
lines(x=newdat$relative_size, y=newdat$predictionsurvival, col="red")


newdat <- data.frame(relative_size=seq(from=-2.2, to=3.5, length.out = 100))
newdat <- predict(object = glmss, type = "response", newdata = newdat, se.fit = TRUE)
plot(surv$relative_size, surv$survival)
lines(x=newdat$relative_size, y=newdat$predictionsurvival, col="red")

newdat$fit +newdat$se.fit*2

newdat$fit -newdat$se.fit*2


newdat <- data.frame(relative_size=seq(from=-2.2, to=3.5, length.out = 100))
fit <- predict(object = glmss, type = "link", newdata = newdat, se.fit = TRUE)
plot(surv$relative_size, surv$survival)

lines(x=newdat$relative_size, y=plogis(fit$fit), col="red", lwd=5)
lines(x=newdat$relative_size, y=plogis(fit$fit-2*fit$se.fit), col="red")
lines(x=newdat$relative_size, y=plogis(fit$fit+2*fit$se.fit), col="red")


survweight <- read.csv("Data/survivalweight.csv")
str(survweight)

ggplot(survweight, aes(x=weight, y=survival, color=sex))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"))
summary(glm(survival ~ 1 + sex*weight, data=survweight, family = binomial))



#### Poisson ####

poisson_random_sample <- rpois(n = 10000, lambda = 1)
hist(poisson_random_sample)
mean(poisson_random_sample)
var(poisson_random_sample)

repro <- read.csv("Data/reproduction.csv")
str(repro)
library(ggplot2)
ggplot

hist(repro$reproduction, freq = FALSE)
lines(x = seq(from=-3, to=15, by=0.1),
      y=dnorm(x = seq(from=-3, to=15, by=0.1),
            mean=mean(repro$reproduction),
            sd = sd(repro$reproduction)), col="red")

lmrep <- lm(reproduction ~ 1 + size, data=repro)
summary(lmrep)
plot(repro$size, repro$reproduction)
abline(lmrep, col="red")
plot(lmrep)

glmrep <- glm(reproduction ~ 1 + size*sex, data = repro, family = "quasipoisson")
summary(glmrep)
exp(0.36669+ 10*0.22365)

ggplot(repro, aes(x=size, y=reproduction, color=as.factor(sex)))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "quasipoisson"))

min(log(repro$reproduction+0.1))
min(log(repro$reproduction+0.01))
min(log(repro$reproduction+0.0001))


harr <- read.csv("Data/Harassment.Data.csv")
str(harr)
head(harr)

glmnm <- glm(Sum.of.Victim_numb ~ 1 + Treatment, family = poisson, data=harr)
summary(glmnm)
exp(-0.01179)

library(lme4)
harr$obsid <- 1:nrow(harr)
glmmnm <- glmer(Sum.of.Victim_numb ~ 1 + Treatment * Phase + (1|Farm) + (1|obsid),
                family = poisson, data=harr)
summary(glmmnm)
plot(glmmnm)
