candies <- read.csv("Data/candies.csv")
str(candies)
summary(candies)
plot(candies$number)
table(candies$number)/nrow(candies)

simulated <- rbinom(n = 100, size = 5, prob = 0.5)
table(simulated)/length(simulated)

mean(candies$number)/5
simulated <- rbinom(n = 100, size = 5, prob = 0.64)
table(simulated)/length(simulated)

dbinom(x = 5, size = 5, prob = .5)


resp <- read.csv(file = "Data/d_respiration.csv")
resp_L <- subset(resp, Variation == "Low")
resp_H <- subset(resp, Variation == "High")

lmL <- lm(rrarea ~ 1 + Plant_type, data=resp_L)
plot(lmL)
plot(lmL, which=2)
plot(lmL, which=3)
plot(lmL, which=4)
summary(lmL)

wheat <- read.csv("Data/wheatyieldPLUS.csv")
summary(aov(Yield ~ Variety, data=wheat, ))
summary(lm2 <- lm(Yield ~ Variety, data=wheat))

anova(lm2, )
library(car)
Anova(lm(Yield ~ Variety, data=search, type=2))

lm2.results<-summary(emmeans(lm2,~Variety))



("Data/Prac1peadata.csv")
ggplot(lm2.results,aes(Variety,emmean, fill=Variety))+
  geom_bar(stat="identity", width=.4)+
  geom_errorbar(aes(ymin =lm2.results$lower.CL,
                    ymax = lm2.results$upper.CL), width=.2)+
  ylim(0,4)+
  geom_point(data=wheat, aes(x=Variety, y=Yield), color="red")+
  labs(y = "Yield")+
  geom_text(aes(x=2.5, y=3.5, label="p=0.001"))






orch <- read.csv("Data/Prac1seedorcharddata.csv")
str(orch)

ggplot(orch, aes(x=seedlot, y=dbh))+geom_point()

summary(lm(dbh ~ seedlot, data=orch))
t.test(dbh ~ seedlot, data=orch)
plot(lm(dbh ~ seedlot, data=orch))

peas <- read.csv("Data/Prac1peadata.csv")
aovpeas <- aov(length~sugar, data = peas)
emmeans(aovpeas, ~sugar)
emmeans(aovpeas, pairwise~sugar)
plot(aovpeas)




#data simulation
samplesize <- 50
predictor <- rnorm(n = samplesize, mean = 2, sd = 3)
hist(predictor)
intercept <- 1
slope <- 0.2
response <- intercept + slope*predictor + runif(n=samplesize, min = -1, max=1)

plot(predictor, response)

dat <- data.frame(predictor, response)

str(dat)

lm0 <- lm(response ~ 1 + predictor, data = dat)
plot(lm0)
