library(emmeans)
library(ggplot2)


resp <- read.csv("Data/d_respiration.csv")
str(resp)
View(resp)

library(ggplot2)
ggplot(resp,aes(Plant_type,rrarea,colour=Plant_type))+
  geom_point()+facet_wrap(~Variation)

plot(resp$rrarea, x=resp$Plant_type)


resp_H <- subset(resp,Variation == "High")
resp_L <- subset(resp,Variation == "Low")

t.test(rrarea~Plant_type, data=resp_H, var.equal=TRUE)

aov1 <- aov(rrarea~Plant_type, data=resp_H)
summary(aov1)

lm1 <- lm(rrarea~Plant_type, data=resp_H)
summary(lm1)

emmeans(lm1, ~Plant_type)

lmL<-lm(rrarea ~ Plant_type, data = resp_L)
summary(lmL)

plot(lmL)


lm1.results<-summary(emmeans(lm1,~Plant_type))

ggplot(lm1.results,aes(Plant_type,emmean, fill=Plant_type))+
  geom_bar(stat="identity", width=.4)+
  geom_errorbar(aes(ymin =lm1.results$lower.CL, ymax = lm1.results$upper.CL), width=.2)+
  ylim(0,4)+
  geom_point(data=resp_L, aes(x=Plant_type, y=rrarea), color="red")+
  labs(y = "Dark Respiration (units)")+
  geom_text(aes(x=1.5, y=3.5, label="p=0.002"))  
  


wheat2<-read.csv("Data/wheatyieldPLUS.csv")

str(wheat2) #check data types for each variable
View(wheat2) #View data
ggplot(wheat2, aes(Variety, Yield, colour=Variety)) +
geom_point()


lm2<-lm(Yield ~ Variety, data = wheat2)
anova(lm2)
summary(lm2)
plot(lm2)



lm2.results<-summary(emmeans(lm2,~Variety))

ggplot(lm2.results,aes(Variety,emmean, fill=Variety))+
  geom_bar(stat="identity", width=.4)+
  geom_errorbar(aes(ymin =lm2.results$lower.CL, ymax = lm2.results$upper.CL), width=.2)+
  ylim(0,4)+
  geom_point(data=wheat2, aes(x=Variety, y=Yield), color="red")+
  labs(y = "Yield")+
  geom_text(aes(x=2, y=3.8, label="p=0.002"))  












###### Candies #####

rbinom(n = 20, size = 1, prob = 0.5)
pbinom(q = 0, size = 5, prob = 0.5)

rhyper()

phyper(q=6, m = 10, n = 10, k = 10)
