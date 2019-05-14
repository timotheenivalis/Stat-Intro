lwr <- read.csv("Data/Prac3mockLWR.csv")
summary(lwr)
str(lwr)

library(ggplot2)

ggplot(data = lwr, aes(GeneB, LWR, color=GeneA)) +
  geom_boxplot()+
  geom_point()


addlm <- lm(LWR ~ 1 + GeneA+GeneB, data = lwr)
summary(addlm)

fullfactlm <- lm(LWR ~ 1 + GeneA+GeneB+GeneA:GeneB, data = lwr)
summary(fullfactlm)


plot(fullfactlm)
summary(aov(LWR ~ 1 + GeneA*GeneB, data = lwr))

anova(fullfactlm)

library(emmeans)
emmeans(fullfactlm, pairwise ~ GeneA*GeneB)
emmeans(fullfactlm, pairwise ~ GeneA)
emmeans(fullfactlm, pairwise ~ GeneB)

plot(fullfactlm)


cabb <- read.csv("Data/Prac3cabbagedata.csv")
str(cabb)

ggplot(cabb, aes(x = Date, y=VitC, color=Cult))+geom_boxplot()


ggplot(cabb, aes(x = Cult, y=VitC))+geom_boxplot()

#### Additive ####
cablm <- lm(VitC ~ 1 + Cult + HeadWt * Date, data = cabb)
summary(cablm)
plot(cablm)

emmeans(cablm, pairwise ~ Cult)
emmeans(cablm, ~ Cult)

#### FF ####
plot(lm(VitC ~ 1 + Cult*Date, data = cabb))

#### Tomatoes ####

tom <- read.csv("Data/Prac3droughtdata.csv")

ggplot(tom, aes(x=Genotype, y=Temperature, color=WaterCondition))+
  geom_boxplot()

lmtom <- lm(Temperature ~ 1 + WaterCondition*Genotype, data=tom)
summary(lmtom)
summary(aov(Temperature ~ 1 + Genotype*WaterCondition, data=tom))
anova(lmtom)
summary(aov(Temperature ~ 1 + Genotype+WaterCondition, data=tom))

emmeans(lmtom, pairwise ~ Genotype|WaterCondition)
emmeans(lmtom,  pairwise~ WaterCondition)


x <- rnorm(100)
x2 <- rnorm(100)+x
y <- x + x2 + rnorm(100)

summary(lm0 <- lm(y~x))
summary(lm(residuals(lm0)~ x2))
summary(aov(y~x+x2))



library(ggplot2)
tree <- read.csv("Data/Prac3forest.csv")
str(tree)
ggplot(tree, aes(x = QuadDiam, y=Density,
                 color=StandType)) +
  geom_point()+
  geom_smooth(method="lm")

lm0 <- lm(Density ~ 1, data=tree)
summary(lm0)
plot(lm0)

lm1 <- lm(Density ~ 1 + QuadDiam, data=tree)
summary(lm1)

lm2 <- lm(Density ~ 1 + QuadDiam+StandType, data=tree)
summary(lm2)

lm3 <- lm(Density ~ 1 + QuadDiam*StandType, data=tree)
summary(lm3)
anova(lm3)

1-(711.7 ^ 2 )/var(tree$Density)
library(emmeans)
emmeans(lm3, pairwise ~ StandType)



summary(lm(QuadDiam ~ 1 + Density * StandType, data=tree))

mice <- read.csv("Data/Prac3diabeticmice.csv")
str(mice)
mice[mice$Diet=="HFD" & mice$Strain=="NODk" & mice$Age>9,] <- NA

ggplot(data = mice, aes(x=Age, y= Wt, color=interaction(Diet, Strain))) + geom_point() +
  stat_smooth(formula = y ~ x)

ggplot(data = mice, aes(x=Age, y= Wt, color=interaction(Diet, Strain))) + geom_point() +
  stat_smooth(formula = y ~ x + I(x^2), method = "lm")

summary(mice)

ggplot(data = mice, aes( y= Wt, x=interaction(Diet, Strain))) + geom_boxplot()

lmmice0 <- lm(Wt ~ 1 + Strain*Diet, data=mice)
summary(lmmice0)

mice$agesq <- mice$Age^2

lmmice <- lm(Wt ~ 1 + Strain*Diet+poly(Age, 2), data=mice[])
lmmice <- lm(Wt ~ 1 + Strain*Diet+Age+I(Age^2), data=mice)
summary(lmmice)
plot(lmmice)

lmmicefull <- lm(Wt ~ 1 + Strain*Diet*Age+Strain*Diet*I(Age^2), data=mice)
summary(lmmicefull)
emmeans(lmmicefull, pairwise~interaction(Diet, Strain))
emmeans(lmmice, pairwise~interaction(Diet, Strain))
