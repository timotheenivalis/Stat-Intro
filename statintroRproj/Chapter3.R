library(emmeans)
library(ggplot2)

LWR <- read.csv("Data/Prac3mockLWR.csv")

ggplot(LWR, aes(GeneB,LWR,colour=GeneA)) +
  geom_boxplot() + geom_point()

lmadditive <- lm(LWR ~ GeneA + GeneB, data = LWR)
summary(lmadditive)
anova(lmadditive)

lminteraction <- lm(LWR ~ GeneA * GeneB, data = LWR)
summary(lminteraction)
anova(lminteraction)
emmeans(lminteraction, pairwise ~ GeneA|GeneB)
emmeans(lminteraction, pairwise ~ GeneB|GeneA)

plot(lminteraction)

 # cabbage
cabb <- read.csv("Data/Prac3cabbagedata.csv")

str(cabb)
ggplot(cabb, aes(Date, VitC, colour=Cult))+ geom_boxplot()

boxplot(VitC ~ Date*Cult, data = cabb, at=c(1,3,5,2,4,6))

#tomoato
tom <- read.csv("Data/Prac3droughtdata.csv")

#trees
tree <- read.csv("Data/Prac3forest.csv")

ggplot(tree, aes(QuadDiam, Density, colour=StandType))+geom_point()

ggplot(tree, aes(QuadDiam, Density, colour=StandType))+geom_point() + geom_smooth(method='lm',formula=y~1, se=FALSE, mapping = aes(x=QuadDiam, y=Density), inherit.aes = FALSE)
ggplot(tree, aes(QuadDiam, Density, colour=StandType))+geom_point() + geom_smooth(method='lm',formula=y~1+x, se=FALSE, mapping = aes(x=QuadDiam, y=Density), inherit.aes = FALSE)

ggplot(tree, aes(QuadDiam, Density, colour=StandType))+geom_point()

tree$col <- c(rgb(1,0.5,0.5), rgb(0.1,0.6,0.5,0.8),rgb(0.1,0.1,0.5))[as.numeric(tree$StandType)]
plot(tree$QuadDiam, y=tree$Density, col=tree$col, pch=16, ylab="Density", xlab="Diameter")
lm(Density~QuadDiam, data=tree)
abline()
for(i in levels(tree$StandType))
  {
  abline(lm(Density~QuadDiam, data=tree[tree$StandType==i,]))
}

summary(lm(Density~QuadDiam, data=tree))
summary(lm(Density~QuadDiam+StandType, data=tree))
summary(lm(Density~QuadDiam*StandType, data=tree))
anova(lm(Density~QuadDiam*StandType, data=tree))


mice <- read.csv("Data/Prac3diabeticmice.csv")

ggplot(mice, aes(x = Age, y=Wt, colour=interaction(Strain, Diet)))+geom_point()
