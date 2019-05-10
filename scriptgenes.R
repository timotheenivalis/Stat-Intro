dcd <- read.csv(file = "~/Documents/CanberraPostDoc/teaching/StatThinkingRSB/StatisticalThinkingRSB/Data/genedata.csv")

str(dcd)

dcd <- dcd[dcd$Disease!= "UC",]
nrow(dcd)

dcd$Dlogical <- (-as.integer(dcd$Disease)+2)

vecpval <- vector(length = ncol(dcd)-2)
for(i in 3:ncol(dcd))
{
  glm0 <- glm(dcd$Dlogical ~ dcd[, i], family = "binomial")
  sglm0 <- summary(glm0)
  vecpval[i-2] <- sglm0$coefficients[2,4]
}

plot(-log(vecpval), ylim = c(0,10))
abline(h=-log(0.05), col="red")
abline(h=-log(0.05/1020), col="orange")

0.05*1020
sum(vecpval<0.05)
