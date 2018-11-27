library(lme4)
library(lmerTest)
photo <- read.csv(file = "Data/Prac4photosynthesis.csv")

m_noblock <- lm(PhotoRate~Temp, data=photo)
anova(m_noblock)



m_block <- lm(PhotoRate~Temp+ as.factor(Position), data=photo)
anova(m_block)

m_block_re <- lmer(PhotoRate~Temp+ (1|Position), data=photo)
anova(m_block_re)
lmerTest::ranova(m_block_re)
summary(m_block_re)


drought <- read.csv("Data/Prac3droughtdata.csv")

drought$Genotype<-relevel(drought$Genotype, ref="WT")
drought$WaterCondition<-relevel(drought$WaterCondition, ref="Normal")


ggplot(drought, aes(x=interaction(Genotype, WaterCondition), y=Temperature, color=as.factor(plant)))+
  geom_point()+xlab("Genotype-by-exposure")

lm.drought <- lm(Temperature ~ Genotype*WaterCondition, data=drought)
anova(lm.drought)

lmer.drought <- lmer(Temperature ~ Genotype*WaterCondition + (1|plant), data=drought)
anova(lmer.drought)


#dark respiration

resp2 <- read.csv("Data/Prac4darkrespiration.csv")
resp2$Day <- sample(x = c(1:5), size = nrow(resp2), replace = TRUE)
write.csv("Data/Prac4darkrespiration.csv", quote = FALSE,x = resp2)

mm0 <- lmer(Dry_mass_resp ~ 1 + Leaf_section+ (1|Species/Plant_ID) + (1|Day), data = resp2)
summary(mm0)
mm0 <- lmer(Dry_mass_resp ~ 1 + Leaf_section+ (1|Day/Leaf_stage/Plant_ID), data = resp2)

mm0 <- lmer(Dry_mass_resp ~ 1 + Leaf_section+ (1|Leaf_stage/Plant_ID/Day), data = resp2)
summary(mm0)
