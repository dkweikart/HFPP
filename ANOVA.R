setwd("~/PennState/HFPP_lab")

#Load Packages
library(readxl)
library(dplyr)
library(ggpubr)
library(agricolae)

#oad data for tissue ANOVA analysis
indiv <- read_excel("metadata_nohydro.xlsx")
View(indiv)

#Set Factors
indiv$Mouse <- as.factor(indiv$Mouse)
indiv$Treatment <- as.factor(indiv$Treatment)
indiv$Cage <- as.factor(indiv$Cage)
indiv$MouseNo <- as.factor(indiv$MouseNo)

str(indiv)

####ANOVA
#Compute ANOVA by final weight by Treatment
final.aov <- aov(Final ~ Treatment, data = indiv)
#Summary of the analysis
summary(final.aov)
#Tukey Post Hoc Test
TukeyHSD(final.aov)
#Group based on significance (assigns letters based on sig)#
HSD.test(final.aov, "Treatment", console=TRUE)

#Compute ANOVA by colon length by Treatment
Colon.aov <- aov(Colon ~ Treatment, data = indiv)
summary(Colon.aov)
TukeyHSD(Colon.aov)
HSD.test(Colon.aov, "Treatment", console=TRUE)

#Compute ANOVA by Slope of weight gain (rate) by Treatment
Slope.aov <- aov(Slope ~ Treatment, data = indiv)
summary(Slope.aov)
TukeyHSD(Slope.aov)
HSD.test(Slope.aov, "Treatment", console=TRUE)


#Compute ANOVA by Epi weight by Treatment
Epi.aov <- aov(Epi ~ Treatment, data = indiv)
summary(Epi.aov)
TukeyHSD(Epi.aov)
HSD.test(Epi.aov, "Treatment", console=TRUE)

#Compute ANOVA by Intest weight by Treatment
Intest.aov <- aov(Intest ~ Treatment, data = indiv)
summary(Intest.aov)
TukeyHSD(Intest.aov)
HSD.test(Intest.aov, "Treatment", console=TRUE)


#Compute ANOVA by Retro weight by Treatment
Retro.aov <- aov(Retro ~ Treatment, data = indiv)
summary(Retro.aov)
TukeyHSD(Retro.aov)
HSD.test(Retro.aov, "Treatment", console=TRUE)

#Compute ANOVA by Total weight by Treatment
Total.aov <- aov(Total ~ Treatment, data = indiv)
summary(Total.aov)
TukeyHSD(Total.aov)
HSD.test(Total.aov, "Treatment", console=TRUE)

#Compute ANOVA by Liver weight by Treatment
Liver.aov <- aov(Liver ~ Treatment, data = indiv)
summary(Liver.aov)
TukeyHSD(Liver.aov)
HSD.test(Liver.aov, "Treatment", console=TRUE)

#Compute ANOVA by Spleen weight by Treatment
Spleen.aov <- aov(Spleen ~ Treatment, data = indiv)
summary(Spleen.aov)
TukeyHSD(Spleen.aov)
HSD.test(Spleen.aov, "Treatment", console=TRUE)

#Compute ANOVA by Feed Efficiency by Treatment
Efficiency.aov <- aov(Efficiency ~ Treatment, data = indiv)
summary(Efficiency.aov)
TukeyHSD(Efficiency.aov)
HSD.test(Efficiency.aov, "Treatment", console=TRUE)

#Compute ANOVA by Initial weight by Treatment
Initial.aov <- aov(Initial ~ Treatment, data = indiv)
summary(Initial.aov)
TukeyHSD(Initial.aov)
HSD.test(Initial.aov, "Treatment", console=TRUE)

#Compute ANOVA by Final weight by Treatment
Final.aov <- aov(Final ~ Treatment, data = indiv)
summary(Final.aov)
TukeyHSD(Final.aov)
HSD.test(Final.aov, "Treatment", console=TRUE)

#Compute ANOVA by percent weight gain by Treatment
perc.aov <- aov(perc ~ Treatment, data = indiv)
summary(perc.aov)
TukeyHSD(perc.aov)
HSD.test(perc.aov, "Treatment", console=TRUE)

####ANOVA on Cage feeding data
cage <- read_excel("Meta_cage.xlsx")
View(cage)

#Set Factors
cage$Cage <- as.factor(cage$Cage)
cage$Treatment <- as.factor(cage$Treatment)

str(cage)

Energy.aov <- aov(Energy ~ Treatment, data = cage)
summary(Energy.aov)
TukeyHSD(Energy.aov)
HSD.test(Energy.aov, "Treatment", console=TRUE)

Food.aov <- aov(Food ~ Treatment, data = cage)
summary(Food.aov)
TukeyHSD(Food.aov)
HSD.test(Food.aov, "Treatment", console=TRUE)


####ANOVA on SCFAs
scfa <- read_excel("SCFA_NMR.xlsx", sheet=5)
View(scfa)

#Set Factors
scfa$Sample <- as.factor(scfa$Sample)
scfa$Treatment <- as.factor(scfa$Treatment)

str(scfa)

#Compute ANOVA by Acetate by Treatment
Acetate.aov <- aov(Acetate ~ Treatment, data = scfa)
summary(Acetate.aov)
TukeyHSD(Acetate.aov)
HSD.test(Acetate.aov, "Treatment", console=TRUE)

#Compute ANOVA by Propionate by Treatment
Propionate.aov <- aov(Propionate ~ Treatment, data = scfa)
summary(Propionate.aov)
TukeyHSD(Propionate.aov)
HSD.test(Propionate.aov, "Treatment", console=TRUE)

#Compute ANOVA by Butyrate by Treatment
Butyrate.aov <- aov(Butyrate ~ Treatment, data = scfa)
summary(Butyrate.aov)
TukeyHSD(Butyrate.aov)
HSD.test(Butyrate.aov, "Treatment", console=TRUE)

#Compute ANOVA by Total by Treatment
Total.aov <- aov(Total ~ Treatment, data = scfa)
summary(Total.aov)
TukeyHSD(Total.aov)
HSD.test(Total.aov, "Treatment", console=TRUE)
