#Using a Two-Way ANOVA response variable explained by 2 categorical variables
#Using a two-way because I have 2 independent variables and 2 groups in each independent variable (species(BE and UZ) and season(16 and 12))
#Using ANOVA because I would like to measure the variance within and between my groups
#IV: Species and Season
#Groups: UZ12, UZ16, BE12, BE16
#response variable is % lipids of dry mass
#AOV of %  Lipids explained by Speacies:Season

setwd("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/Data/Lipids/USDA BUP")
dat = read.csv("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/Data/Lipids/USDA BUP/20171024 Samples RAW data.csv", header = T)
summary(dat)

#Heteroscad. Testing

par(mfrow=c(2,2))
hist(dat$Lipid.Wt, main="Lipid Mass")
boxplot(dat$Lipid.Wt ~ dat$Colony.Info, main="Lipid Mass")
logLipid=log(dat$Lipid.Wt)
hist(logLipid, main="Log Transformed Lipid Mass")
boxplot(logLipid~dat$Colony.Info, main="Log Transformed Lipid Mass")


boxplot(dat$LEAN.Larvae~dat$Colony.Info, main="Lean Mass")
boxplot(dat$DRY.Larva~dat$Colony.Info, main="Dry Mass")
boxplot(dat$WET.Larva~dat$Colony.Info, main="Wet Mass")
boxplot(dat$Lipid.Wt~dat$Colony.Info, main="Lipid Mass")

hist(dat$LEAN.Larvae, main="Lean Mass")
boxplot(dat$LEAN.Larvae~dat$Colony.Info, main="Lean Mass")
logLean=log(dat$LEAN.Larvae)
hist(logLean, main="Log of Lean Mass")
boxplot(logLean~dat$Colony.Info, main="Log of Lean Mass")

hist(dat$WET.Larva, main="Wet Mass")
boxplot(dat$WET.Larva~dat$Colony.Info, main="Wet Mass")
logWet=log(dat$WET.Larva)
hist(logWet, main="Log of Wet Mass")
boxplot(logWet~dat$Colony.Info, main="Log of Wet Mass")

hist(dat$DRY.Larva, main="Dry Mass")
boxplot(dat$DRY.Larva~dat$Colony.Info, main="Dry Mass")
logDry=log(dat$DRY.Larva)
hist(logDry, main="Log of Dry Mass")
boxplot(logDry~dat$Colony.Info, main="Log of Dry Mass")

par(mfrow=c(1,1))
boxplot(dat$LEAN.Larvae~dat$Block, xlab="Analysis Block", ylab="Lean Mass",main="Distribution of Lean Mass Across Blocks")
boxplot(dat$WET.Larva~dat$Block, xlab="Analysis Block", ylab="Wet Mass",main="Distribution of Wet Mass Across Blocks")
boxplot(dat$DRY.Larva~dat$Block, xlab="Analysis Block", ylab="Dry Mass",main="Distribution of Dry Mass Across Blocks")
boxplot(dat$Lipid.Wt~dat$Block, xlab="Analysis Block", ylab="Lipid Mass",main="Distribution of Lipid Mass Across Blocks")

boxplot(dat$LEAN.Larvae~dat$Cohort, xlab="Cohort", ylab="Lean Mass",main="Distribution of Lean Mass Across Cohorts")
boxplot(dat$WET.Larva~dat$Cohort, xlab="Cohort", ylab="Wet Mass",main="Distribution of Wet Mass Across Cohorts")
boxplot(dat$DRY.Larva~dat$Cohort, xlab="Cohort", ylab="Dry Mass",main="Distribution of Dry Mass Across Cohorts")
boxplot(dat$Lipid.Wt~dat$Cohort, xlab="Cohort", ylab="Lipid Mass",main="Distribution of Lipid Mass Across Cohorts")

Lipid=dat$Lipid.Wt
Species=dat$Colony.Info
both=dat$Cohort
Light=dat$Season
Wet=dat$WET.Larva
Lean=dat$LEAN.Larvae
Dry=dat$DRY.Larva
summary(both)

y=cbind(Lipid,Wet,Lean,Dry)
A=Species
B=Light

allvar_manova=manova(y~A*B)
summary(allvar_manova, test = "Pillai")
summary.aov(allvar_manova)

Dry_datanova=lm(dat$DRY.Larva ~ dat$Season+dat$Cohort, data=dat)
Wet_datanova=lm(dat$WET.Larva ~ dat$Season+dat$Cohort, data=dat)
Lean_datanova=lm(dat$LEAN.Larvae ~ dat$Season+dat$Cohort, data=dat)
Lipid_datanova=lm(dat$Lipid.Wt ~ dat$Cohort, data=dat)

Dry_datanova=aov(dat$DRY.Larva ~ dat$LEAN.Larvae, data=dat)
Wet_datanova=aov(dat$WET.Larva ~ dat$LEAN.Larvae, data=dat)
Lean_datanova=aov(dat$LEAN.Larvae ~ dat$LEAN.Larvae, data=dat)
Lipid_datanova=aov(dat$Lipid.Wt ~ dat$Cohort, data=dat)

TukeyHSD(Lean_datanova)
TukeyHSD(Wet_datanova)
TukeyHSD(Dry_datanova)
TukeyHSD(Lipid_datanova)

summary(Dry_datanova)
summary(Wet_datanova)
summary(Lean_datanova)
summary(Lipid_datanova)

