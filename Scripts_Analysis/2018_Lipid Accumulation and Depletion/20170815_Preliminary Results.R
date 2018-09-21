#Using a Two-Way ANOVA response variable explained by 2 categorical variables
#Using a two-way because I have 2 independent variables and 2 groups in each independent variable (species(BE and UZ) and season(16 and 12))
#Using ANOVA because I would like to measure the variance within and between my groups
#IV: Species and Season
#Groups: b1, b2, a1, a2
#response variable is % lipids of dry mass
#AOV of %  Lipids explained by Speacies:Season

setwd("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/Data/Lipids")
dat = read.csv("20170816_Data Survey.csv", header = T)
summary(dat)

Species=dat$Strain
Lipid=dat$Lipid.Mass
Light=dat$Light.Hours
Wet=dat$WET.Larva
Lean=dat$Lean.Mass
Dry=dat$Lean.Mass

par(mfrow=c(1,1))
plot(Lipid~Species:Light, data = dat)
plot(Lipid~Light, data = dat)
plot(Lipid~Wet, data = dat)
plot(Lipid~Dry, data = dat)
plot(Lipid~Lean, data = dat)

plot(Lean~Species:Light, data = dat)
plot(Lean~Light, data = dat)
plot(Lean~Wet, data = dat)
plot(Lean~Dry, data = dat)
plot(Species~Lean, data = dat)

plot(Light~Species, data = dat)
plot(Dry~Light, data = dat)
plot(Dry~Wet, data = dat)
plot(Dry~Lipid, data = dat)
plot(Dry~Lean, data = dat)

Lipid_light=aov(Lipid ~ Light*Species, data=dat)
Lipid_light_leanwetdry=aov(Lipid ~ Light*Lean*Wet*Dry, data=dat)
Lipid_strain_leanwetdry=aov(Lipid ~ Strain*Lean*Wet*Dry, data=dat)
Lean_strain_lipidleanwetdry=aov(Lean~Strain*Lipid*Wet*Dry, data=dat)

summary(Lipid_light)
summary(Lipid_light_leanwetdry)
summary(Lipid_strain_leanwetdry)
summary(Lean_strain_lipidleanwetdry)

par(mfrow=c(3,3))
plot(Lipid ~ Light*Lean*Wet*Dry, data = dat)
plot(Dry~Species*Light, data = dat)
plot(Wet~Species*Light, data = dat)
plot(Lipid~Species*Light, data = dat)
