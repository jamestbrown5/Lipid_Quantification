#Using a Two-Way ANOVA response variable explained by 2 categorical variables
#Using a two-way because I have 2 independent variables and 2 groups in each independent variable (species(BE and UZ) and season(16 and 12))
#Using ANOVA because I would like to measure the variance within and between my groups
#IV: Species and Season
#Groups: b1, b2, a1, a2
#response variable is % lipids of dry mass
#AOV of %  Lipids explained by Speacies:Season

setwd("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/Data/Lipids/Visual Data")
dat = read.csv("20170816_R Data first pass.csv", header = T)
summary(dat)

qqnorm(dat$X.Total.Lipids.Dry.Mass.)
qqline(dat$X.Total.Lipids.Dry.Mass., col = "red")
logDRY=log(dat$X.Total.Lipids.Dry.Mass.)
qqnorm(logDRY)
qqline(logDRY, col = "red")


qqnorm(dat$Total.Lipid.WET.Mass)
qqline(dat$Total.Lipid.WET.Mass, col="red")
logWET=log(dat$Total.Lipid.WET.Mass)
qqnorm(logWET)
qqline(logWET, col="red")

qqnorm(dat$Lean.Mass.Dry.Mass)
qqline(dat$Lean.Mass.Dry.Mass, col="red")
boxplot(dat$Total.Lipid.WET.Mass, dat$X.Total.Lipids.Dry.Mass., dat$Lean.Mass.Dry.Mass)

Dry=dat$X.Total.Lipids.Dry.Mass.
Species=dat$Species
Season=dat$Season
Wet=dat$Total.Lipid.WET.Mass
Lean=dat$Lean.Mass.Dry.Mass


Dry_datanova=aov(Dry ~ Species+Season+Species:Season, data=dat)
Wet_datanova=aov(Wet ~ Species+Season+Species:Season, data=dat)
Lean_datanova=aov(Lean ~ Species+Season+Species:Season, data=dat)

summary(Dry_datanova)
summary(Wet_datanova)
summary(Lean_datanova)

