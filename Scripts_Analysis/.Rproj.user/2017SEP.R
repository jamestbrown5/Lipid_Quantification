---
  title: "Visualizing July Lipid Data"
output: html_document
editor_options: 
  chunk_output_type: console
---
  
  ## loading libraies
  ```{r}
library(tidyr)
library(ggplot2)
library(plyr)
library(MASS)
library(readxl)


```
## reading in the data and looking at its structure
```{r}
setwd("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/GitHub/Lipid_Quantification/Data")
data=read_excel("/Users/JamesB/Google Drive/Graduate School/Self_JamesTBrown/GitHub/Lipid_Quantification/Data/Lipid Extraction Samples/20171207 Samples RAW data.xlsx", sheet = "2017SEP")
str(data) #shows the structure of the data
```

## plotting out relevant data
there is a problem because the data is in column format. 
Especially for ggplot so we need to convert the trait data into 
columns and make it into 2 columns. 1 with trait ID and the 
other with trait measurements. this aloows us to call on 
the trait id column to plot.

Goal: plot larval data against treat
```{r}
#converting wide to long
dat.long=gather(data,traits,measurement,wet_mass:lipid_mass)

## here are the plots
## all the traits against colony info and "traits" as boxplot
ggplot(dat.long,aes(x=treat,y=measurement))+
  geom_boxplot()+
  facet_grid(traits~.,scales="free")

## all the traits against colony info and treat as boxplot
ggplot(dat.long,aes(x=treat,y=measurement))+
  geom_boxplot()+
  facet_grid(traits~.,scales="free")


## all the traits against treatment  as boxplot
ggplot(dat.long,aes(x=treat,y=measurement,colour=treat))+geom_boxplot()+facet_grid(traits~.,scales="free")

````

````{r}
## The response of each trait within each treatment (read:strain and photoperiod)
ggplot(dat.long,aes(x=treat,y=measurement,colour=treat))+
  theme_classic()+
  geom_boxplot()+
  ggtitle("Trait Measurements")+
  scale_colour_discrete(name="Strain x PP")+
  facet_grid(traits~.,scales="free")
````



##Exploring the stats using ANOVA
````{r}
###making blocks as a factor and making treatment a numerical value

data1=subset(data,data$treat!="BE12")
data2=subset(data1,data1$treat!="BE16")
data3=subset(data,data$treat!="BE16")
data4=subset(data3,data3$treat!="BE16")

##constructing an aov model for UZ
modaUZ=aov(lipid_mass~treat+treat+Rep+lean_mass,data=data2)
summary(modaUZ)
modbUZ=aov(lipid_mass~treat*treat*Rep*lean_mass,data=data2)
summary(modbUZ)

##constructing an aov model for BE
modaBE=aov(lipid_mass~treat+treat+Rep+lean_mass,data=data4)
summary(modaBE)
modbBE=aov(lipid_mass~treat*treat*Rep*lean_mass,data=data4)
summary(modbBE)
`````
##Exploring the Stats using AIC
`````{r}
## checking model visualy

mod1=stepAIC(modaUZ, direction="both")
summary(mod1)

mod2=stepAIC(modbUZ, direction="both")
summary(mod2)

mod3=stepAIC(modaBE, direction="both")
summary(mod3)

mod4=stepAIC(modbBE, direction="both")
summary(mod4)

````
###Calculating the means. Visualized using boxplots 
````{r}
mean.dat=ddply(data,.(treat,Cohort),summarize,mean=mean(Lipid.Wt))
ggplot(data,aes(x=treat,y=Lipid.Wt,colour=Cohort))+geom_boxplot()+geom_line(data=mean.dat,aes(x=as.numeric(treat),y=mean,colour=Cohort))
````