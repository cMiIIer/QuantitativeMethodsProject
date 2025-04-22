#Data Organization and Loading
library(tidyverse)
library(readxl)
library(data.table)
#Date Analysis
library(nlraa)
library(multcompView)

dry<- read_excel("Data/Grad_3-14-25_Miller/Tadpole Wet and Dry Weight.xlsx")
dry$Treatment[which(dry$Treatment==0.3)]<- 0.4
dry<- dry[-c(11,46,113),]
dry$Water<- dry$`Wet Weight (g)`-dry$`Dry Weight (g)`
dry$WpD<- dry$Water/dry$`Dry Weight (g)`
dry$Dry.Mass..g.<- log(dry$`Dry Weight (g)`)
dry$Wet.Mass..g.<- log(dry$`Wet Weight (g)`)
dry$logWpD<- log(dry$WpD)

dryMod<- lm(data=dry, logWpD~Wet.Mass..g.+as.factor(Treatment))
dryModSum<- summary(dryMod)
round(dryModSum$coefficients, digits=2)

mDW<- lm (data=dry, DpW~Treatment)
summary(mDW)
summary(lm(data=dry, `Dry Weight (g)`~`Wet Weight (g)`*Treatment))
dry$predictedDryMass<- 0.0810788*dry$`Wet Weight (g)`-0.0042290

ggplot(data=dry, aes(`Wet Weight (g)`, WpD, color = as.factor(Treatment)))+geom_point()+
  geom_smooth(method="lm", formula = y~x)+
  facet_wrap(~Treatment)+
  xlab("Wet Mass (g)")+ylab("Percent Dry Mass")

# Convert data frame to data table
setDT(dry)
# Calculate mean points scored by team
dry[, .(mean = mean(`Wet Weight (g)`)), by = Treatment]
dry[, .(mean = mean(`Dry Weight (g)`)), by = Treatment]
dry[, .(mean = mean(DpW)), by = Treatment]
