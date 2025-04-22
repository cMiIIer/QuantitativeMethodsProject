#Data Organization and Loading
library(tidyverse)
library(readxl)
library(data.table)
#Graphing
library(tidySEM)
#Date Analysis
library(nlraa)
library(multcompView)

dry<- read_excel("Data/Grad_3-14-25_Miller/Tadpole Wet and Dry Weight.xlsx")
dry$Treatment[which(dry$Treatment==0.3)]<- 0.4
dry<- dry[-c(11,46,113),]`
dry$DpW<- dry$`Dry Weight (g)`/dry$`Wet Weight (g)`
mDW<- lm (data=dry, DpW~Treatment)
summary(mDW)
summary(lm(data=dry, `Dry Weight (g)`~`Wet Weight (g)`*Treatment))
dry$predictedDryMass<- 0.0810788*dry$`Wet Weight (g)`-0.0042290

ggplot(data=dry, aes(`Wet Weight (g)`, DpW, color = as.factor(Treatment)))+geom_point()+
  geom_smooth(method="lm", formula = y~log(x))+
  facet_wrap(~Treatment)+
  xlab("Wet Mass (g)")+ylab("Percent Dry Mass")
write.csv(dry, file="Data/dryMass")

# Convert data frame to data table
setDT(dry)
# Calculate mean points scored by team
dry[, .(mean = mean(`Wet Weight (g)`)), by = Treatment]
dry[, .(mean = mean(`Dry Weight (g)`)), by = Treatment]
dry[, .(mean = mean(DpW)), by = Treatment]
