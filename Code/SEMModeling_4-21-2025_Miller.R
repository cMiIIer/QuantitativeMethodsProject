#Data Organization and Loading
library(tidyverse)
library(readxl)
library(data.table)
#Graphing
library(tidySEM)
#Date Analysis
library(lubridate)
#Mixed Effect Linear models
library(glmmTMB)
#SEM Modeling
library(piecewiseSEM)

##Data Loading and Minor formatting
tadpole<- read.csv("Data/Grad_3-14-25_Miller/TadDataCorrected_4-9-2025_Miller")
tadpole<- tadpole[,-1]
tadpole$days[which(tadpole$days == 95)] <- 96 # line up dates for easier pairing
tadpole$days[which(tadpole$days == 0)] <- 1
tadpole$days[which(tadpole$days == 11)] <- 12
chlor<- read.csv("Data/Grad_3-14-25_Miller/ChlDataCorrected_4-9-2025_Miller")
colnames(chlor)[colnames(chlor) == 'Chl.a.concentraion'] <- 'Chl.a.concentration'
chlor<- chlor[,-1]
chlor<- chlor[!chlor$Enclosure==c("M","T"),] # remove middle and tank measurments
chlor_clean <- chlor %>% ## assisted by AI... remove duplicate rows
  group_by(Tank, Treatment, Enclosure, days) %>%
  slice(1) %>%
  ungroup()

systemData <- # pair chlorophyll and tadpole data
  left_join(tadpole, chlor_clean,
            join_by(Tank, Treatment, Enclosure, days),
            relationship = "one-to-one")
systemData<- systemData[,-c(6,11:12,16:17)]

## Data Cleanup
systemData$Chl.a.concentration<- log(systemData$`Chl-a concentraion`) # normalize data
systemData$Tank<- as.factor(systemData$Tank)
systemData$Treatment<- as.numeric(systemData$Treatment)
systemData<- systemData %>% # establish initial tadpole density measurements
  group_by(Treatment, Tank, Enclosure) %>%
  mutate(denI = first(Density.at.t.1))
systemData$days<- as.numeric(systemData$days)
systemData<- ungroup(systemData)
systemData<- as.data.frame(systemData)

## Pair water chemistry
chem<- read.csv("Data/WaterQualityCorrected_4-21-2025_Miller.csv") #water chemistry data
chem<- chem[,-1]
chem$days<- as.numeric(chem$days)
chem$Tank<- as.factor(chem$Tank)
final_data<-left_join(systemData, chem, # join data sets, with mismatched dates
                       join_by("Tank", "Treatment", closest(days>=days)))
final_data<- final_data[,-15] # remove junk data

## Pair frog data for total anuran biomass
frog<- read_excel("Data/Grad_3-14-25_Miller/Frog Data.xlsx")
datesFrog<- ymd(frog$Date) # add days column
frogdays<- yday(datesFrog)-205
frog$days<- frogdays
frog$Tank<- as.factor(frog$Tank)
frog$days <- as.numeric(frog$days)
frog<- frog[-c(95:96),] # remove post-hoc individuals
frog<- frog[,-c(1,7,8)] # remove notes columns and date
frog$`Weight (g)`<- frog$`Weight (g)` %>% replace_na(0) ## change frogs that didn't get massed
frogs_cumulative <- frog %>% # Helped with AI, establish cumulative masses
  arrange(days) %>% 
  group_by(Tank, Enclosure, days, Treatment) %>% 
  summarise(Mass..g.. = sum(`Weight (g)`)) %>% # compress multiple observations per day to one
  ungroup()%>%
  group_by(Tank, Enclosure, Treatment) %>% # regroup to get export frog mass per enclosure
  mutate(cumulative_frog_biomass = cumsum(Mass..g..)) 

final_system_data <- # Helped by AI
  left_join(final_data, frogs_cumulative, 
            join_by(Treatment, Tank, Enclosure, closest(days.x >= days))) %>%
  group_by(Treatment, Tank, Enclosure) %>%
  arrange('days', .by_group = TRUE) %>%
  tidyr::fill(cumulative_frog_biomass, .direction = "down") %>%
  mutate(cumulative_frog_biomass = replace_na(cumulative_frog_biomass, 0)) %>%
  mutate(total_biomass = Total.Weight..g. + cumulative_frog_biomass)
final_system_data$Days<- as.factor(final_system_data$days.x)
final_system_data$TFTotalMass<- sqrt(final_system_data$total_biomass)
#write.csv(final_system_data, file ="../Spring 2025/Quantitatvie Methods//SystemData_4-21-2025_Miller")


#=== PSEM AC1 Modeling ===
m1<- glmmTMB(Chl.a.concentration~Treatment+ Temp + pH +
               (1|Tank)+ #random effects term to deal with spatial effects of tank
               ar1(Days+0|Tank), #temporal auto-correlative term
             data=final_system_data)
m2<- glmmTMB(Current.Density~Chl.a.concentration+denI+Treatment+
               (1|Tank)+ar1(Days+0|Tank),
             data=final_system_data, family = "poisson" )
m3<- glmmTMB(TFTotalMass~Chl.a.concentration+denI+Treatment+Current.Density+Temp+
               (1|Tank)+ar1(Days+0|Tank),
             data = final_system_data)

x<- psem(m1, m2, m3, data=final_data) # Structural equation model

summary(x) # the summary function with pSEM doesn't always work: internal package issue per
# package creator..... use plot function below to tease out effects

plot(
  x,
  return = FALSE,
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "white",
                          width=1.3),
  edge_attrs = data.frame(style = "solid", color = "black"),
  ns_dashed = T, # set insignificant paths as dashed
  alpha = 0.05, # alpha value to evaluate under
  show = "unstd", # standardized coefficients
  digits = 2,
  add_edge_label_spaces = T,
  layout="tree" # change plot layout - some layouts obscure some paths.
    # circle, tree, spring, spring 2, tree2, circle2, kamadakawai, or layout.auto
)

?plot.psem
