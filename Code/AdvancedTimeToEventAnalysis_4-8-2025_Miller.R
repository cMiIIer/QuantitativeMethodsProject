#Data Loading
library(readxl)
#tidyverse
library(tidyverse)
#Event Analysis and Graphing
library(survival)
library(survminer)
library(ggsurvfit)
library(tidycmprsk)
library(data.table)
library(contsurvplot)

# Data Loading
anura<- read.csv("Data/Grad_3-14-25_Miller/AnuraState_4-8-25_Miller")
odonata<- read.csv("Data/Grad_3-14-25_Miller/OdonateState_4-8-25_Miller")
chl<- read.csv("Data/Grad_3-14-25_Miller/ChlDataCorrected_4-9-2025_Miller")
chemistry<- read_excel("Data/Water Quality Data.xlsx")

## Data Transforming - Water Chemistry 
chemdates<- ymd(chemistry$Date) # Date manipulation
chemdays<- yday(chemdates)-205
chemistry$days<-chemdays
chemistry<- chemistry %>% replace_na(list(Bubbled = "No", Rain="No")) # Fixing NAs
chemistry[464,6]<- 5.70 # correcting outlying point to correct value
chemistry$Tank<- as.factor(chemistry$Tank)
rm(chemdates,chemdays) # remove floating strings

## Creating advanced Anura state data set
anura<- anura %>% separate(Position, into=c("Tank", "Enclosure"), sep = "_")
anura<- anura[,-1] #remove misc column
anura$Tank<- as.factor(anura$Tank)
anura<- left_join(anura, chemistry, # chemistry based time to event analysis
            join_by("Tank", "Treatment", closest(days >=days)))
anura$days.x<- as.integer(anura$days.x)
anura$days<- anura$days.x
anura<- anura[,-c(6,9)]
chl$Tank<- as.factor(chl$Tank) # transforming chlorophyll data set for use
chl<- chl[!chl$Enclosure=="M",] # rid extra measures
chl<- chl[!chl$Enclosure=="T",]
chl$Chl.a.concentration<- log(chl$Chl.a.concentraion)# correction and transformation
chl$days<- as.integer(chl$days)
chl<- chl[,-c(1,4:6)]
chlor_clean <- chl %>% ## assisted by AI... remove duplicate rows
  group_by(Tank, Treatment, Enclosure, days) %>%
  slice(1) %>%
  ungroup()

anura<- left_join(anura, chlor_clean,
                   join_by("Tank", "Treatment", "Enclosure", "days"))

odonata<- odonata %>% separate(Position, into=c("Tank", "Enclosure"), sep = "_")
odonata<- left_join(odonata, chemistry, 
                    join_by("Tank","Treatment", closest(days >=days)))

#write.csv(odonata, file = "../Spring 2025/Quantitatvie Methods/OdonataTTE")

m1<- coxph(formula = Surv(days, status) ~ Treatment, data = anura) # prop. hazards model test
summary(m1)

fit1<- survfit(Surv(days, status) ~ Treatment, data = anura)#graphing info
ggsurvplot(fit1, conf.int = TRUE, legend.labs=c(), # survival graph
           ggtheme = theme_minimal(), ylab="Metamorphosis Probablility",
           fun="event")