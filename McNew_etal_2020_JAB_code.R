# Sex ratio analysis
# JAB paper McNew et al. 2020
# See also: simulation of missing data in R markdown
#
library(glmmTMB)
library(lme4)
library(lmerTest)
library(plotrix)
library(emmeans)
library(MASS)
library(dplyr)
library(car)
library(sjPlot)


# Data --------------------------------------------------------------------

#Each row is a nestling
sex <- read.csv("./CSVs/nestling-sex.csv")
sex$round<-as.factor(sex$round)
sex$year<-as.factor(sex$year)
sex$male[sex$sex=="M"]<-1
sex$male[sex$sex=="F"]<-0
sex$sex[sex$sex==""] <-NA
sex<-droplevels(sex)

#Each row is a nest
sexnest <- read.csv("./CSVS/sexpernest.csv")
sexnest$hatch.date <- as.Date(sexnest$hatch.date, format= "%m/%d/%y")
sexnest$year <- as.factor(sexnest$year)
sexnest <- sexnest[sexnest$round==1,]

#Metadata for nests

allnests <- read.csv("./CSVs/StudySummary2012-2016.csv")
allnests$year <- as.factor(allnests$year)


# Analysis ----------------------------------------------------------------


