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

# Analysis ----------------------------------------------------------------

# Summary of sampling
sum(sexnest$nestlings)
sum(sexnest$missing)
84/362
sum(sexnest$male) + sum(sexnest$female)
278/362

aggregate(male/(male+female)  ~ year, sexnest, mean) #summary stats per year

## Did sex ratios differ significantly among years?
#No year x year comparisons significant
glm(cbind(male,female)~year,
    data=sexnest, family="binomial") %>% #summary()
emmeans(~ year) %>% pairs(.)


# Complete cases: only nests where we sexed all nestlings
glm(cbind(male,female)~year,
    data=sexnest[sexnest$round==1 &sexnest$complete==1,], family="binomial") %>% tab_model()
emmeans(~ year) %>% cld.emmGrid()

# Assume all missing nestlings were female
glm(cbind(male,(female+missing))~year,
    data=sexnest[sexnest$round==1,], family="binomial") %>% tab_model()
  #emmeans(~ year) %>% cld.emmGrid() #use emmeans for posthoc tests among groups

# Assume all missing nestlings are male
glm(cbind((male+missing),female)~year,
    data=sexnest[sexnest$round==1,], family="binomial") %>% tab_model()
#emmeans(~ year) #%>% cld.emmGrid()

#Variation in likelihood of fledging, not significant
glmer(fledged~sex+(1|year),
      data=sex, family="binomial") %>% tab_model()

#Males are larger than females
lmer(mass~sex + (1| year), data=sex) %>% tab_model()


## Did sex ratios vary significantly with rain?
glmer(cbind(male,female) ~ cumulative.rain + (1|year), data=sexnest, "binomial") %>%
  tab_model()

# Effect of rain assuming all missing nestlings are female
glm(cbind(male, (female+missing)) ~ cumulative.rain,
    data=sexnest, "binomial") %>%
  tab_model()

# Effect of rain assuming all missing nestlings were male
glm(cbind((male+missing), female) ~ cumulative.rain,
    data=sexnest, "binomial") %>%
  tab_model()

