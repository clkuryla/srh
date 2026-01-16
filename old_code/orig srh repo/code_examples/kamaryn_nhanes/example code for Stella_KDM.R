#####################################
## Code to calculate Klemer-Doubal biological age using NHANES data
## for Stella
## 22-Oct-2024 KTT
##
#####################################


######
# CLK ADDED

library(here)
library(readxl)  

source(here("code_examples/kamaryn_nhanes/Klemera_et_Doubal_v2.R"))  ##Contains calc for KDM BA distance
source(here("code_examples/kamaryn_nhanes/Fonctions pour K_D_v2.R")) ##Helper functions for KDM BA

nh <- read_csv(here("code_examples/kamaryn_nhanes/nhanes_1999-2018_2023-11-29.csv"))

#####

## Clear the environment
rm(list = ls())


# Source Functions in Other Files / Load libraries

library(readxl)                   ##To read a *.XLSX file
#source("Klemera_et_Doubal_v2.R")  ##Contains calc for KDM BA distance
#source("Fonctions pour K_D_v2.R") ##Helper functions for KDM BA

#############
##### NHANES 

## define the directory where the NHANES data is located (CHANGE THIS FOR YOUR DIRECTORIES)
#nhDir <- "C:/Users/ktt2128/Cris Dropbox/Kamaryn Tanner/KT Work/Columbia/Cohen Lab/Data/NHANES/"

## read the data file
#nh <- read.csv(paste0(nhDir, "nhanes_1999-2018_2023-10-30.csv.gz"))

# In NHANES, 2nd visit was measured 3 days after in a small subset of subjects, so restrict to first visit 
# Also, we often exclude children (development different from aging). 
nh2 <- nh[nh$age_visit >= 20 & nh$visit==1, ]        

# In NHANES, the creatinine variable is separated in two variables with different names (two different lab assays). 
# We use the one with the lowest number of NAs and change its name to match the one in other datasets.
nh2$creat <- ifelse(is.na(nh2$creat_d) & is.na(nh2$creat_x), NA, 
                    ifelse(!is.na(nh2$creat_d), nh2$creat_d, nh2$creat_x))


## Choose biomarkers for KDM BA.
## NOTE: The considerations here are different.  I recommend reading some papers.  Basically,
##       you want biomarkers that are correlated with chronological age AND are not correlated
##       with each other.
## I'm borrowing this list from a past analysis
vars <- c("alb", "alt", "bun", "chol", "hb", "hct", "k", "ldl", "lympho_p", "mch", "mcv", "mono_p", "neut_p", 
                   "pltlt", "rbc", "rdw") 
             

##################
##### Prepare for KDM BA

# Restrict the dataset to complete cases and keep only necessary columns
nh3 <- na.omit(nh2[,c("age_visit", vars)])
# Make sure everything looks correct
head(nh3)


##################
##### KDM calculation

# We have standard lab code for the calculation of KDM BA that has been tested. Its best to use this.
# The function is Klemera_Doubal and it has two arguments:
#  data = the complete dataset for use. In this case, nh3
#  variables = the set of biomarkers to be used
#
nh3$BA <- Klemera_Doubal(data=nh3, variables = vars)
# We often use the difference between biological and chronological age in our analyses.
nh3$BA_diff <- nh3$BA - nh3$age_visit



par(mfrow=c(1,3))
hist(nh3$BA_diff)
head(nh3)
summary(nh3$BA_diff)

# Is it correlated with age?
plot(x=nh3$age_visit, y=nh3$BA_diff, ylab="KDM BA", xlab="Age (years)")
lines(abline(lm(nh3$BA_diff~nh3$age_visit),col="plum", lwd=3) )
cor.test(nh3$BA_diff, nh3$age_visit)




