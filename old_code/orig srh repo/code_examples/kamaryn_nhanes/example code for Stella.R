#####################################
## Code to calculate DM using NHANES data
## for Stella
## 22-Oct-2024 KTT
##
#####################################

## Clear the environment
rm(list = ls())


# Source Functions in Other Files / Load libraries

library(readxl)                    ##To read a *.XLSX file
source("MHBD calculation_std2.R")  ##Contains calc for Mahalanobis distance
source("var_trsf.R")               ##Function for transforming variables

#############
##### NHANES 

## define the directory where the NHANES data is located (CHANGE THIS FOR YOUR DIRECTORIES)
nhDir <- "C:/Users/ktt2128/Cris Dropbox/Kamaryn Tanner/KT Work/Columbia/Cohen Lab/Data/NHANES/"

## read the data file
nh <- read.csv(paste0(nhDir, "nhanes_1999-2018_2023-10-30.csv.gz"))

# In NHANES, 2nd visit was measured 3 days after in a small subset of subjects, so restrict to first visit 
# Also, we often exclude children (development different from aging). 
nh2 <- nh[nh$age_visit >= 20 & nh$visit==1, ]        

# In NHANES, the creatinine variable is separated in two variables with different names (two different lab assays). 
# We use the one with the lowest number of NAs and change its name to match the one in other datasets.
nh2$creat <- ifelse(is.na(nh2$creat_d) & is.na(nh2$creat_x), NA, 
                    ifelse(!is.na(nh2$creat_d), nh2$creat_d, nh2$creat_x))


## Choose biomarkers based on DM17 in Li et al. (2022)
vars <- c("ggt", "mch", "rdw", "pltlt", "rbc", "hb", "ast", "wbc", "alkp", "baso_p",
                        "tot_prot", "hdl", "ca", "lympho_p", "alb", "k", "vitb12") 
             

##################
##### Prepare for DM calculation

# For DM, we use relatively normally-distributed variables so we often have to transform the data
# For example, 
par(mfrow=c(1,2))
hist(rnorm(n=1000), main="Normal")
hist(exp(rnorm(n=1000)), main="Lognormal")
# We want the data to look like the graph on the left but biomarker data often looks like the plot on the right
# We apply a log transformation to this data to make it more normal and, hopefully, satisfy
# the assumptions of the Mahalanobis distance calculation. Sometimes we must apply a different transformation
# but this is rare.


# USUALLY, the transformation to use is the one indicated in the column "best_transf" in the variable dictionary.
# Its always a good idea to look for yourself
# Take a look at histograms for the biomarkers we picked to see the distribution
# First 9:
par(mfrow=c(3, 3))
for(i in 1:9){
  hist(nh2[,vars[i]], main=vars[i])
}
# ggt looks lognormal and in the variable dictionary, log(x) transformation is suggested
# hb looks fairly normal. In the varible dictionary, the transformation column says x meaning no transformation required
# Next 8:
for(i in 10:17){
  hist(nh2[,vars[i]], main=vars[i])
}


# You can tranform the variables one by one using log()
# Or, if your assessment of what needs to be transformed agrees with the variable dictionary,
# you can do it all at once using this code
var_base = as.data.frame(read_xlsx(paste0(nhDir, "NHANES_variable list_rev2023-02-14.xlsx")))
cont.vars = var_base[var_base$Name_Var %in% colnames(nh2) & var_base$Type=="continuous", "Name_Var"]
nh3 <- nh2 
for (vv in 1:length(cont.vars)) {
  nh3[ , cont.vars[vv]] <- var_trsf(nh3, cont.vars[vv], var_base)
}

# Check that the data has been transformed properly
par(mfrow=c(3, 3))
for(i in 1:9){
  hist(nh3[,vars[i]], main=vars[i])
}
# Next 8:
for(i in 10:17){
  hist(nh3[,vars[i]], main=vars[i])
}

# Restrict the dataset to complete cases and keep only necessary columns
nh3 <- na.omit(nh3[,c("id", "age_visit", "sex", vars)])
# Make sure everything looks correct
head(nh3)


##################
##### DM calculation

# We have standard lab code for the calculation of DM that has been tested. Its best to use this.
# The function is mhbd_calc and it has three arguments:
#  dat = the complete dataset. In this case, nh3
#  ref = the reference population.  We'll use nh3 but this needs to be thought through if there are multiple visits per person
#  log = T/F; We often log-transform DM since it has a ~log-normal distribution (hence the log = T)
#  std = T/F; The scale of DM depends on the number and scale of the variables included. 
#             You can standardize it by dividing by the standard deviation  (std=T)
#
# Note: In the mhbd_calc function, the data is scaled by the mean and sd of the reference population

dat <- nh3[ , vars]
nh3$DM17 <- mhbd_calc(dat = dat, ref = dat, log = T, std=F)

par(mfrow=c(1,3))
hist(nh3$DM17)
head(nh3)
summary(nh3$"DM17")

# Is it correlated with age?
plot(x=nh3$age_visit, y=nh3$DM17, ylab="DM17", xlab="Age (years)")
lines(abline(lm(nh3$DM17~nh3$age_visit),col="plum", lwd=3) )
cor.test(nh3$DM17, nh3$age_visit)

# Would this be any different if the reference population were different?
# Randomly sample 10,000 people from the dataset
nh3$DM17_b <- mhbd_calc(dat = dat, ref = dat[sample(1:nrow(dat), size=10000, replace=F),], log = T, std=F)
plot(x=nh3$age_visit, y=nh3$DM17_b, ylab="DM17_b", xlab="Age (years)")
lines(abline(lm(nh3$DM17_b~nh3$age_visit),col="green", lwd=3) )
cor.test(nh3$DM17_b, nh3$age_visit) #very similar when we use a random sample




