## Time Series Lab

## Questions: 1. Create a multivariate time series; perform any interpolations.  
## 2. Graph the relationships between X and Y.  Explain how you think Y should relate to your key Xs.
## 3. Run a simple time series regression, with one X and no trend.  Interpret it.
## 4. Run a time series regression with one X and trend.  Interpret it.  Perform autocorrelation diagnostics.  Explain what you found.
## 5. Consider running a time series regression with many Xs and trend.  Interpret that.  Check VIF.
## 6. Run a first differenced time series regression.  Interpret that.    
## 7. Check your variables for unit roots.  Do some tests.  Interpret them.
## 8. Perform an Automatic ARIMA on the residuals from one of your earlier models.  Tell me what it says.
## 9. Run an ARIMA that follows from Step 8.  Interpret that, too.


GSS = read.csv(file.choose()) ## choose the GSS Trends file.  ##

## install.packages("devtools", dependencies = TRUE)
## library(devtools)
## install_github("jgabry/QMSS_package")
## install.packages("ggplot2")
## install.packages("plyr")
install.packages("car")
install.packages("fUnitRoots")

#load packages
library(QMSS)
library(ggplot2)
library(plyr)
library(car)
library(fUnitRoots)
library(lmtest)

## 1. Create a multivariate time series ##

vars <- c("year", "trust", "sex", "age", "partyid", "wrkstat", "happy", "degree", "realinc", "health", "life", "polviews", "satfin")
sub <- GSS[, vars]

sub <- sub %>% 
       mutate(ntrust = ifelse(trust == 1, 1, 0), 
              baplus = ifelse(degree >= 3, 1, 0),
              happiness = ifelse(happy == 1, 1, 0),
              excellent_health = ifelse(health == 1, 1, 0),
              good_health = ifelse(health == 2, 1, 0),
              fair_health = ifelse(health == 3, 1, 0),
              poor_health = ifelse(health == 4, 1, 0),
              exciting_life = ifelse(life == 1, 1, 0),
              nonextreme_views = ifelse(polviews %in% c(3, 4, 5), 1, 0),
              extreme_views = ifelse(polviews %in% c(1, 2, 6, 7), 1, 0),
              moderate_views = ifelse(polviews == 4, 1, 0),
              cohort = floor(year - age),
              over50 = ifelse(age >= 50, 1, 0),
              boomer = ifelse(cohort >= 1946 & cohort <= 1964, 1, 0) ,
              millenial = ifelse(cohort >= 1981 & cohort <= 1996, 1, 0) ,
              bornin40s = ifelse(cohort >= 1940 & cohort <= 1949, 1, 0),
              sat_w_finances = ifelse(satfin == 1, 1, 0),
              income = realinc
              ) 


# get means by year
by.year <- aggregate(subset(sub, sel = -year), list(year = sub$year), mean, na.rm = T) 

# interpolate for some missing years

# First, add the extra years
unique(sub$year) # years in dataset
extra_years <- setdiff(seq(1972, 2018), unique(sub$year)) # years missing for a continus TS; skip 2020+ because of covid
dim(by.year)[1] # number of years in original data (34)
length(extra_years) # number of years to add (15)
dim(by.year)[1] + length(extra_years) # sum (49)
by.year[35:49, "year"] <- as.vector(extra_years) # add the extra years
by.year <- dplyr::arrange(by.year, year) # arrange by year

# Now make a time series object by.year.ts and interpolate using na.approx
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# calculate pct strong republican, percent fulltime, percent under 50 with BA
by.year.ts <- as.data.frame(by.year.ts)
by.year.ts <- mutate(by.year.ts, 
                     happy_pct = happiness*100,
                     excellent_health_pct = excellent_health*100,
                     exciting_life_pct = exciting_life*100,
                     trust_pct = ntrust*100,
                     extreme_views_pct = extreme_views*100,
                     moderate_views_pct = moderate_views*100,
                     over50_pct = over50*100,
                     boomer_pct = boomer*100,
                     bornin40s = bornin40s*100,
                     sat_w_finances_pct = sat_w_finances*100,
                     millenial_pct = millenial*100,
                     ba_pct = baplus*100)

# get rid of 1972,1973, after 2018 and convert back to time series object
 by.year.ts <- ts(subset(by.year.ts, year >= 1974 & year <= 2018))

by.year.ts

# correlations
cor.vars <- c("trust_pct", "happy_pct", "ba_pct", "age", "income", "year", "excellent_health_pct", "exciting_life_pct")
cor.vars <- colnames(by.year.ts)
cor.dat <- by.year.ts[, cor.vars]


#install.packages("corrplot")
#library(corrplot)
corrplot(cor(cor.dat))



## 2. Graph the relationships between X and Y.

# First install the reshape2 package if not already installed
# install.packages("reshape2")
library(reshape2)


meltMyTS <- function(mv.ts.object, time.var, keep.vars){
  # mv.ts.object = a multivariate ts object
  # keep.vars = character vector with names of variables to keep 
  # time.var = character string naming the time variable
  require(reshape2)
  
  if(missing(keep.vars)) {
    melt.dat <- data.frame(mv.ts.object)
  }
  else {
    if (!(time.var %in% keep.vars)){
      keep.vars <- c(keep.vars, time.var)
    }
    melt.dat <- data.frame(mv.ts.object)[, keep.vars]
  }
  melt.dat <- melt(melt.dat, id.vars = time.var)
  colnames(melt.dat)[which(colnames(melt.dat) == time.var)] <- "time"
  return(melt.dat)
}

# Make a character vector naming the variables we might want to plot
keep.vars <- c("year", "trust_pct", "happy_pct", "age", "ba_pct", "income", "excellent_health_pct", "exciting_life_pct")
keep.vars <- setdiff(colnames(by.year.ts), "year")

# Use meltMyTS to transform the data to a 3-column dataset containing a column
# for time, a column for variable names, and a column of values corresponding to
# the variable names


plot.dat <- meltMyTS(mv.ts.object = by.year.ts, time.var = "year", keep.vars = keep.vars)
plot.dat

# Use ggMyTS to plot any of the variables or multiple variables together


ggMyTS <- function(df, varlist, line = TRUE, point = TRUE, pointsize = 3, linewidth = 1.25, ...){
  require(ggplot2)
  # varlist = character vector with names of variables to use
  if(missing(varlist)){
    gg <- ggplot(df, aes(time, value, colour = variable)) 
  }
  else{
    include <- with(df, variable %in% varlist)
    gg <- ggplot(df[include,], aes(time, value, colour = variable))   
  }
  if(line == FALSE & point == FALSE) {
    stop("At least one of 'line' or 'point' must be TRUE") 
  }
  else{
    if(line == TRUE) gg <- gg + geom_line(size = linewidth, aes(color = variable), ...)
    if(point == TRUE) gg <- gg + geom_point(size = pointsize, aes(color = variable), ...)
  }
  
  gg + xlab("") + theme(legend.position = "bottom") + scale_x_continuous(breaks = min(df$time):max(df$time))
} 

(g_trust <- ggMyTS(df = plot.dat, varlist = c("trust_pct")))

(g_happy_pct <- ggMyTS(df = plot.dat, varlist = c("happy_pct")))
(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = c("income")))
ggMyTS(df = plot.dat, varlist = c("excellent_health_pct"))
ggMyTS(df = plot.dat, varlist = c("exciting_life_pct"))
(g_age <- ggMyTS(df = plot.dat, varlist = c("age")))

ggMyTS(df = plot.dat, varlist = c("ba_pct"))
ggMyTS(df = plot.dat, varlist = c("moderate_views_pct"))
ggMyTS(df = plot.dat, varlist = c("extreme_views_pct"))
ggMyTS(df = plot.dat, varlist = c("exciting_life_pct"))
ggMyTS(df = plot.dat, varlist = c("boomer_pct"))
ggMyTS(df = plot.dat, varlist = c("over50_pct"))


## 3. Run a simple time series regression, with one X and no trend.  Interpret it.

# simplest regression
lm.excellent_health <- lm(excellent_health_pct ~ ba_pct, data = by.year.ts)
summary(lm.excellent_health)

# test for heteroskedasticity
bptest(lm.excellent_health)

# look for autocorrelation in errors
e <- lm.excellent_health$resid
acf(e) 
acf(e, xlim = c(1,8), col = "red", lwd = 2) # can also customize acf output
plot(e) # plot residuals over time
dwtest(lm.excellent_health) # Durbin-Watson test
bgtest(lm.excellent_health) # Breusch-Godfrey test
durbinWatsonTest(lm.excellent_health, max.lag=3) # Durbin-Watson with more lags

## 4. Run a time series regression with one X and trend.  Interpret it.  Perform autocorrelation diagnostics.  Explain what you found.

# include year trend
lm.excellent_health2 <- update(lm.excellent_health, ~ . + year)
summary(lm.excellent_health2)

# look for autocorrelation
e2 <- lm.excellent_health2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2)
pacf(e2, xlim = c(1,8), col = "red", lwd = 2)
plot(e2)
dwtest(lm.excellent_health2)
bgtest(lm.excellent_health2)
durbinWatsonTest(lm.excellent_health2, max.lag=3)

## 5. Consider running a time series regression with many Xs and trend.  Interpret that.  Check VIF.

# add some more predictors
lm.excellent_health3 <- update(lm.excellent_health2, ~ . + age + happy_pct)
summary(lm.excellent_health3)
vif(lm.excellent_health3) # variance inflation factor 
durbinWatsonTest(lm.excellent_health3, max.lag=2)


## Can I get rid of that spike at lag 2? ##
by.year.ts$is.even <- by.year.ts$year %% 2 == 0
lm.excellent_health5 <- update(lm.excellent_health2, ~ . + is.even)
summary(lm.excellent_health5)
vif(lm.excellent_health3) # variance inflation factor 
durbinWatsonTest(lm.excellent_health3, max.lag=2)


## 6. Run a first differenced time series regression.  Interpret that.    


firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}

## Use the first differences
by.yearFD <- summarise(data.frame(by.year.ts),
                       excellent_health_pct = firstD(excellent_health_pct), # using firstD functon from QMSS package
                       age = firstD(age),
                       ba_pct = firstD(ba_pct),
                       happy_pct = firstD(happy_pct),
                       income = firstD(income),
                       year = year)

lm.excellent_health4 <- update(lm.excellent_health2, data = by.yearFD)
summary(lm.excellent_health4)
e4 <- lm.excellent_health4$resid
acf(e4, xlim = c(1,6), col = "red", lwd = 2)
pacf(e4, xlim = c(1,6), col = "red", lwd = 2)

#install.packages("forecast")
library(forecast)
auto.arima(e4, trace=TRUE)

## 7. Check your variables for unit roots.  Do some tests.  Interpret them.

adfTest(by.year.ts[,"excellent_health_pct"], lags = 0, type="ct")
adfTest(by.year.ts[,"excellent_health_pct"], lags = 4, type="ct")

# Phillips-Perron test
PP.test(by.year.ts[,"excellent_health_pct"],lshort=TRUE)


# BTW, Solution 1: use Newey & West autocorrelation consistent covariance matrix
# estimator

library(sandwich)
coeftest(lm.excellent_health3, vcov = NeweyWest(lm.excellent_health2, lag = 2))

## 8. Perform an Automatic ARIMA on the residuals from one of your earlier models.  Tell me what it says.

library(forecast)
auto.arima(e2, trace=TRUE)

## 9. Run an ARIMA that follows from Step 7.  Interpret that, too.

xvars.fat <- by.year.ts[,c("ba_pct", "year")]

# ARIMA(0,0,0) = OLS
arima.001 <- arima(by.year.ts[,"excellent_health_pct"], order = c(0,0,1), xreg = xvars.fat)
summary(arima.001)

Box.test(resid(arima.001), lag = 20, type = c("Ljung-Box"), fitdf = 0)




