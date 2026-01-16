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

vars <- c("year", "trust", "sex", "age", "partyid", "wrkstat", "happy", "degree", "realinc")
sub <- GSS[, vars]

sub <- mutate(gss_for_ts, 
              ntrust = ifelse(trust == 1, 1, 0), 
              baplus = ifelse(degree >= 3, 1, 0),
              happiness = ifelse(happy == 1, 1, 0),
              income = realinc)


# get means by year
by.year <- aggregate(subset(sub, sel = -year), list(year = sub$year), mean, na.rm = T)

# interpolate for some missing years
# add the extra years
by.year[30:40, "year"] <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
by.year <- dplyr::arrange(by.year, year)

# make a time series object by.year.ts and interpolate using na.approx
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# calculate pct strong republican, percent fulltime, percent under 50 with BA
by.year.ts <- as.data.frame(by.year.ts)
by.year.ts <- mutate(by.year.ts, 
                     happy_pct = happiness*100,
                     trust_pct = ntrust*100,
                     ba_pct = baplus*100)

# only keep up to 1992 and convert back to time series object
# by.year.ts <- ts(subset(by.year.ts, year <= 1992))

by.year.ts

# correlations
cor.vars <- c("trust_pct", "happy_pct", "ba_pct", "age", "income", "year")
cor.dat <- by.year.ts[, cor.vars]


#install.packages("corrplot")
library(corrplot)
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
keep.vars <- c("year", "trust_pct", "happy_pct", "age", "ba_pct", "income")

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
(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = c("ba_pct")))
(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = c("happy_pct")))
(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = c("income")))
(g_age <- ggMyTS(df = plot.dat, varlist = c("age")))


## 3. Run a simple time series regression, with one X and no trend.  Interpret it.

# simplest regression
lm.trust <- lm(trust_pct ~ ba_pct, data = by.year.ts)
summary(lm.trust)

# test for heteroskedasticity
bptest(lm.trust)

# look for autocorrelation in errors
e <- lm.trust$resid
acf(e) 
acf(e, xlim = c(1,8), col = "red", lwd = 2) # can also customize acf output
plot(e) # plot residuals over time
dwtest(lm.trust) # Durbin-Watson test
bgtest(lm.trust) # Breusch-Godfrey test
durbinWatsonTest(lm.trust, max.lag=3) # Durbin-Watson with more lags

## 4. Run a time series regression with one X and trend.  Interpret it.  Perform autocorrelation diagnostics.  Explain what you found.

# include year trend
lm.trust2 <- update(lm.trust, ~ . + year)
summary(lm.trust2)

# look for autocorrelation
e2 <- lm.trust2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2)
pacf(e2, xlim = c(1,8), col = "red", lwd = 2)
plot(e2)
dwtest(lm.trust2)
bgtest(lm.trust2)
durbinWatsonTest(lm.trust2, max.lag=3)

## 5. Consider running a time series regression with many Xs and trend.  Interpret that.  Check VIF.

# add some more predictors
lm.trust3 <- update(lm.trust2, ~ . + age + happy_pct)
summary(lm.trust3)
vif(lm.trust3) # variance inflation factor 
durbinWatsonTest(lm.trust3, max.lag=2)


## Can I get rid of that spike at lag 2? ##
by.year.ts$is.even <- by.year.ts$year %% 2 == 0
lm.trust5 <- update(lm.trust2, ~ . + is.even)
summary(lm.trust5)
vif(lm.trust3) # variance inflation factor 
durbinWatsonTest(lm.trust3, max.lag=2)


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
                       trust_pct = firstD(trust_pct), # using firstD functon from QMSS package
                       age = firstD(age),
                       ba_pct = firstD(ba_pct),
                       happy_pct = firstD(happy_pct),
                       income = firstD(income),
                       year = year)

lm.trust4 <- update(lm.trust2, data = by.yearFD)
summary(lm.trust4)
e4 <- lm.trust4$resid
acf(e4, xlim = c(1,6), col = "red", lwd = 2)
pacf(e4, xlim = c(1,6), col = "red", lwd = 2)

install.packages("forecast")
library(forecast)
auto.arima(e4, trace=TRUE)

## 7. Check your variables for unit roots.  Do some tests.  Interpret them.

adfTest(by.year.ts[,"trust_pct"], lags = 0, type="ct")
adfTest(by.year.ts[,"trust_pct"], lags = 4, type="ct")

# Phillips-Perron test
PP.test(by.year.ts[,"trust_pct"],lshort=TRUE)


# BTW, Solution 1: use Newey & West autocorrelation consistent covariance matrix
# estimator

library(sandwich)
coeftest(lm.trust3, vcov = NeweyWest(lm.trust2, lag = 2))

## 8. Perform an Automatic ARIMA on the residuals from one of your earlier models.  Tell me what it says.

library(forecast)
auto.arima(e2, trace=TRUE)

## 9. Run an ARIMA that follows from Step 7.  Interpret that, too.

xvars.fat <- by.year.ts[,c("ba_pct", "year")]

# ARIMA(0,0,0) = OLS
arima.001 <- arima(by.year.ts[,"trust_pct"], order = c(0,0,1), xreg = xvars.fat)
summary(arima.001)

Box.test(resid(arima.001), lag = 20, type = c("Ljung-Box"), fitdf = 0)




