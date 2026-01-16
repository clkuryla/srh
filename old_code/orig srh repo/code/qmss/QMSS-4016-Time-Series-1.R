## Time Series Lab




## install.packages("devtools", dependencies = TRUE)
## library(devtools)
## install_github("jgabry/QMSS_package")
## install.packages("ggplot2")
## install.packages("plyr")
## install.packages("car")
## install.packages("data.table")

#load packages
##library(QMSS)
library(ggplot2)
library(plyr)
library(car)
library(lmtest)
library(MASS)
library(sandwich)

### Select variables ###

library(data.table)

## pick the gss-trends.csv dataset

vars <- c("year", "trust", "sex", "age", "partyid", "wrkstat", "degree")
GSS <- data.table::fread(
  file.choose(),
  sep = ",",
  select = vars,
  data.table = FALSE)
sub <- GSS[, vars]

sub = as.data.frame(sub)

sub <- mutate(sub, 
              ntrust = ifelse(trust == 1, 1, 0), 
              baplus = ifelse(degree >= 3, 1, 0),
              degreelt50 = ifelse(baplus == 1 & age <50, 1, 0),
              partyid6 = ifelse(partyid == 6, 1, 0),
              partyid7 = ifelse(partyid == 7, 1, 0))

## My QUESTION is: Are trust and education related over time in the way they are related cross-nationally and at the individual level?

# get means by year
by.year <- aggregate(subset(sub, sel = -year), list(year = sub$year), mean, na.rm = T)

# interpolate for some missing years
# add the extra years
by.year[30:40, "year"] <- c(1979, 1981, 1992, 1995, seq(1997, 2009, 2))
by.year <- arrange(by.year, year)

# make a time series object by.year.ts and interpolate using na.approx
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

# calculate pct strong republican, percent fulltime, percent under 50 with BA
by.year.ts <- as.data.frame(by.year.ts)
by.year.ts <- mutate(by.year.ts, 
                     repub = partyid6 + partyid7,
                     repub_pct = repub*100,
                     degreelt50_pct = degreelt50*100)

# only keep up to 1992 and convert back to time series object
# by.year.ts <- ts(subset(by.year.ts, year <= 1992))

# correlations
cor.vars <- c("ntrust", "repub_pct", "degreelt50_pct", "age", "year")
cor.dat <- by.year.ts[, cor.vars]
cor(cor.dat, use = "complete")


# Time series plots with ggplot  ------------------------------------------

# First install the reshape2 package if not already installed
# install.packages("reshape2")

# Make a character vector naming the variables we might want to plot
keep.vars <- c("year", "ntrust", "repub_pct", "age", "degreelt50_pct")

# Use meltMyTS to transform the data to a 3-column dataset containing a column
# for time, a column for variable names, and a column of values corresponding to
# the variable names


#' Use \pkg{reshape2}'s \code{melt} on multivariate time series object
#' 
#' \code{meltMyTS} is primarily designed to prepare data to be used with  
#' \code{\link[QMSS]{ggMyTS}} (\pkg{QMSS}) to make plots of time series trends
#' with \code{\link[ggplot2]{ggplot}}.
#'
#' @param mv.ts.object A multivariate time series object created with \code{\link[stats]{ts}}.
#' @param time.var A character string naming the time variable.
#' @param keep.vars An optional character vector with names of variables to keep. 
#' If \code{keep.vars} is not specified then all variables in \code{mv.ts.object} 
#' will be kept. However, if any variables are named then all other variables will
#' be dropped, except \code{time.var}, which is always kept.   
#' @return A molten data frame.
#' @author Jonah Gabry <jsg2201@@columbia.edu>. See \code{\link[reshape2]{melt}} 
#' in (\pkg{reshape2}) for the author of the original \code{melt} function. 
#' @seealso \code{\link[QMSS]{ggMyTS}}, \code{\link[reshape2]{melt.data.frame}} 
#' @export
#' @examples
#' See examples in documentation for ggMyTS.

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


plot.dat <- meltMyTS(mv.ts.object = by.year.ts, time.var = "year", keep.vars = keep.vars)
plot.dat

# Use ggMyTS to plot any of the variables or multiple variables together

#' Easily plot time series trends with \code{\link[ggplot2]{ggplot}}
#' 
#' \code{ggMyTS} is primarily designed to be used with a data frame created with
#' \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS}).
#'
#' @param df The data frame in which to look for variables to be plotted. Typically 
#' created with \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS})
#' @param varlist A character string or vector naming the variable(s) in \code{df} to plot. 
#' If \code{varlist} is not specified, then all variables in \code{df} will be used. 
#' @param line Should lines be plotted? Defaults to \code{TRUE}. 
#' @param point Should points be plotted? Defaults to \code{TRUE}.
#' @param pointsize Size of the points, if \code{point == TRUE}.
#' @param linewidth Width of the line(s), if \code{line == TRUE}.
#' @param ... Other options that will be passed to \code{\link[ggplot2]{geom_line}} 
#' and \code{\link[ggplot2]{geom_point}}. See examples. 
#' @note At least one of \code{line} or \code{point} must be \code{TRUE}.
#' @return A \code{ggplot} object. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>. See \code{\link[ggplot2]{ggplot}} 
#' for the author of the \code{ggplot} function. 
#' @seealso \code{\link[QMSS]{meltMyTS}}
#' @export
#' @examples
#' \dontrun{
#' keep.vars <- c("year", "n.confinan", "fulltime")        
#' plot.dat <- meltMyTS(mv.ts.object = by.year.ts,
#'                      time.var = "year", keep.vars = keep.vars)
#' ggMyTS(plot.dat, varlist = c("n.confinan", "fulltime"))
#' ggMyTS(plot.dat, "n.confinan", color = "forestgreen", point = F, linetype = 2)                      
#' }
#' 

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

(g_trust <- ggMyTS(df = plot.dat, varlist = c("ntrust")))
(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = c("degreelt50_pct")))
(g_age <- ggMyTS(df = plot.dat, varlist = c("age")))




# simplest regression
lm.trust <- lm(ntrust ~ degreelt50_pct, data = by.year.ts)
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


# include year trend
lm.trust2 <- update(lm.trust, ~ . + year)
summary(lm.trust2)

# look for autocorrelation
e2 <- lm.trust2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2)
plot(e2)
dwtest(lm.trust2)
bgtest(lm.trust2)
durbinWatsonTest(lm.trust2, max.lag=3)


# add some more predictores
lm.trust3 <- update(lm.trust2, ~ . + age)
summary(lm.trust3)
vif(lm.trust3) # variance inflation factor 
durbinWatsonTest(lm.trust3, max.lag=2)


# We don't seem to have serial correlation here, but what if we did?
# Solution 1: use Newey & West autocorrelation consistent covariance matrix
# estimator
coeftest(lm.trust3, vcov = NeweyWest(lm.trust3, lag = 1))

# Solution 2: use the first differences
#' Compute first differences 
#'
#' @param var Variable to be first-differenced.
#' @param group Optional grouping variable (see 'Details').
#' @param df Optional data frame containing \code{var} and \code{group} (see 'Details'). 
#' @details If \code{df} is specified then \code{group} must also be specified. So it is possible 
#' to specify all three parameters, \code{var} and \code{group} only, or \code{var} by itself. 
#' An example of when one might wish to omit both \code{group} and \code{df} is when using \code{firstD} 
#' in conjunction with  \pkg{plyr}'s \code{\link[plyr]{ddply}} (see 'Examples'). If \code{df} is specified then it 
#' should be sorted by \code{group} and, if necessary, a second variable (e.g. time) that orders the 
#' observations of \code{var} in the appropriate sequence. 
#' @return \code{firstD(var)} returns a first-differenced version of \code{var}. 
#' \code{firstD(var,group)} returns a first-differenced version of \code{var} by \code{group}. 
#' And \code{firstD(var,group,df)} returns a first-differenced version of \code{var} by \code{group}, 
#' where \code{var} and \code{group} are searched for in \code{df}. Note that the first value of 
#' \code{firstD(var)} will be \code{NA} since there is no difference to compute. Similarly, for
#' \code{firstD(var,group)} and \code{firstD(var,group,df)} the first value for each group 
#' will be \code{NA}.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' # Specifying both group and df
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10))
#' df$Xdiff <- firstD(X, id, df)
#' df
#' 
#' # Omitting df
#' id <- rep(1:3, each = 3)
#' X <- rpois(9, 10)
#' Xdiff <- firstD(X, id)
#' 
#' # Omitting group and df 
#' \dontrun{
#' library(plyr)
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10), Y = rpois(9, 5))
#' ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
#' }

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

by.yearFD <- summarise(data.frame(by.year.ts),
                       ntrust = firstD(ntrust), # using firstD functon from QMSS package
                       age = firstD(age),
                       degreelt50_pct = firstD(degreelt50_pct),
                       year = year)

lm.trust4 <- update(lm.trust3, data = by.yearFD)
summary(lm.trust4)
e4 <- lm.trust4$resid
plot(e4)
acf(e4, xlim = c(1,6), col = "red", lwd = 2)


