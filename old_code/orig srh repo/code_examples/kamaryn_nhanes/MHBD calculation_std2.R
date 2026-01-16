####To Calculate MHBD
#Define a data set, "dat," that consists only of the variables you want to include (columns) 
  #for difference observations (rows)
  
#If necessary, define another data set, "ref," with the same variables in the same order

#ref will serve as a reference population, if you want to measure dysregulation relative to a population
  #other than your main population of interest (e.g., young healthy ref pop, older main pop)
  
#Use a log- or square-root transformation as necessary

#Variables sensitive to sex (e.g. estrogen or testosterone) should be analyzed in separated analyses
# (i.e. using different reference populations for each sex)
  
mhbd_calc<-function(dat, ref = dat, log = F, std = F){
  
  # Standardize variables by mean and sd of reference population
  for (j in 1:ncol(dat)){
    dat[,j] <- (dat[,j]-mean(ref[,j],na.rm=T))/sd(ref[,j],na.rm=T)
  }
  for (j in 1:ncol(ref)){
    ref[,j] <- (ref[,j]-mean(ref[,j],na.rm=T))/sd(ref[,j],na.rm=T)
  }
  
  dat<-na.omit(dat)
  ref<-na.omit(ref)

  if(nrow(ref)==1){
    warning("The reference matrix must have more than one row")
  } else {
    means <- colMeans(ref)
    cv_mat <- var(ref)
  }	

  if(nrow(dat)==1){
    warning("The function does not work with single-row data")
  } else {
    dat <- as.matrix(dat)
    mhbd <- rep(NA,nrow(dat))
    for (x in 1:nrow(dat)){
      mhbd[x]<- sqrt((dat[x,]-means)%*%solve(cv_mat)%*%(dat[x,]-means))
    }
  }
  if(log==F){
    if(std==F){
      return(mhbd)
    } else if(std==T){
      return(mhbd/sd(mhbd))
    }
  } else if(log==T){
    if(std==F){
      return(log(mhbd))
    } else if(std==T){
      return(log(mhbd)/sd(log(mhbd)))
    }
}}      
#mhbd is now your measure of statistical distance, one measure per observation, based on a multivariate normal assumption for the reference pop
#IMPORTANT: 
  # 1)MHBD is usually approximately log-normally distributed. Transform it to 
      #log(mhbd) if you want to calculate correlations or other 
      #statistical operations requiring a normal distribution
  # 2) The scale of MHBD depends on the number and scale of the variables included. 
      #You can standardize it easily by dividing by the standard deviation, e.g. mhbd <- mhbd/sd(mhbd)
      #or log_mhbd <- log(mhbd)/sd(log(mhbd)) as appropriate


