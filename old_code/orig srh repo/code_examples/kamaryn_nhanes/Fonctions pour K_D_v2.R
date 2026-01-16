############################################
### Regression entre BA et les biomarqueurs
############################################
BA_regression <- function(data,variables){
  
  m <- length(variables)
  BA <- data$age_visit
  dat_BM <- data[,variables]
  Reg_Table <- as.data.frame(matrix(NA, m, 4))
  colnames(Reg_Table) <- c("intercept", "slope", "sd_residual", "r" )
  
  for(j in 1:m){
    
    X_j <- dat_BM[,j] #valeur de BM j pour chaque personnne
    BA_lm <- lm(dat_BM[,j]~BA)
    Reg_Table$intercept[j] <- BA_lm$coeff[1]
    Reg_Table$slope[j] <- BA_lm$coeff[2]
    Reg_Table$sd_residual[j] <- summary(BA_lm)$sigma
    Reg_Table$r[j] <- cor.test(X_j, BA)$estimate
  }
  
  return(Reg_Table)  
}


########################################################################
## Variance estime des residus de la regression de BA = CA + R(0, S_BA)
#######################################################################
VAR_BA <- function(data, variables){
  
  n <- nrow(data)
  m <- length(variables)
  CA <- data$age_visit
  dat_BM <- data[,variables]
  BA_e <- K_D_e(data, variables) #Sans l'Age chronologique
  r_char <- r_characteristic(data, variables)
  
  s <- 0
  for(i in 1:n){
    s <- s + (((BA_e[i] - CA[i]) - mean(BA_e - CA))^2)/n
  }
  
  #Variable problématique
  var_ba <- s - ((1 - r_char^2)/r_char^2) * (var(CA)/m)
  
  return(var_ba)
}


###############################################
## r_characteristic
###############################################
r_characteristic <- function(data, variables){
  
  m <- length(variables)
  BA <- data$age_visit
  dat_BM <- data[,variables]
  r_j <- rep()
  
  for(j in 1:m){
    
    X_j <- dat_BM[,j]
    r_j[j] <- cor.test(X_j, BA)$estimate
    
  }
  r_char_num <- sum((((r_j^2)) / (sqrt(1 - r_j^2))))
  r_char_den <- sum((abs(r_j) / (sqrt(1 - r_j^2))))
  r_char <- r_char_num/r_char_den #Un r_char petit peut donner un var_BA<0

  return(r_char)
}


##############################################
## Klemera et Doubal sans age chronologique
#############################################
K_D_e <- function(data, variables){
  
  dat_BM <- data[,variables]
  n <- nrow(data) 
  m <- length(variables) 
  BA_reg <-  BA_regression(data,variables) #FUNCTION Regression entre BA et les biomarqueurs
  q_j <- BA_reg$intercept
  k_j <- BA_reg$slope
  s_j <- BA_reg$sd_residual
  BA_e <- rep()
  
  BA_den <- sum((k_j/s_j)^2)
  
  for(i in 1:n){
    
    BA_num <- 0.0
    
    for(j in 1:m){
      
      X_j <- dat_BM[,j]
      #BA_num <- ((X_j[i] - q_j[j]) * (k_j[j]/s_j[j]))
      #BA_den <- (k_j[j]^2)/s_j[j]
      BA_num <- BA_num + ((X_j[i] - q_j[j]) * (k_j[j]/(s_j[j]^2)))
  
    }
    BA_e[i] <- BA_num / BA_den
  }
  
  return(BA_e)
}

