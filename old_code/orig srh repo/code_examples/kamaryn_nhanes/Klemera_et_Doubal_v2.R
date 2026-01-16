#################################################################
######### Klemera-Doubal Biological Age (avec l'?ge chronologique)
##################################################################
#source("Fonctions pour K_D_v2.R")

Klemera_Doubal <- function(data, variables){
  
  dat_BM <- data[,variables]
  n <- nrow(data) #Nombre d'individus
  m <- length(variables) #Nombre de biomarqueurs dans le calcul de Klemera et Doubal
  CA <- data$age_visit #Chronological age
  BA_reg <-  BA_regression(data,variables) #FUNCTION Regression entre BA et les biomarqueurs
  q_j <- BA_reg$intercept
  k_j <- BA_reg$slope
  s_j <- BA_reg$sd_residual
  S_BA <- VAR_BA(data, variables) #FUNCTION Variance estime des residus de la regression de BA = CA + R(0, S_BA)
  #S_BA <- 1.5e2   #20*VAR_BA(data, variables)     #Un petit valeur donne effectivemnt BA = CA
  
  BA <- rep()
  
  for(i in 1:n){
    BA_num <- CA[i]/S_BA
    BA_den <- (1/S_BA) + sum((k_j/s_j)^2)

    for(j in 1:m){
      X_j <- dat_BM[,j] #Valeurs de BM j 
      BA_num <- BA_num +((X_j[i] - q_j[j]) * (k_j[j]/(s_j[j]^2)))
    }
    BA[i] <- BA_num / BA_den #Avec l'age chronologique
  }
  
 return(BA)
}






