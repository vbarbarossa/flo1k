#define function for RMSE
rmse <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  rmse = sqrt(sum((pred-obs)^2)/length(obs))
  return(rmse)
}

#define function for R2
r.squared <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  num = sum((obs-mean(obs))*(pred-mean(pred)))
  den = sqrt(sum((obs-mean(obs))**2))*sqrt(sum((pred-mean(pred))**2))
  rsq = (num/den)**2
  return(rsq)
}

#define function for adjusted coeff of agreement d (Legates and McCabe 1999)
d <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  num = sum(abs(obs-pred))
  den = sum(abs(pred-mean(obs))+abs(obs-mean(obs)))
  d = 1 - num/den
  return(d)
}

#define function for adjusted coeff of efficiency E (Legates and McCabe 1999)
E <- function(obs,pred){
  ex = 2
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  num = sum(abs(obs-pred)**ex)
  den = sum(abs(obs-mean(obs))**ex)
  E = 1 - num/den
  return(E)
}

MSE <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  SE = abs(pred-obs)/obs
  MSE = mean(SE,na.rm=TRUE)
  return(MSE)
}

# Kling-Gupta Efiiciency (KGE)
KGE <- function(obs,sim){
  obs = as.numeric(obs)
  sim = as.numeric(sim)
  # correlation coefficient
  CC = cov(sim,obs)/(sd(sim)*sd(obs))
  # bias ration
  BR = mean(sim)/mean(obs)
  # relative variability
  RV = (sd(sim)/mean(sim))/(sd(obs)/mean(obs))
  return(data.frame(
    CC = CC,
    BR = BR,
    RV = RV,
    KGE = 1-sqrt((CC-1)**2+(BR-1)**2+(RV-1)**2)
  ))
}










