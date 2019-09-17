sampler <- function(x){
  
  n = which(!is.na(x))
  
  sampled = sample(n,min_years_monitored)
  
  x[n[!(n %in% sampled)]] <- NA
  
  return(x)
  
}


#create function to extract climate data
clim_matrix <- function(climavar_name,grdc_sel,seq.extr){
  clim_data = read.csv(paste0(folder.dyn.var,climavar_name,'.csv'))
  clim_data = clim_data[clim_data$grdc_no %in% grdc_sel,seq.extr]
  return(as.matrix(clim_data[,2:ncol(clim_data)]))
}


#create function to extract past climate data
clim_matrix_past <- function(climavar_name,grdc_sel,seq.extr,n){
  seq.extr[2:length(seq.extr)] <- ( seq.extr[2:length(seq.extr)] - n ) #take current - n past year
  clim_data = read.csv(paste0(folder.dyn.var,climavar_name,'.csv'))
  clim_data = clim_data[clim_data$grdc_no %in% grdc_sel,seq.extr]
  return(as.matrix(clim_data[,2:ncol(clim_data)]))
}


#split the data by folds considering stations rather than observations
k.folds <- function(df,n.folds){
	fold <- NA
	ids = unique(df$grdc_no)
	ids <- ids[sample(length(ids))]
	kval <- cut(seq(1,length(ids)),breaks=n.folds,labels=FALSE)
	for(f in 1:length(ids)) fold[ df$grdc_no %in% ids[f] ] <- kval[f]
	return(fold)
} 

back2yeoj <- function(y,lambda,fudge){
  
  #correct for fudge
  if(lambda >= 0-fudge & lambda <= 0+fudge) lambda <- 0
  if(lambda >= 1-fudge & lambda <= 1+fudge) lambda <- 1
  
  if(lambda == 0) res = exp(y)
  if(lambda != 0) res = (y*lambda+1)**(1/lambda)
  
  return(res)
}

back2scale <- function(proc,x_scaled){
  
  #first de-scale
  # x_scaled = (x - mean(x))/sd(x)
  # x = x_scaled*sd(x) + mean(x)
  # read mean and sd from preProcess proc object
  m = as.numeric(proc$mean['Q'])
  s = as.numeric(proc$std['Q'])
  l = as.numeric(proc$bc$Q$lambda)
  f = as.numeric(proc$bc$Q$fudge)
  
  res = sapply( (x_scaled*s+m), function(x) back2yeoj(x,l,f) ) 
  
  return(res)
  
}


nn.tenfold.tune <- function(train,test,fml,size,proc){
  
  mdl <- nnet::nnet(as.formula(fml), data = train, size=size, decay = 0.01, linout=TRUE, maxit=100000)
  
  obs.tr = log10(back2scale(proc,train$Q))
  obs.te = log10(back2scale(proc,test$Q))
  sim.tr = log10(back2scale(proc,mdl$fitted))
  sim.te = log10(back2scale(proc,predict(mdl, test)))
  
  return(data.frame(train.rsq = r.squared(obs.tr,sim.tr), test.rsq = r.squared(obs.te,sim.te),
                    train.rmse = rmse(obs.tr,sim.tr), test.rmse = rmse(obs.te,sim.te)))
  
}

nn.tenfold.tune_extra <- function(train,test,fml,size,decay,proc){
  
  mdl <- nnet::nnet(as.formula(fml), data = train, size=size, decay = decay, linout=TRUE, maxit=100000)
  
  obs.tr = log10(back2scale(proc,train$Q))
  obs.te = log10(back2scale(proc,test$Q))
  sim.tr = log10(back2scale(proc,mdl$fitted))
  sim.te = log10(back2scale(proc,predict(mdl, test)))
  
  return(data.frame(train.rsq = r.squared(obs.tr,sim.tr), test.rsq = r.squared(obs.te,sim.te),
                    train.rmse = rmse(obs.tr,sim.tr), test.rmse = rmse(obs.te,sim.te)))
  
}

  
# K-FOLD CROSS VALIDATION
nn.tenfold <- function(train,test,fml,size,proc){
  
  mdl <- nnet::nnet(as.formula(fml), data = train, size=size, decay = 0.01, linout=TRUE, maxit=100000)
  
  pr <- predict(mdl, test)
  
  data.store <- cbind(rbind(train,test),data.frame(pred = c(mdl$fitted,pr),class = c(rep(1,nrow(train)),rep(2,nrow(test))) ) )
  
  obs.tr = log10(back2scale(proc,train$Q))
  obs.te = log10(back2scale(proc,test$Q))
  sim.tr = log10(back2scale(proc,mdl$fitted))
  sim.te = log10(back2scale(proc,pr))
  
  return(list(data.frame(train.rsq = r.squared(obs.tr,sim.tr), test.rsq = r.squared(obs.te,sim.te),
                         train.rmse = rmse(obs.tr,sim.tr), test.rmse = rmse(obs.te,sim.te)),
			  data.store) )
  
  
}


# function to create the input data frame to feed to the modelling algorithm
database4model <- function(df,Q.obs){
  
  grdc_sel = df[,'grdc_no']
  
  years_cal = ncol(Q.obs)-3
  Qobs = as.matrix(Q.obs[Q.obs$grdc_no %in% grdc_sel,2:(years_cal+1)])
  
  variables_list = list(MAF_av = Qobs, 
						pav = clim_matrix('pav',grdc_sel,seq.extr), tav = clim_matrix('tav',grdc_sel,seq.extr), eav = clim_matrix('eav',grdc_sel,seq.extr),
                        pma = clim_matrix('pma',grdc_sel,seq.extr), pmi = clim_matrix('pmi',grdc_sel,seq.extr), 
                        tma = clim_matrix('tma',grdc_sel,seq.extr), tmi = clim_matrix('tmi',grdc_sel,seq.extr),
						psi = clim_matrix('psi',grdc_sel,seq.extr), esi = clim_matrix('esi',grdc_sel,seq.extr) )
  
  variables_vectorized = data.frame(grdc_no = rep(grdc_sel,years_cal), MAF_av = c(variables_list[['MAF_av']]), 
                                    pav = c(variables_list[['pav']]), tav = c(variables_list[['tav']]), pma = c(variables_list[['pma']]), 
                                    pmi = c(variables_list[['pmi']]), tma = c(variables_list[['tma']]), tmi = c(variables_list[['tmi']]),
                                    psi = c(variables_list[['psi']]), eav = c(variables_list[['eav']]), esi = c(variables_list[['esi']]))
  
  df_rep = as.data.frame(do.call("rbind", replicate(years_cal, df, simplify = FALSE)))
  
  row.names(variables_vectorized) <- 1:(years_cal*nrow(df))
  data_rep = cbind(variables_vectorized,df)
  data = data_rep[!is.na(data_rep$MAF_av),]
  
  model.df = create_database(data)
  
  return(model.df)
  
}

database.random.obs <- function(Q.data,variables.data){
  
  # select min_years_monitored random years per station
  Q.data[,2:(ncol(Q.data)-2)] = t(apply(Q.data[,2:(ncol(Q.data)-2)],1,function(x) sampler(x)))
  
  # matches climate data with observed Q and create the 1-year observations database
  return(database4model(variables.data,Q.data))
  
  
}
