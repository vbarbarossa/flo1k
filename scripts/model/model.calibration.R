#sbatch --array=1-3

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(var.n in nodenr:nodenr){
  
  pkgs <- c('raster','nnet','foreach','doParallel','rgdal','ggplot2','gridExtra','maps')
  invisible(suppressMessages(suppressWarnings(lapply(pkgs, require, character.only = T))))
  
  # set working directory
  wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
  wd.old <- '/vol/milkun3/Valerio/FLO1K/'
  
  folder.dyn.var = paste0(wd,"GRDC.DATA/DYN/")
  
  #detect number of cores for parallel calculations
  NC <- 20
  
  #scripts directory
  folder.scripts <- paste0(wd,'scripts/')
  folder.scripts.model <- paste0(wd,'scripts/model/')
  
  source(paste0(folder.scripts,'functions_generic.R'))
  
  # load staistical functions rmse, r.squared, d, E
  source(paste0(folder.scripts,'stats.functions.R'))
  
  # load functions for the model
  source(paste0(folder.scripts.model,'model.functions.R'))
  
  # user settings
  source(paste0(folder.scripts.model,'model.settings.R'))
  
  #create sequances to extract Q.obs and climatic data
  seq = (time_span[1]-Q.obs.start+2):(time_span[2]-Q.obs.start+2)
  seq.extr = c(1,seq)
  
  seq.clim = (time_span[1]-clim.start+2):(time_span[2]-clim.start+2)
  seq.extr.clim = c(1,seq.clim)
  
  
  Qname = Qnames[var.n]
  
  
  ### 10-FOLD CV ########################################
  
  #folder for storing partial calibration results
  folder.store.cal = dir_(paste0(folder.tabs,'fitting/'))
  
  # set up dataframes to feed to the SVM
  # initialize lists
  final.models.list = list()
  final.models.list.noproc = list()
  model.input.list.train = list()
  model.input.list.test = list()
  proc.list.final.models = list()
  proc.list.cal = list()
  
  #resample 10 years from each station rep.cal.max times
  for(rep in 1:rep.cal.max){
    
    #resample
    source(paste0(folder.scripts.model,'model.data.filtering.noplots.R'))
    
    model.input.tot = database.random.obs(read.csv(paste0(folder.tabs,Qname,'.GRDC.obs.filtered.csv')),
                                          read.csv(paste0(folder.tabs,Qname,'.static.filtered.data.csv')))
    
    # store overall model
    final.models.list.noproc[[rep]] <- model.input.tot
    
    # pre-process variables: normalize and scale
    proc <- caret::preProcess(model.input.tot,method = c('BoxCox','center','scale'), fudge = 0.1 )
    proc.list.final.models[[rep]] <- proc
    
    final.models.list[[rep]] <- predict(proc, model.input.tot)
    
    # store train-test splitted data
    model.input.tot$fold <- k.folds(df=model.input.tot,n.folds=n.folds)
    
    for(j in 1:n.folds){
      index <- (rep-1)*n.folds + j
      
      model.input.list.train[[index]] <- predict(proc, model.input.tot[model.input.tot$fold != j, ] )
      model.input.list.test[[index]] <- predict(proc, model.input.tot[model.input.tot$fold == j, ] )
      
      proc.list.cal[[index]] <- proc
    }
    
  }
  
  #read best hyperparameters from tuning
  size.vec <- c(rep(read.csv(paste0(folder.tabs,Qname,'.tuning.results.best.par.csv'))$size,length(model.input.list.train)))
  
  ## for diagnostics ############################################
  #cost.vec <- rep(1,40)
  #eps.vec <- rep(0.1,40)
  #r.tr <- sample(1:nrow(model.input.list.train[[1]]),5000)
  #r.te <- sample(1:nrow(model.input.list.test[[1]]),500)
  #model.input.list.train <- lapply(model.input.list.train,function(x) x[r.tr,] )
  #model.input.list.test <- lapply(model.input.list.test,function(x) x[r.te,] )
  ###############################################################
  
  #CV of the SVM model for the rep.cal.max resampled dfs
  out.list <- mcmapply(nn.tenfold,train=model.input.list.train,test=model.input.list.test,
                       fml = model.string,size=size.vec,proc=proc.list.cal,
                       mc.cores=20, SIMPLIFY = FALSE)
  
  # merge model with raw data filtered and store it for further analyses if needed
  # out.df <- lapply(lapply(out.list,function(x) x[[2]]),function(x) merge(x,read.csv(paste0(folder.tabs,Qname,'.static.filtered.data.csv')),by='grdc_no') )
  # names(out.df) <- paste0('rep.',rep(1:rep.cal.max,each=n.folds))
  # save( out.df,file=paste0(folder.store.cal,Qname,'.NN.10fold.df.',rep.cal.max,'rep.RData') )
  # 
  
  save( proc.list.final.models,file=paste0(wd,Qname,'.NN.10fold.proc.',rep.cal.max,'rep.RData') )
  
  #compute median and store results
  out <- do.call("rbind",  lapply(out.list,function(x) x[[1]]))
  
  out.median.list <- lapply(split(out,rep(1:(nrow(out)/n.folds),each=n.folds)),function(x) apply(x,2,median) )
  out.median <- do.call("rbind", out.median.list)
  out.median <- rbind(out.median,apply(out.median,2,median),apply(out.median[1:rep.cal.max,],2,sd),apply(out.median[1:rep.cal.max,],2,mad))
  row.names(out.median) <- c(1:rep.cal.max,'median','SD','MAD')
  
  write.csv(out.median,paste0(folder.tabs,Qname,'.tenfold.TOTAL.csv'))
  
  
  ### run final models #############################
  #lump together data frames
  save(final.models.list,file=paste0(folder.store.cal,Qname,'.NN.df.',rep.cal.max,'rep.RData'))
  save(final.models.list.noproc,file=paste0(folder.store.cal,Qname,'.NN.df.noproc.',rep.cal.max,'rep.RData'))
  
  #read best hyperparameters
  size.vec <- c(rep(read.csv(paste0(folder.tabs,Qname,'.tuning.results.best.par.csv'))$size,length(final.models.list)))
  
  final.models.fit <- mcmapply(nnet::nnet, 
                               formula = lapply(as.list(rep(model.string,length(final.models.list))),function(x) as.formula(x)),
                               data = final.models.list, size=size.vec, decay = 0.01, linout=TRUE, maxit=100000,
                               mc.cores=10,SIMPLIFY = FALSE)
  
  save(final.models.fit,file=paste0(wd,Qname,'.NN.fit.',rep.cal.max,'rep.RData'))
  
}
