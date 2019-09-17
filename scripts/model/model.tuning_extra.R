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
  NC <- 30
  
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
  
  #store the results of the tuning
  folder.store.tuning = paste0(folder.tabs,'tuning/')
  if (!dir.exists(folder.store.tuning)) dir.create(file.path(folder.store.tuning))
  
  ### TUNING ############################################
  
  source(paste0(folder.scripts.model,'model.data.filtering.noplots.R'))
  
  model.input.tot = database.random.obs(read.csv(paste0(folder.tabs,Qname,'.GRDC.obs.filtered.csv')),
                                        read.csv(paste0(folder.tabs,Qname,'.static.filtered.data.csv')))
  
  
  proc <- caret::preProcess(model.input.tot,method = c('BoxCox','center','scale'), fudge = 0.1 )
  
  #density plots
  save(model.input.tot,file = paste0(folder.store.tuning,Qname,'.input.tot.RData'))
  input.to.melt = predict(proc,model.input.tot)[,2:ncol(model.input.tot)]
  input.melted = reshape::melt(input.to.melt)
  density.plot <- ggplot2::ggplot(input.melted, aes (value)) +
    geom_density() +
    facet_wrap(~variable)
  ggsave(paste0(folder.store.tuning,Qname,'.dendity.plot.tiff'),plot = density.plot)
  
  
  parms <- expand.grid(size = size.val, decay = decay.val)
  
  #split the data
  model.input.list.train = list()
  model.input.list.test = list()
  
  model.input.tot$fold <- k.folds(df=model.input.tot,n.folds=n.folds)
  
  for(j in 1:n.folds){
    
    model.input.list.train[[j]] <- predict( proc, model.input.tot[model.input.tot$fold != j, ] )
    model.input.list.test[[j]] <- predict( proc, model.input.tot[model.input.tot$fold == j, ] )
    
  }
  
  # repeat for as many times as the svm needs to be evaluated for the parms
  model.input.list.train <- rep(model.input.list.train,nrow(parms))
  model.input.list.test <- rep(model.input.list.test,nrow(parms))
  
  # build list of input hyperparameters, each has to be repeated length(list)/nrow(parms)
  size.vec <- foreach(v = 1:nrow(parms),.combine = c) %do% rep(parms[v,'size'],(length(model.input.list.train)/nrow(parms)))
  decay.vec <- foreach(v = 1:nrow(parms),.combine = c) %do% rep(parms[v,'decay'],(length(model.input.list.train)/nrow(parms)))
  
  out.list <- mcmapply(nn.tenfold.tune_extra,train=model.input.list.train,test=model.input.list.test,
                       fml = model.string,size=size.vec,decay=decay.vec,proc=rep(list(proc),length(size.vec)),mc.cores=NC, SIMPLIFY = FALSE)
  out <- do.call("rbind", out.list)
  
  out.median.list <- lapply(split(out,rep(1:(nrow(out)/n.folds),each=n.folds)),function(x) apply(x,2,median) )
  out.median <- do.call("rbind", out.median.list)
  
  #list of tables with parms and corresponding medians of 10-fold CV
  tuned.models.tabs <- cbind(parms,as.data.frame(out.median))
  
  write.csv(tuned.models.tabs,paste0(folder.store.tuning,Qname,'.tuning.results.csv'),row.names = FALSE)
  
  #store table with the results of the best set of hyper-parameters
  write.csv(tuned.models.tabs[tuned.models.tabs$test.rsq == max(tuned.models.tabs[,'test.rsq']),]
            ,paste0(folder.tabs,Qname,'.tuning.results.best.par.csv'))
  
}
