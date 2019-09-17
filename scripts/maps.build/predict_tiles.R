#sbatch --array=1-2500

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(n in nodenr:nodenr){
  
  years_seq <- 1961:2015
  Qnames = c('qav','qma','qmi') #
  
  wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
  source(paste0(wd,'scripts/functions_generic.R'))
  
  
  folder.tiles <- dir_(paste0(wd,'predictors.tiled/'))
  folder.tiles.q <- do.call('c',lapply(Qnames,function(x) dir_(paste0(folder.tiles,x))))
  
  # check years already existing
  check_ <- sapply(years_seq,
                   function(x) sum(file.exists(paste0(folder.tiles.q,'/',x,'/',n,'.tif'))) != 3)
  # and filter them out
  years_seq <- years_seq[check_]
  
  
  if(length(years_seq) != 0){
    
    
    library(gdalUtils); library(foreach); library(raster); library(rgdal); library(nnet); library(caret);
    
    
    source(paste0(wd,'scripts/maps.build/maps.build.functions.R'))
    
    
    
    # names of the variables
    covar.static.names <- c('cells','alt','area','slope_pager') #
    covar.dynamic.names <- c('pav','pma','pmi','psi','tav','tma','tmi','eav','esi') #
    
    # create temporary folder
    folder.tiles <- dir_(paste0(wd,'predictors.tiled/'))
    folder.tiles.q <- do.call('c',lapply(Qnames,function(x) dir_(paste0(folder.tiles,x))))
    folder.tmp <- dir_(paste0(wd,'tmp/tmp.predict.tiles.',n,'/'))
    rasterOptions(tmpdir = folder.tmp)
    # # conform names of static vars to c('cells','alt','area','slope_pager')
    # file.rename(paste0(folder.tiles,c('acc_cells','alt_acc','area_acc','slope_pager_acc')),
    #             paste0(folder.tiles,covar.static.names))
    
    
    
    rep.cal.max = 20
    
    # need to rename them in a loop as they are stored all with the same name: final.models.fit
    fit.by.var <- list()
    proc.by.var <- list()
    for(nv in 1:length(Qnames)){
      load(paste0(wd,'/',Qnames[[nv]],'.NN.fit.',rep.cal.max,'rep.RData')) #final.models.fit
      load(paste0(wd,'/',Qnames[[nv]],'.NN.10fold.proc.',rep.cal.max,'rep.RData')) #proc.list.final.models
      
      fit.by.var[[nv]] <- final.models.fit
      proc.by.var[[nv]] <- proc.list.final.models
    }  
    
    
    for(year in years_seq){
      
      t <- n #set tile number
      
      folder.tiles.q.year <- do.call('c',lapply(1:length(Qnames),function(x) dir_(paste0(folder.tiles.q[x],'/',year))))
      
      
      # create function to read raster and output vector
      static <- as.data.frame( lapply(paste0(folder.tiles,covar.static.names,'/',t,'.tif'),
                                      function(x) c(as.matrix(raster(x))) ), col.names = covar.static.names )
      dynamic <- as.data.frame( lapply(paste0(folder.tiles,covar.dynamic.names,'/',year,'/',t,'.tif'),
                                       function(x) c(as.matrix(raster(x))) ), col.names = covar.dynamic.names )
      
      covariates <- cbind(static,dynamic)
      
      r.ref.gdal = readGDAL(paste0(folder.tiles,covar.static.names[2],'/',t,'.tif'),
                            silent = TRUE)
      
      df <- create_database(covariates)
      
      #initialize q.pred vector
      q.pred <- list()
      q.cv <- list()
      for(i in 1:length(Qnames)){
        q.pred[[i]] <- rep(NA,nrow(df))
        q.cv[[i]] <- rep(NA,nrow(df))
        
      } 
      
      # evaluate NN only if there are cells without NAs
      if( !( sum(is.na(df$A)) == nrow(df) ) ) {
        
        for( nv in 1:length(Qnames) ){
          
          q.temp <- data.frame(matrix(nrow=nrow(df),ncol=rep.cal.max))
          
          for(rep in 1:rep.cal.max){
            
            #need to first scale predictor variables
            df.scaled <- predict(proc.by.var[[nv]][[rep]],df)
            df.scaled <- df.scaled[,3:ncol(df.scaled)]
            for(col in 1:ncol(df.scaled)) df.scaled[(is.infinite(df.scaled[,col]) | is.nan(df.scaled[,col])),col] <- NA
            
            q.temp[,rep] <- back2scale(proc.by.var[[nv]][[rep]], predict(fit.by.var[[nv]][[rep]],df.scaled,na.action=na.exclude) )
            
          }
          
          q.pred[[nv]] <- apply(q.temp,1,function(x) median(x,na.rm=T))
          q.cv[[nv]] <- apply(q.temp,1,function(x) raster::cv(x,na.rm=T))
          
        }
        
      }
      
      folder.tiles.q.year <- do.call('c',lapply(1:length(Qnames),function(x) dir_(paste0(folder.tiles.q[x],'/',year))))
      
      for(nv in 1:length(Qnames)){
        
        x <- r.ref.gdal
        m <- matrix(round(q.pred[[nv]],3),ncol=x@grid@cells.dim[1],nrow = x@grid@cells.dim[2])
        x@data$band1 <- c(t(m))
        writeGDAL(x,paste0(folder.tiles.q.year[nv],'/',t,'.tif'),options="COMPRESS=DEFLATE")
        
        x.cv <- r.ref.gdal
        m.cv <- matrix(round(q.cv[[nv]],3),ncol=x.cv@grid@cells.dim[1],nrow = x.cv@grid@cells.dim[2])
        x.cv@data$band1 <- c(t(m.cv))
        writeGDAL(x.cv,paste0(folder.tiles.q.year[nv],'/',t,'.cv.tif'),options="COMPRESS=DEFLATE")
      }
      
      
    }
    
    
    
    
    system(
      paste0(
        'rm -r ',folder.tmp
      )
    )
    
  }
  
  
}