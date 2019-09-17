mk.dir <- function(str) {
  
  for(i in 1:length(str)){
    
    if (!dir.exists(str[i])) dir.create(file.path(str[i]))
    
  }
  
  return(str)
}


# in_var is the path to the src variable
# l is the lateral dimention of the tiles grid e.g. 2 for 4 tiles, 6 for 36 tiles
# NC is the number of cores to use
mc_create_tiles <- function(folder,in_var,l,NC){
  
  # #example:
  # l = 6
  # in_var <- covar.static.paths[1]
  # NC = 36
  # folder <- paste0(folder.calculations,'cells/',cont,'/')
  if (!dir.exists(folder)) dir.create(file.path(folder))
  
  # Get the dimensions of the jpg    
  dims <- as.numeric( strsplit( gsub('Size is|\\s+', '', grep('Size is', gdalinfo(in_var), value=TRUE)),',')[[1]] )
  
  # Set the window width and height
  w <- foreach(i=1:l,.combine=c) %do% length(cut(1:dims[1],breaks=l)[as.numeric(cut(1:dims[1],breaks=l)) == i])
  h <- foreach(i=1:l,.combine=c) %do% length(cut(1:dims[2],breaks=l)[as.numeric(cut(1:dims[2],breaks=l)) == i])
  
  #sequence of lower left corners corners
  llx <- c(0,cumsum(w[-length(w)]))
  lly <- c(0,cumsum(h[-length(h)]))
  
  # Create a data.frame containing coordinates of the lower-left
  # corners of the windows, and the corresponding output filenames.
  xy <- setNames(expand.grid(llx,lly), c('llx', 'lly'))
  xy$w <- rep(w,l) #width of each window
  xy$h <- rep(h,each=l) #height of each window
  xy$nm <- paste0(folder,1:l**2,'.tif') 
  
  # Create a function to split the raster using gdalUtils::gdal_translate
  split_rast <- function(infile, outfile, llx, lly, win_width, win_height) {
    library(gdalUtils)
    gdal_translate(infile, outfile, 
                   srcwin=c(llx, lly, win_width, win_height))
  }
  
  # split_rast(covar.static.paths[2], paste0(folder.calculations,xy$nm[1]), 
  #            xy$llx[1], xy$lly[1], xy$w[1], xy$h[1])
  
  parallel::mcmapply(split_rast, in_var, xy$nm, xy$llx, xy$lly, xy$w, xy$h,mc.cores=NC)
  
}

create_database <- function(data){
  df = data.frame(grdc_no= rep(100000,nrow(data)), #dummy, to apply predict(proc) later on the whole database
                  Q      = rep(1,nrow(data)),#dummy, to apply predict(proc) later on the whole database
                  S      = data$slope_pager/data$cells,#
                  H      = data$alt/data$cells,#
                  A      = data$area,
                  pav    = data$pav/data$cells + 1,
                  pma    = data$pma/data$cells + 1,
                  pmi    = data$pmi/data$cells + 1,
                  psi    = data$psi/data$cells, #need to check distribution
                  tav    = (data$tav/10)/data$cells,
                  tma    = (data$tma/10)/data$cells,
                  tmi    = (data$tmi/10)/data$cells,
                  eav    = data$eav/data$cells + 1,  #need to check distribution
                  esi    = data$esi/data$cells + 1,
                  ai     = (data$eav/data$cells)/(data$pav/data$cells) + 1)  #need to check distribution
  
  #df$koppen = data$koppen
  
  return(df)
}

# functions to backtransform scaled variables
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


predict_tiles <- function(t){
  
  #create function to read raster and output vector
  static <- as.data.frame( lapply(paste0(folder.calculations,covar.static.names,'/',cont,'/',t,'.tif'),
                                  function(x) c(as.matrix(raster(x))) ), col.names = covar.static.names )
  dynamic <- as.data.frame( lapply(paste0(folder.calculations,covar.dynamic.names,'/',cont,'/',t,'.tif'),
                                   function(x) c(as.matrix(raster(x))) ), col.names = covar.dynamic.names )
  
  covariates <- cbind(static,dynamic)
  
  #get koppen values use 'cells' layer as reference
  #maybe more efficient resample koppen layer and treat is as covariate?
  #r.ref = raster(paste0(folder.calculations,covar.static.names[1],'/',cont,'/',t,'.tif'))
  r.ref.gdal = readGDAL(paste0(folder.calculations,covar.static.names[2],'/',cont,'/',t,'.tif'),
                        silent = TRUE)
  
  df <- create_database(covariates)
  #df[(!is.na(df$A) & df$A < 4),] <- NA #threshold for Area
  
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
        
        q.temp[,rep] <- back2scale(proc.by.var[[nv]][[rep]], predict(fit.by.var[[nv]][[rep]],df.scaled,na.action=na.exclude) )
        
      }
      
      q.pred[[nv]] <- apply(q.temp,1,function(x) median(x,na.rm=T))
      q.cv[[nv]] <- apply(q.temp,1,function(x) raster::cv(x,na.rm=T))
      
    }
    
  }
  
  for(nv in 1:length(Qnames)){
    
    x <- r.ref.gdal
    m <- matrix(round(q.pred[[nv]],3),ncol=x@grid@cells.dim[1],nrow = x@grid@cells.dim[2])
    x@data$band1 <- c(t(m))
    writeGDAL(x,paste0(folder.store.maps.temp.cont[nv],t,'.tif'))
    
    x.cv <- r.ref.gdal
    m.cv <- matrix(round(q.cv[[nv]],3),ncol=x.cv@grid@cells.dim[1],nrow = x.cv@grid@cells.dim[2])
    x.cv@data$band1 <- c(t(m.cv))
    writeGDAL(x.cv,paste0(folder.store.maps.temp.cont[nv],t,'.cv.tif'))
    # raster <- matrix(round(q.pred[[nv]],3),
    #                  ncol=r.ref.gdal@grid@cells.dim[1],nrow=r.ref.gdal@grid@cells.dim[2])
    # height <- nrow(raster)
    # width <-ncol(raster)
    # tif <- new("GDALTransientDataset", new("GDALDriver", "GTiff"), height, width, 1, 'Byte')
    # 
    # pattern <- matrix(raster, width, height)
    # bnd1 <- putRasterData(tif, pattern)
    # 
    # tif_file <- paste0(folder.store.maps.temp.cont[nv],t,'prova.tif')
    # saveDataset(tif, tif_file)
    # GDAL.close(tif)
    # 
    # x <- readGDAL(tif_file)
    # x@grid <- r.ref.gdal@grid
    # x@bbox <- r.ref.gdal@bbox
    # proj4string(x) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    # 
    # 
    # 
    #     
    #     
    #     q.map <- raster(matrix(q.pred[[nv]],ncol=ncol(r.ref),nrow=nrow(r.ref)),
    #                     crs = ' +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
    #     extent(q.map) <- extent(r.ref)
    #     #DECIDE FORMAT
    #     q.map <- round(q.map,3)
    #     #q.map <- raster::shift(q.map, x=0, y=-res(q.map)[2])
    #     writeRaster(q.map,paste0(folder.store.maps.temp.cont[nv],t,'.tif'),
    #                 format = 'GTiff',datatype='FLT4S', overwrite=TRUE)
    #     
    # 
    # 
    # q.map.cv <- raster(matrix(q.cv[[nv]],ncol=ncol(r.ref),nrow=nrow(r.ref)),
    #                 crs = ' +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
    # extent(q.map.cv) <- extent(r.ref)
    # #DECIDE FORMAT
    # q.map.cv <- round(q.map.cv,3)
    # #q.map.cv <- raster::shift(q.map.cv, x=0, y=-res(q.map.cv)[2])
    # writeRaster(q.map.cv,paste0(folder.store.maps.temp.cont[nv],t,'.cv.tif'),
    #             format = 'GTiff',datatype='FLT4S', overwrite=TRUE)
    # 
  }
  
  
} #end function predict_tiles


# aggregate all tiles to the continent function
# nv: variables number
# t: tiles
aggregate_to_continent <- function(nv,t){
  # list all tiles
  tiles.name <- paste0(folder.store.maps.temp.cont[nv],t,'.tif')
  
  # create mosaic and store it
  mosaic_rasters(tiles.name,
                 paste0(folder.store.maps.bycontinent[nv],year,'.tif'),
                 paste0(folder.store.maps.temp.cont[nv],'_mosaic_temp.vrt'),
                 co="COMPRESS=DEFLATE",
                 a_nodata = -1)
  
}

aggregate_to_continent.cv <- function(nv,t){
  # list all tiles
  tiles.name <- paste0(folder.store.maps.temp.cont[nv],t,'.cv.tif')
  
  # create mosaic and store it
  mosaic_rasters(tiles.name,
                 paste0(folder.store.maps.bycontinent[nv],year,'.cv.tif'),
                 paste0(folder.store.maps.temp.cont[nv],'_mosaic_temp.vrt'),
                 co="COMPRESS=DEFLATE",
                 a_nodata = -1)
  
}

# rast.files <- paste0(paste0(folder.calculations,covar.dynamic.names,'/',cont,'/')[2],
#                      list.files(paste0(folder.calculations,covar.static.names,'/',cont,'/')[2]) )
# mosaic_rasters(rast.files,
#                paste0(folder.store.maps.bycontinent[nv],'pma.alt.tif'),
#                paste0(folder.store.maps.bycontinent[nv],'_mosaic_temp.vrt'),
#                a_nodata = -1)
# 

# rast.files <- paste0(folder.store.maps.temp.cont[1],list.files(folder.store.maps.temp.cont[1],pattern='.tif') )
# rast.list <- lapply(as.list(rast.files),function(x) raster(x) )
# rast.list$fun <- mean
# rast.list$na.rm <- TRUE
# 
# mos <- do.call(mosaic,rast.list)
# writeRaster(mos,paste0(folder.store.maps.bycontinent[1],year,'2000.prova.mosaic.tif'))
