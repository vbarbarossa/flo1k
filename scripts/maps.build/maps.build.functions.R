mk.dir <- function(str) {
  
  for(i in 1:length(str)){
    
    if (!dir.exists(str[i])) dir.create(file.path(str[i]))
    
  }
  
  return(str)
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
