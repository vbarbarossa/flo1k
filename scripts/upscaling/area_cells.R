#export area map
library(raster)
#15s = 0.004166667 degrees
wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

source(paste0(wd,'scripts/functions_generic.R'))

dir.drt <- paste0(wd,'upscaling/DRT_FDR/')
dir.store <- dir_(paste0(wd,'upscaling/areas/'))
dir.tmp <- dir_(paste0(wd,'tmp_upscl'))
rasterOptions(tmpdir=dir.tmp)

res.str <- c('5min','30min')
target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'
target_resolutions <- c(1/12,1/2)

for(i in res.str){
  n = which(res.str == i)
  
  r <- raster(res=target_resolutions[n])
  a <- area(r)
  
  crs(a) <- target_crs
  
  writeRaster(a,paste0(dir.store,'area_',i,'.tif'),format='GTiff',overwrite = TRUE)
  
  
}

system(paste0('rm -r ',dir.tmp))
