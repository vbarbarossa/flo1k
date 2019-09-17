library(raster)

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

source(paste0(wd,'scripts/functions_generic.R'))

dir.drt <- paste0(wd,'upscaling/DRT_FDR/')
dir.fd <- dir_(paste0(wd,'upscaling/FD_taudem/'))
dir.tmp <- dir_(paste0(wd,'tmp_upscl'))
rasterOptions(tmpdir = dir.tmp)

res.str <- c('5min','30min')

# set conform target extent
target_extent <- c(-180,180,-90,90)
target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'
target_resolutions <- c(1/12,1/2)

for(i in res.str){
  n = which(res.str == i)
  
  r = raster(paste0(dir.drt,'DRT_',i,'.asc'))
  a = raster(paste0(wd,'upscaling/areas/area_',i,'.tif'))
  ### adjust routing to taudem
  r[r%in%c(8)] <- 6
  r[r%in%c(4)] <- 7
  r[r%in%c(2)] <- 8
  r[r%in%c(128)] <- 2
  r[r%in%c(64)] <- 3
  r[r%in%c(32)] <- 4
  r[r%in%c(16)] <- 5
  r[!r %in% 1:8] <- NA
  
  ### adjust extent
  crs(r) <- target_crs
  res(r) <- target_resolutions[n]
  r <- extend(r,extent(a))
  extent(r) <- extent(a)
  
  fd.name <- paste0(dir.fd,'DRT_FD_',i,'_taudem.tif')
  
  writeRaster(r,fd.name,format = 'GTiff',datatype='INT1U',
              overwrite=TRUE)
  
  ### accumulate area with taudem
  area.acc.name <- paste0(wd,'upscaling/areas/area_acc_',i,'.tif')
  area.name <- paste0(wd,'upscaling/areas/area_',i,'.tif')
  
  system(
    paste0(
      'mpiexec -n 5 /opt/Taudem5/bin/aread8 -p ',
      fd.name,' -ad8 ',area.acc.name,' -wg ',area.name,' -nc'
    )
  )
  
  
}

system(paste0('rm -r ',dir.tmp))
