library(raster);library(gdalUtils);library(foreach);library(rgdal)

NC = 30

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

source(paste0(wd,'scripts/functions_generic.R'))

dir.tmp <- dir_(paste0(wd,'tmp_upscl/'))
rasterOptions(tmpdir=dir.tmp)

area.30s.name <- paste0(wd,'upscaling/areas/area_acc_30s.tif')

target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

if(!file.exists(area.30s.name)){
  
  area.30s.src <- '/vol/milkun3/Valerio/FLO1K/COVAR.STATIC.ACC/area_acc.tif'
  
  ### conform area.30s
  target_extent <- c(-180,180,-90,90)
  target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'
  
  area.30s <- raster(area.30s.src)
  
  crs(area.30s) <- target_crs
  res(area.30s) <- 1/120
  area.30s <- extend(area.30s,target_extent)
  extent(area.30s) <- extent(target_extent)
  
  writeRaster(area.30s,area.30s.name,format = 'GTiff',overwrite=TRUE)
  
}

res.str <- c('30min','5min')
target_resolutions <- c(1/2,1/12)

for(i in res.str){
  n = which(res.str == i)
  
  area.drt.name <- paste0(wd,'upscaling/areas/area_acc_',i,'.tif')
  area.drt <- raster(area.drt.name)
  area.f1k <- raster(area.30s.name)
  
  res = target_resolutions[n]
  
  ulx <- seq(-180,180-res,res)
  uly <- sort(seq(-90+res,90,res),decreasing=T) #necessary to maintain the order in which raster stores the values
  
  grid <- expand.grid(ulx,uly)
  
  get_coord <- function(t){
    
    e <- extent(c(grid[t,1],grid[t,1]+res,grid[t,2]-1/2,grid[t,2]))
    
    drt <- crop(area.drt,e)
    
    tab <- data.frame(
      x = grid[t,1] + res/2,
      y = grid[t,2] - res/2,
      area_acc_30s = NA,
      area_up = NA
    )
    
    if(!is.na(drt[])){
      
      a.up <- drt[]
      
      f1k <- crop(area.f1k,e)
      
      p <- rasterToPoints(f1k)
      
      tab <- cbind(
        t(p[which(abs(a.up-p[,3]) == min(abs(a.up-p[,3]),na.rm=T)),]),data.frame(area_up = a.up)
      )
      
    }
    
    tab <-  cbind(t,tab)
    colnames(tab) <- c('DRT_ID','lon','lat','area_30s',paste0('area_',i)) 
    return(tab)
  }
  
  tab <- do.call('rbind',
                 parallel::mclapply(as.list(1:nrow(grid)),get_coord,mc.cores=NC)
  )
  
  tab <- do.call('rbind',
                 parallel::mclapply(as.list(1:1000),get_coord,mc.cores=10)
  )
  
  
  write.csv(tab,paste0(wd,'upscaling/','mapped_tab_',i,'.csv'),row.names = F)
  
  
}


system(paste0('rm -r ',dir.tmp))
