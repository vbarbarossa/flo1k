library(raster);library(gdalUtils);library(foreach);library(rgdal)

NC = 40

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

i = res.str[2]
n = which(res.str == i)

area.drt.name <- paste0(wd,'upscaling/areas/area_acc_',i,'.tif')
area.drt <- raster(area.drt.name)
area.f1k <- raster(area.30s.name)

res = target_resolutions[n]

ulx <- seq(-180,180-res,res)
uly <- sort(seq(-90+res,90,res),decreasing=T) #necessary to maintain the order in which raster stores the values

grid <- expand.grid(ulx,uly)

get_coord <- function(t){
  
  e <- extent(c(grid[t,1],grid[t,1]+res,grid[t,2]-res,grid[t,2]))
  
  drt <- crop(area.drt,e)
  
  tab <- data.frame(
    x = grid[t,1] + res/2,
    y = grid[t,2] - res/2,
    area_acc_30s = NA,
    area_up = drt[]
  )
  
  if(!is.na(drt[])){
    
    e <- extent(c(grid[t,1]-res/2,grid[t,1]+1.5*res,grid[t,2]-1.5*res,grid[t,2]+res/2))
    a.up <- drt[]
    
    f1k <- crop(area.f1k,e)
    
    p <- rasterToPoints(f1k)
    
    if(nrow(p) != 0){
      
      min.val <- which(abs(a.up-p[,3]) == min(abs(a.up-p[,3]),na.rm=T))
      if(length(min.val) > 1) min.val <- sample(min.val,1)
      
      tab <- cbind(
        t(p[min.val,]),data.frame(area_up = a.up)
      )
      
      
    }
    
    
  }
  
  tab <-  cbind(t,tab)
  colnames(tab) <- c('DRT_ID','lon','lat','area_30s',paste0('area_',i)) 
  return(tab)
}

tiles <- which(!is.na(area.drt[]))
nc.index <- cut(1:length(tiles),breaks=NC,labels=F)

mc_mapping <- function(nc,dir.store){
  
  tn <- tiles[nc.index == nc]
  
  tab <- do.call('rbind',
                 lapply(as.list(tn),get_coord)
  )
  
  write.csv(tab,paste0(dir.store,nc,'.csv'),row.names = F)
  
}

# #for diagnostic
# nc = 32
# tn <- tiles[nc.index == nc]
# for(h in tn) get_coord(h)

parallel::mcmapply(mc_mapping,
                   as.list(unique(nc.index)),
                   dir_(paste0(wd,'upscaling/mapping_',i,'/')),
                   mc.cores = NC
)

system(paste0('rm -r ',dir.tmp))

source(paste0(wd,'scripts/upscaling/build_raster_5min.R'))