rm(list=ls())

library(raster);library(foreach)

NC = 40 #needs to be the same used in mapping

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

source(paste0(wd,'scripts/functions_generic.R'))

dir.tmp <- dir_(paste0(wd,'tmp_upscl/'))
rasterOptions(tmpdir=dir.tmp)
dir.maps.wy <- paste0(wd,'FLO1K_wy/')

years_seq <- 1961:2015
Qnames = c('qav','qma','qmi') #
target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

res.str <- c('30min','5min')
target_resolutions <- c(1/2,1/12)

i = res.str[1]
n = which(res.str == i)

# extract cell values from FLO1K at 30 arc sec
dir.tabs <- paste0(wd,'upscaling/mapping_',i,'/')

tabs.src <- foreach(nc = 1:40) %do% read.csv(paste0(dir.tabs,nc,'.csv'))
tab <- do.call('rbind',
               parallel::mcmapply(
                 extract_by_station,
                 t = tabs.src,
                 coord.names = rep(list(c('lon','lat')),NC),
                 years_seq = rep(list(years_seq),NC),
                 Qnames = rep(list(Qnames),NC),
                 dir.flo1k=dir.maps.wy,prefix = FALSE,
                 SIMPLIFY = F,NC = NC
               )
)

write.csv(tab,paste0(wd,'upscaling/extractions_',i,'.csv'))
dir.store <- dir_(paste0(dir_(paste0(wd,'upscaled/')),i,'/'))

ras <- raster(res=target_resolutions[n])
crs(ras) <- target_crs
vec <- ras[]

for(q in Qnames){
  
  dir.q <- dir_(paste0(dir.store,q,'/'))
  
  for(y in years_seq){
    
    vec[tab[,'DRT_ID']] <- tab[,paste0(q,'.',y)]
    
    ras[] <- vec
    
    writeRaster(ras,paste0(dir.q,y,'.tif'),format = 'GTiff',datatype='FLT4S', overwrite=TRUE)
    
  }
  
}

