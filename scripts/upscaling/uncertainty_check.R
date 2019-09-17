rm(list=ls())

library(raster);library(foreach)

NC = 40 #needs to be the same used in mapping

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

source(paste0(wd,'scripts/functions_generic.R'))

dir.tmp <- dir_(paste0(wd,'tmp_upscl/'))
rasterOptions(tmpdir=dir.tmp)

target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

res.str <- c('30min','5min')
target_resolutions <- c(1/2,1/12)

i = res.str[2]
n = which(res.str == i)

# extract cell values from FLO1K at 30 arc sec
dir.tabs <- paste0(wd,'upscaling/mapping_',i,'/')

tabs.src <- foreach(nc = 1:40) %do% read.csv(paste0(dir.tabs,nc,'.csv'))
tab <- do.call('rbind',tabs.src)
tab$area_diff <- abs(tab$area_30s-tab[,paste0('area_',i)])/apply(tab[,c(paste0('area_',i),'area_30s')],1,max)

dir.store <- dir_(paste0(dir_(paste0(wd,'upscaled/')),i,'/'))

ras <- raster(res=target_resolutions[n])
crs(ras) <- target_crs


vec <- ras[]
vec[tab[,'DRT_ID']] <- tab$area_diff
ras[] <- vec

writeRaster(ras,paste0(dir.store,'uncertainty_',i,'.tif'),format = 'GTiff',datatype='FLT4S', overwrite=TRUE)

length = 8.5
dpi = 600
labs = 0.5


wi = length
he = length*1.7/3
un = 'in'

library(tmap)
main <- tm_shape(ras) +
  tm_grid(projection="longlat", labels.size = labs, col = 'light grey',
          lwd = 0.01,alpha = 1,labels.inside.frame = FALSE) +
  
  tm_raster(palette="YlOrRd", contrast = c(0, 1),
            legend.show = TRUE, breaks = seq(0,1,0.1),title ='Relative difference (-)') + 
  
  tm_layout(legend.position = c('left','bottom'),
            legend.format = list(text.separator = 'to')
  )


library(grid)
save_tmap(main,filename=paste0(dir.store,'uncertainty_',i,'.jpg'),
          width = wi, height = he, units = un,
          outer.margins=c(0.1,0.03,0,0.03), dpi = dpi)

