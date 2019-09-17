
wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
wd.old <- '/vol/milkun3/Valerio/FLO1K/'

library(raster); library(rgdal)
source(paste0(wd,'scripts/functions_generic.R'))

folder.tmp <- dir_(paste0(wd,'tmp/'))

r.ref <- paste0(folder.tmp,"raster_ref.tif")

system(
  paste0(
    'gdal_calc.py -A ',paste0(wd,'COVAR.DYN.ACC/pav/1961.tif'),' --outfile=',r.ref,' --calc "A*0" --NoDataValue=0 --type=Byte --overwrite'
  )
)

basins <- readOGR(paste0(wd,'worldbasins1k/worldbasins1k.shp'))

assign_hemisphere <- function(id){
  
  print(id)
  
  folder.bas <- dir_(paste0(folder.tmp,'basin_',id,'/'))
  
  bas.shp <- paste0(folder.bas,'single_basin.shp')
  
  writeOGR(basins[basins@data$ID == id,],
           bas.shp,
           paste0(id,'_basin.shp'),driver="ESRI Shapefile")
  
  bas.tif <- paste0(folder.bas,'single_basin.tif')
  
  system(
    paste0(
      'gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES -ot Byte  -crop_to_cutline -cutline ',
      bas.shp,' ',r.ref,' ',bas.tif,' -overwrite'
    )
  )
  
  lat <- init(raster(bas.tif),'y')
  lat_vec <- getValues(lat)
  
  # Hemisphere: 1=north, 2=central, 3=south
  H <- 2
  if(median(lat_vec,na.rm=T) >= 45) H <- 1
  if(median(lat_vec,na.rm=T) <= -45) H <- 3
  
  system(paste0('rm -r ',folder.bas))
  
  return(H)
}
H_res <- mapply(assign_hemisphere,as.list(as.numeric(basins@data$ID)))
write.csv(data.frame(H = H_res),paste0(folder.tmp,'/diag.csv'))

basins@data$H <- H_res

write.csv(basin@data,paste0(wd,'worldbasins1k/reassigned.csv'))
writeOGR(basins,paste0(wd,'worldbasins1k/worldbasins1k_hemisfere.shp'),'worldbasins1k.shp',driver="ESRI Shapefile")
