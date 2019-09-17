library(rgdal); library(raster)

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'

#INPUT shapefile of relocated stations
points = readOGR(paste0(wd,'/GRDC.DATA/MONITORING.STATIONS/RE-ALLOCATED/grdc_relocated_taudem.shp'))

CLIMpath = paste0(wd,'/COVAR.DYN.ACC/')
STOREpath = paste0(wd,'/GRDC.DATA/DYN/')

climate = c('pav','pma','pmi','psi') #c('pav','pma','pmi','psi','eav','esi','tav','tma','tmi')
continents = c('af','as','au','eu','na','saca')
year_seq = 1961:2015


for(i in 1:length(climate)){
  
  temp_tab = data.frame(matrix(nrow=length(points),ncol=(length(year_seq)+1)))
  colnames(temp_tab) = c('grdc_no',year_seq)
  temp_tab[,1] = points$grdc_no
  
  CLIMVARpath = paste0(CLIMpath,climate[i],'/')
  
  for(y in 1:length(year_seq)){
    
    year = year_seq[y]
    
    master_values = rep(0,length(points))
    
    for(n in 1:length(continents)){
      
      CLIMVARCONTpath = paste0(CLIMVARpath,continents[n],'/')
      
      curRaster = raster(paste0(CLIMVARCONTpath,year,'.tif'))
      values = extract(curRaster,points)
      values[is.na(values)] <- 0
      
      master_values = master_values + values
      
    }
    
    
    temp_tab[,as.character(year)] <- master_values
    
  }
  
  
  
  write.csv(temp_tab,paste0(STOREpath,climate[i],'.csv'),row.names=FALSE)
  
}
