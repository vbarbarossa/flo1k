### TO MODIFY ####################################################################################################################
library(raster); library(foreign); library(gdalUtils)

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
wd.old <- '/vol/milkun3/Valerio/FLO1K/'

source(paste0(wd,'scripts/functions_generic.R'))

folder.store = dir_(paste0(wd,"/COVAR.DYN.ACC/"))
folder.temp = dir_(paste0(wd,"/COVAR.DYN.ACC/CRU.temporary/"))
folder.file = paste0(wd.old,'/COVAR.DYN.ACC/CRU/')
folder.flow.dir = paste0("/vol/milkun8/Valerio/HYDROGRAPHY/DIR_taudem/")

# modify with name of CRU TS file
file.name = 'cru_ts3.24.01.1901.2015.tmp.dat.nc'

prefix = 't'
# time interval 
start_year = 1960
end_year = 2015
database_start_year = 1901

#specify variable name as in the CRU database
var_name = 'tmp'

##################################################################################################################################
##################################################################################################################################

#months to exclude: e.g. 12*(1981-1901)+1=961
start_month = 12*(start_year - database_start_year)+1-3 #-3 is to consider the water year starting October of the previous month

#create sequence of "Januaries" for each year
yearseq = start_year:end_year
nseq = seq(start_month,(start_month+length(yearseq)*12-12),12)

#read the ncdf file
for(i in 1:length(yearseq)){
  
  var_start = raster(paste0(folder.file,file.name), varname=var_name, band = nseq[i])
  
  av = var_start
  ma = var_start
  mi = var_start
  
  for(j in 1:11){
    
    var_new = raster(paste0(folder.file,file.name), varname=var_name, band = (nseq[i]+j))
    
    av = av + var_new
    ma = max(ma,var_new)
    mi = min(mi,var_new)
    
  }
  
  av = av/12
  
  #now we have av,ma,mi for the current year
  var_metrics = c('av','ma','mi')
  
  #need to iterate the operation for the 6 continents
  continents = c('af','as','au','eu','na','saca')
  
  
  
  for(c in 1:length(var_metrics)){
    
    var = get(var_metrics[c])
    
    #store the variable as integer, different for p or t
    var_store = (var+273.2)*10+0.5
    storage.mode(var_store[]) = "integer"
    
    
    name_store = paste0(folder.temp,var_metrics[c],'.tif')
    name_res = paste0(folder.temp,var_metrics[c],'_res.tif')
    
    writeRaster(var_store,name_store,format = 'GTiff',datatype='INT2U', overwrite=TRUE)
    gdal_translate(name_store,name_res,ot="UInt16",of='GTiff',tr = c(0.0083333333,0.0083333333),r='nearest')
    
    storepath_var <- paste0(folder.store,prefix,var_metrics[c],"/")
    
    #create folder if it doesnt exist
    if (!dir.exists(storepath_var)){
      dir.create(file.path(storepath_var))
    }
    
    for(n in 1:length(continents)){
      
      storepath_cont <- paste0(storepath_var,continents[n],"/")
      
      #create folder if it doesnt exist
      if (!dir.exists(storepath_cont)){
        dir.create(file.path(storepath_cont))
      }
      
      #load the name of flow dir file and raster
      name_fd = paste0(folder.flow.dir,continents[n],'_dir.tif')
      
      name_index = paste0(folder.temp,continents[n],'_index.shp')
      name_res_cont = paste0(folder.temp,continents[n],'_',var_metrics[c],'.tif')
      
      if(!file.exists(name_index)) gdaltindex(name_index,name_fd)
      
      #crop the global raster with GDAL to the extent of the fd raster
      gdalwarp(name_res,name_res_cont,cutline=name_index,t_srs = crs(raster(name_fd)),
               crop_to_cutline = TRUE,overwrite = TRUE)
      
      name_acc = paste0(storepath_cont,yearseq[i],'.tif')
      
      system(
        paste0(
          'mpiexec -n 20 /opt/Taudem5/bin/aread8 -p ',
          name_fd,' -ad8 ',name_acc,' -wg ',name_res_cont,' -nc'
        )
      )
      
    }
    
    
    #DELETE files
    file.remove(
      c(name_store,
        name_res,
        paste0(folder.temp,continents,'_',var_metrics[c],'.tif'))
    )
    
  }
  
  
}