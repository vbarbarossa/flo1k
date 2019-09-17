### TO MODIFY ####################################################################################################################
library(raster); library(foreign); library(rgdal); library(gdalUtils)

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
wd.old <- '/vol/milkun3/Valerio/FLO1K/'

source(paste0(wd,'scripts/functions_generic.R'))

folder.store = dir_(paste0(wd,"/COVAR.DYN.ACC/"))
folder.temp = dir_(paste0(wd,"/COVAR.DYN.ACC/CRU.temp.pet/"))
folder.cru = paste0(wd.old,'/COVAR.DYN.ACC/CRU/')
folder.solrad = paste0(wd.old,'/COVAR.DYN.ACC/ET_SolRad/')
folder.flow.dir = paste0("/vol/milkun8/Valerio/HYDROGRAPHY/DIR_taudem/")

# modify with name of CRU TS file
file.tmp = 'cru_ts3.24.01.1901.2015.tmp.dat.nc'
file.dtr = 'cru_ts3.24.01.1901.2015.dtr.dat.nc'

prefix = 'e'
# time interval 
start_year = 1960
end_year = 2015
database_start_year = 1901

#specify variable name as in the CRU database
var_tmp = 'tmp'
var_dtr = 'dtr'
##################################################################################################################################
##################################################################################################################################

#aggregate ET_SolRad to 0.5 (native resolution is 0.025)
folder.solrad.aggr = paste0(folder.solrad,'HalfDeg/')
if (!dir.exists(folder.solrad.aggr)){
  
  dir.create(file.path(folder.solrad.aggr))
  
  for(i in 1:12){
    
    r = readGDAL(paste0(folder.solrad,'et_solrad_',i))
    
    name_store = paste0(folder.solrad.aggr,i,'_temp.tif')
    name_res = paste0(folder.solrad.aggr,i,'.tif')
    
    writeGDAL(r,name_store)
    gdal_translate(name_store,name_res,ot="Float32",of='GTiff',tr = c(0.5,0.5),r='average')
    
    file.remove(name_store)	
    
  }
  
  
}

#months to exclude: e.g. 12*(1981-1901)+1=961
start_month = 12*(start_year - database_start_year)+1-3 #-3 is to consider the water year starting October of the previous month

#create sequence of "Januaries" for each year
yearseq = start_year:end_year
nseq = seq(start_month,(start_month+length(yearseq)*12-12),12)

days2month = c(31,30,31,31,28,31,30,31,30,31,31,30) #days in each water year month
solrad_seq = c(10:12,1:9) #according to the water year

#read the ncdf file
for(i in 1:length(yearseq)){
  
  pet_list = list()
  
  for(j in 1:12){
    
    #CRU tmp [C]
    Tmean = raster(paste0(folder.cru,file.tmp), varname=var_tmp, band = (nseq[i]+(j-1)))
    #CRU dtr [C]
    TD = raster(paste0(folder.cru,file.dtr), varname=var_dtr, band = (nseq[i]+(j-1)))
    #Solar Radiation [mm/day] --> need mm/month
    RA = raster(paste0(folder.solrad.aggr,solrad_seq[j],'.tif'),
                crs=crs(Tmean))*days2month[j]
    
    #Hargreaves PET formulation	
    pet_list[[j]] = 0.0023*RA*(Tmean + 17.8)*TD**0.5
    
  }
  
  sum = Reduce('+',pet_list)
  sum[sum < 0] <- 0 #important: there are some negative values at high latitudes
  
  #average over the year
  av = sum/12
  
  #seasonality index (si) over the year
  si = Reduce('+',lapply(pet_list,function(x) abs(x-av)))/sum
  
  #now we have av,si for the current year
  var_metrics = c('av','si')
  
  #need to iterate the operation for the 6 continents
  continents = c('af','as','au','eu','na','saca')
  
  for(c in 1:length(var_metrics)){
    
    var = get(var_metrics[c])
    
    #store the variable as integer, different for p or t
    var_store = var
    
    name_store = paste0(folder.temp,var_metrics[c],'.tif')
    name_res = paste0(folder.temp,var_metrics[c],'_res.tif')
    
    writeRaster(var_store,name_store,format = 'GTiff',datatype='FLT4S', overwrite=TRUE)
    gdal_translate(name_store,name_res,ot="Float32",of='GTiff',tr = c(0.0083333333,0.0083333333),r='nearest')
    
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