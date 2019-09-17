res.name <- '5min'

years_seq <- 1961:2015
Qnames = c('qav','qma','qmi')

dir.scripts <- '/vol/milkun3/Valerio/FLO1K_1.1/scripts/netcdf.creation/'
dir.flo1k <- paste0('/vol/milkun3/Valerio/FLO1K_1.1/upscaled/',res.name,'/')

target_extent <- c(-180,-90,180,90)
target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

comments <- paste0('Output upscaled to ',substr(res.name,1,2),' arc min')

q.long.names = c('mean annual streamflow',
                 'maximum annual streamflow',
                 'minimum annual streamflow')

q.descriptions <- c('mean over the 12 mean monthly streamflow values of the year',
                  'maximum mean monthly streamflow of the year',
                  'minimum mean monthly streamflow of the year')
                 
for(q in Qnames){
  
  nq <- which(Qnames == q)
  q.long.name <- q.long.names[nq]
  q.description <- q.descriptions[nq]
  
  dir.src <- paste0(dir.flo1k,q,'/')
  dir.tmp <- paste0(dir.flo1k,q,'_tmp/')
  if(!dir.exists(dir.tmp)) dir.create(dir.tmp)
  
  for(year in years_seq){
    
    src <- paste0(dir.src,year,'.tif')
    dst.zero <- paste0(dir.tmp,year,'nozeroes_tmp.tif')
    
    system(
      paste0(
        'gdal_calc.py -A ',src,' --outfile=',dst.zero,
        ' --calc="A*(A>0)" --NoDataValue=-1'
      )
    )
    
    dst <- paste0(dir.tmp,year,'.nc')
    
    system(
      paste0(
        'gdal_translate -of netcdf -co FORMAT=NC4 -co COMPRESS=DEFLATE -a_nodata -1 ',
        dst.zero,' ',dst,' && ncrename -v Band1,',q,' ',dst
      )
    )
    
    system(paste0( 'rm -r ',dst.zero) )
    
  }
  
  flo1k.db.name.tmp = paste0(dir.flo1k,'FLO1K.1.1.ts.',years_seq[1],'.',years_seq[length(years_seq)],'.',q,'_TMP.nc')
  flo1k.db.name = paste0(dir.flo1k,'FLO1K.',res.name,'.ts.',years_seq[1],'.',years_seq[length(years_seq)],'.',q,'.nc')
  # collate years in 1 NC4
  system(
    paste0(
      'ncecat -O -u time ',paste(paste0(dir.tmp,years_seq,'.nc'),collapse = ' '),' ',
      flo1k.db.name.tmp
    )
  )
  
  #remove crs variable
  system(paste0('ncks -C -O -x -v crs ',flo1k.db.name.tmp,' ',flo1k.db.name))
  system(paste0('rm -r ',flo1k.db.name.tmp))
  
  #modify attributes
  system(
    paste0(
      'ncatted -O -a description,',q,',o,c,"',q.description,'" -a long_name,',q,',o,c,"',q.long.name,'" -a units,',q,
      ',a,c,"m3/s" -a GDAL_AREA_OR_POINT,global,d,, -a GDAL,global,d,, -a history,global,d,, -a Conventions,global,d,, -a nco_openmp_thread_number,global,d,, -a NCO,global,d,, ',
      flo1k.db.name,' -h'
    )
  )
  
  
  # add time variable
  py.lines = readLines(paste0(dir.scripts,'time_var.py'))
  py.lines[11] <- paste0("nco = netCDF4.Dataset('",flo1k.db.name,"','a')")
  py.lines[25] <- paste0("years_seq = range(",years_seq[1],",",(years_seq[length(years_seq)]+1),")")
  py.lines[41] <- paste0(    "nco.comment='",comments,"'"  )
  py.lines[44] <- paste0(    "nco.history='Created on ",paste0(Sys.Date()),"'"  )
  writeLines(py.lines,paste0(dir.scripts,'time_var.py'))
  
  system(paste0('python ',dir.scripts,'time_var.py'))
  
  system(paste0('rm -r ',dir.tmp))
  
}
