#sbatch --array=1-3

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(n in nodenr:nodenr){
  
  library(gdalUtils)
  
  years_seq <- 1961:2015
  Qnames = c('qav','qma','qmi') #
  
  wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
  source(paste0(wd,'scripts/functions_generic.R'))
  
  folder.tiles <- paste0(wd,'predictors.tiled/',Qnames[n],'/')
  folder.store <- dir_(paste0(wd,'FLO1K_wy/'))
  
  for(year in years_seq){
    
    folder.store.q <- dir_(paste0(folder.store,Qnames[n],'/'))
    folder.tmp <- dir_(paste0(wd,'tmp_aggregate_',Qnames[n],year,'/'))
    
    tiles <- paste0(folder.tiles,year,'/',1:2500,'.tif')
    
    splitter <- list(1:1500,1501:2500)
    # split in 2 and aggregate
    for(i in 1:2){
      tiles_tmp <- tiles[splitter[[i]]]
      
      system(
        paste0(
          'gdal_merge.py -o ',paste0(folder.tmp,'tmp_mosaic_',i,'.tif'),
          ' -a_nodata -1 ',paste(tiles_tmp,collapse=' ')
        )
      )
      
    }
    
    tiles_tmp_par <- paste(paste0(folder.tmp,'tmp_mosaic_',1:2,'.tif'),collapse = ' ')
    
    system(
      paste0(
        'gdal_merge.py -o ',paste0(folder.tmp,'tmp_mosaic.tif'),
        ' -co "COMPRESS=DEFLATE" -a_nodata -1 -v ',tiles_tmp_par
      )
    )
    
    system(paste0('rm -r ',tiles_tmp_par))
    
    
    system(
      paste0(
        'gdal_calc.py -A ',paste0(folder.tmp,'tmp_mosaic.tif'),
        ' --outfile=',paste0(folder.store.q,year,'.tif'),
        ' --calc="A*(A>0)" --co="COMPRESS=DEFLATE" --NoDataValue=-1 --overwrite'
      )
    )
    
    system(paste0('rm -r ',folder.tmp))
    
  }
  
  
  
}