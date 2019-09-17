#sbatch --array=1-9

slurm_arrayid<-Sys.getenv("SLURM_ARRAY_TASK_ID")
nodenr<-as.numeric(slurm_arrayid)

for(n in nodenr:nodenr){
  
  # names of the variables
  covar.dynamic.names <- c('pav','pma','pmi','psi','tav','tma','tmi','eav','esi') #
  layer.name = covar.dynamic.names[n]
  
  wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
  wd.old <- '/vol/milkun3/Valerio/FLO1K/'
  
  library(gdalUtils); library(foreach); library(raster)
  
  source(paste0(wd,'scripts/functions_generic.R'))
  
  NC = 10
  
  # create temporary folder
  folder.tmp <- dir_(paste0(wd,'tmp.tiling_',layer.name,'/'))
  folder.tiles <- dir_(paste0(wd,'predictors.tiled/'))
  
  continents = c('as','na','af','au','eu','saca')
  years_seq <- 1961:2015
  # years_seq <- years_seq[!years_seq %in% list.dirs(path = paste0(folder.tiles,layer.name),
  #                                                  recursive = FALSE, full.names = FALSE)]
  
  # set conform target extent in GDAL format -te xmin ymin xmax ymax
  target_extent <- c(-180,-90,180,90)
  target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'
  
  # set tiles side l
  l = 50 # 2500 tiles in total of 864*432 pixels each
  
  for(year in years_seq){
    
    folder <- dir_(paste0(dir_(paste0(folder.tiles,layer.name,'/')),year,'/'))
    check.files <- list.files(folder)
    if(length(check.files) < l**2) {
      
      system(paste0('rm -r ',folder))
      folder <- dir_(paste0(dir_(paste0(folder.tiles,layer.name,'/')),year,'/'))
      
      lyr.src <- paste0(wd,'COVAR.DYN.ACC/',layer.name,'/',year,'.tif')
      
      # need to mosaic the variables stored as continents chuncks
      if(!file.exists(lyr.src)){
        
        folder.tmp.aggr <- dir_(paste0(folder.tmp,'aggr/'))
        
        rasterOptions(tmpdir = folder.tmp.aggr)
        
        src.files <- paste0(wd,'COVAR.DYN.ACC/',layer.name,'/',continents,'/',year,'.tif')
        dst.file <- paste0(wd,'COVAR.DYN.ACC/',layer.name,'/',year,'.tif')
        
        src.list <- lapply(as.list(src.files),function(x) raster(x) )
        
        src.list$fun <- mean
        src.list$na.rm <- TRUE
        mos <- do.call(mosaic,src.list)
        writeRaster(mos,dst.file,format = 'GTiff',datatype='FLT4S', overwrite=TRUE)
        
        system(paste0('rm -r ',folder.tmp.aggr))
        
      }
      
      #adjust precipitation layers: set negative values to 0
      if(layer.name %in% c('pav','pma','pmi','psi')){
        lyr.tmp2 <- paste0(folder.tmp,layer.name,year,'.tmp2.tif')
        
        system(
          paste0(
            'gdal_calc.py -A ',lyr.src,' --outfile=',lyr.tmp2,
            ' --calc="A*(A>=0)" --NoDataValue=-1'
          )
        )
        lyr.src <- lyr.tmp2
      }
      
      #conform file with extent and crs
      lyr.tmp <- paste0(folder.tmp,layer.name,year,'.tmp.tif')
      
      gdalwarp(lyr.src,lyr.tmp,t_srs = target_crs, te = target_extent,
               co="COMPRESS=DEFLATE")
      
      if(layer.name %in% c('pav','pma','pmi','psi')) system(paste0('rm -r ',lyr.tmp2))
      # divide in tiles
      # code from function mc_create_tiles in final.maps.functions
      in_var <- lyr.tmp
      
      
      # Get the dimensions of the jpg    
      dims <- as.numeric( strsplit( gsub('Size is|\\s+', '', grep('Size is', gdalinfo(in_var), value=TRUE)),',')[[1]] )
      
      # Set the window width and height
      w <- foreach(i=1:l,.combine=c) %do% length(cut(1:dims[1],breaks=l)[as.numeric(cut(1:dims[1],breaks=l)) == i])
      h <- foreach(i=1:l,.combine=c) %do% length(cut(1:dims[2],breaks=l)[as.numeric(cut(1:dims[2],breaks=l)) == i])
      
      #sequence of lower left corners corners
      llx <- c(0,cumsum(w[-length(w)]))
      lly <- c(0,cumsum(h[-length(h)]))
      
      # Create a data.frame containing coordinates of the lower-left
      # corners of the windows, and the corresponding output filenames.
      xy <- setNames(expand.grid(llx,lly), c('llx', 'lly'))
      xy$w <- rep(w,l) #width of each window
      xy$h <- rep(h,each=l) #height of each window
      xy$nm <- paste0(folder,1:l**2,'.tif') 
      
      # Create a function to split the raster using gdalUtils::gdal_translate
      split_rast <- function(infile, outfile, llx, lly, win_width, win_height) {
        library(gdalUtils)
        gdal_translate(infile, outfile, 
                       srcwin=c(llx, lly, win_width, win_height),
                       co="COMPRESS=DEFLATE")
      }
      
      parallel::mcmapply(split_rast, in_var, xy$nm, xy$llx, xy$lly, xy$w, xy$h,mc.cores=NC)
      
      
      # and remove the tmp lyr
      system(
        paste0(
          'rm -r ',lyr.tmp
        )
      )
      
      
    }
    
  }
  
  system(
    paste0(
      'rm -r ',folder.tmp
    )
  )
  
}