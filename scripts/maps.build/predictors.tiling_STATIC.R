wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
wd.old <- '/vol/milkun3/Valerio/FLO1K/'

library(gdalUtils); library(foreach);

source(paste0(wd,'scripts/functions_generic.R'))

NC = 10

# create temporary folder
folder.tmp <- dir_(paste0(wd,'tmp.tiling/'))
folder.tiles <- dir_(paste0(wd,'predictors.tiled/'))

# names of the variables
covar.static.names <- c('acc_cells','alt_acc','area_acc','slope_pager_acc') #

continents = c('as','na','af','au','eu','saca')

# set conform target extent in GDAL format -te xmin ymin xmax ymax
target_extent <- c(-180,-90,180,90)
target_crs <- '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'

# set tiles side l
l = 50 # 2500 tiles in total of 864*432 pixels each

# for STATIC
# simply use the global layers and divide them in tiles
for(layer.name in covar.static.names){
  
  #conform file with extent and crs
  lyr.src <- paste0(wd.old,'COVAR.STATIC.ACC/',layer.name,'.tif')
  lyr.tmp <- paste0(folder.tmp,layer.name,'.tmp.tif')
  
  gdalwarp(lyr.src,lyr.tmp,t_srs = target_crs, te = target_extent,
           co="COMPRESS=DEFLATE")
  
  # divide in tiles
  # code from function mc_create_tiles in final.maps.functions
  in_var <- lyr.tmp
  folder <- dir_(paste0(folder.tiles,layer.name,'/'))
  
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

system(
  paste0(
    'rm -r ',folder.tmp
  )
)