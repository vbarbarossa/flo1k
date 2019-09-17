cont.n = 6
partition = 'milkun3'


wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
wd.old <- '/vol/milkun3/Valerio/FLO1K/'


# SETTINGS
continents = c('as','na','af','au','eu','saca')
tiles.number.by.cont = c(2500,2500,900,625,1600,1600) #must be a rootable number as l = sqrt(tiles_number)
covar.static.names <- c('cells','alt','area','slope_pager')
covar.dynamic.names <- c('pav','pma','pmi','psi','tav','tma','tmi','eav','esi')
Qnames = c('qav','qma','qmi') #

NC <- 20
year_seq = c(1961:2015)
tiles_number = tiles.number.by.cont[cont.n] 
rep.cal.max = 20


# load required packages
library(raster); library(foreach); library(gdalUtils); library(nnet); library(caret); library(rgdal);
# & functions
source(paste0(wd,'/scripts/final.maps/final.maps.functions.R'))

# SET current continent
cont = continents[cont.n]


# SET MAIN FOLDERS
dir.store <- paste0('/vol/',partition,'/Valerio/FLO1K_',cont,'/')
if (!dir.exists(dir.store)) dir.create(file.path(dir.store))
folder.store.maps <- dir.store
if (!dir.exists(folder.store.maps)) dir.create(file.path(folder.store.maps))
folder.calculations <- paste0(folder.store.maps,'covariates.temp/')
if (!dir.exists(folder.calculations)) dir.create(file.path(folder.calculations))

folder.static <- paste0(wd.old,'/COVAR.STATIC.ACC/')
folder.dynamic <- paste0(wd,'/COVAR.DYN.ACC/')

for(i in 1:length(covar.static.names)) if (!dir.exists(paste0(folder.calculations,covar.static.names[i]))) dir.create(file.path(paste0(folder.calculations,covar.static.names[i])))
for(i in 1:length(covar.dynamic.names)) if (!dir.exists(paste0(folder.calculations,covar.dynamic.names[i]))) dir.create(file.path(paste0(folder.calculations,covar.dynamic.names[i])))


# PREPARE COVARIATES ##########################################################################

# CREATE TILES for STATIC VARIABLES
covar.static.paths <- paste0(folder.static,covar.static.names,'/',cont,'.tif')
mapply(mc_create_tiles,paste0(folder.calculations,covar.static.names,'/',cont,'/'),
       covar.static.paths,rep(sqrt(tiles_number),length(covar.static.names)),rep(NC,length(covar.static.names)))

# need to rename them in a loop as they are stored all with the same name: final.models.fit
fit.by.var <- list()
proc.by.var <- list()
for(nv in 1:length(Qnames)){
  load(paste0(wd,'/',Qnames[[nv]],'.NN.fit.',rep.cal.max,'rep.RData')) #final.models.fit
  load(paste0(wd,'/',Qnames[[nv]],'.NN.10fold.proc.',rep.cal.max,'rep.RData')) #proc.list.final.models
  
  fit.by.var[[nv]] <- final.models.fit
  proc.by.var[[nv]] <- proc.list.final.models
}  

print(paste0(cont,': '))


for(year in year_seq){

  # CREATE TILES for DYNAMIC VARIABLES
  covar.dynamic.paths <- paste0(folder.dynamic,covar.dynamic.names,'/',cont,'/',year,'.tif')
  mapply(mc_create_tiles,paste0(folder.calculations,covar.dynamic.names,'/',cont,'/'),
         covar.dynamic.paths,rep(sqrt(tiles_number),length(covar.dynamic.names)),rep(NC,length(covar.dynamic.names)))
  
  # PREDICT and store Q for each tile ###########################################################
  
  # create folders to temporarily store the tiles
  folder.store.maps.temp <- mk.dir(paste0(folder.store.maps,Qnames,'.maps.temp/'))
  
  folder.store.maps.temp.cont <- mk.dir(paste0(folder.store.maps.temp,cont,'/'))
  
  # predict Q in parllel for all the tiles
  parallel::mcmapply(predict_tiles,1:tiles_number,mc.cores=NC)
  
  #see which are the missing files
  # which(!1:900 %in% unique(unlist(strsplit(list.files(fold), "\\.")))[-c(2,3)])
  
  #parallel::mcmapply(predict_tiles,seq(13,900,30)[7:30],mc.cores=NC) #676
  # AGGREGATE to CONTINENT ######################################################################
  # folder to store contient-specific maps
  folder.store.maps.byvar <- mk.dir(paste0(folder.store.maps,Qnames,'/'))
  
  folder.store.maps.bycontinent <- mk.dir(paste0(folder.store.maps.byvar,cont,'/'))
  
  mapply(aggregate_to_continent,1:length(Qnames),rep(list(1:tiles_number),3))
  mapply(aggregate_to_continent.cv,1:length(Qnames),rep(list(1:tiles_number),3))
  
  #remove all tiles
  lapply(as.list(folder.store.maps.temp.cont),
         function(x) system(paste0('rm -r ',x)))
  
  #remove DYN files
  lapply(as.list(paste0(folder.calculations,covar.dynamic.names,'/',cont,'/')),
         function(x) system(paste0('rm -r ',x)))
  
  print(paste0(year,' completed. '))
  
} #closes 55 years loop ##########################

system(
  paste0(
    'rm -r ',folder.calculations
  )
)

lapply(as.list(folder.store.maps.temp),
       function(x) system(paste0('rm -r ',x)))