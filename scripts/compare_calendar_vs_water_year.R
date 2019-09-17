library(raster)

years_seq <- 1961:2015
Qnames = c('qav','qma','qmi') #

wd <- '/vol/milkun3/Valerio/FLO1K_1.1/'
source(paste0(wd,'scripts/functions_generic.R'))

dir.tmp <- dir_(paste0(wd,'tmp_compare'))
dir.maps.cal <- '/vol/milkun5/Valerio/FLO1K/'
dir.maps.wy <- paste0(wd,'FLO1K_wy/')
dir.comparison <- dir_(paste0('/vol/milkun3/Valerio/FLO1K/comparison_calendarANDwater_year/')) #updated results of calendar year

rasterOptions(tmpdir = dir.tmp)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# retrieve SIM values (FLO1K) # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# read GRDC re-allocated stations
stations <- foreign::read.dbf(paste0(wd,
                                     'GRDC.DATA/MONITORING.STATIONS/RE-ALLOCATED/grdc_relocated_taudem.dbf'))
# filter north america stations grdc_no first digit = 4
# stations.na <- stations#[trunc(as.numeric(stations$grdc_no)/10**6) == 4,]
# if(filter_for_na) stations.na <- stations[trunc(as.numeric(stations$grdc_no)/10**6) == 4,]

# extract_by_station <- function(t,coord.names = c('new_lon','new_lat'),years_seq = years_seq,Qnames = Qnames,
#                                dir.flo1k,prefix = TRUE,NC = 10){
# 
#   to.export <- list()
#   for(var.n in 1:length(Qnames)){
# 
#     Qname = Qnames[var.n]
#     dir.rasters <- paste0(dir.flo1k,Qname,'/')
# 
#     ext1year <- function(year){
# 
#       name <- year
#       if(prefix) name <- paste0('FLO1K.',Qname,'.',year)
# 
#       e <- round(extract(
#         raster(paste0(dir.rasters,name,'.tif')), #load raster layer
#         t[,coord.names] # set of coords to extract the values
#       ),3)
# 
#       e[which(e < 0)] <- NA
# 
#       return(e)
# 
#     }
# 
#     ext <- parallel::mcmapply(ext1year,years_seq,SIMPLIFY = FALSE,mc.cores=NC)
#     df <- data.frame(matrix(unlist(ext),nrow=length(ext[[1]])))
#     colnames(df) <- years_seq
#     df$av <- apply(df,1,function(x) round(mean(x,na.rm = TRUE),3))
#     df$me <- apply(df,1,function(x) round(median(x,na.rm = TRUE),3))
#     df$sd <- apply(df,1,function(x) round(sd(x,na.rm = TRUE),3))
# 
#     colnames(df) <- paste0(Qname,'.',colnames(df))
# 
#     to.export[[var.n]] <- df
#   }
# 
#   return(
#     cbind(t,do.call('cbind',to.export))
#   )
# 
# }
# 
# grdc.cal.sim <- extract_by_station(t = stations.na[,c('grdc_no','new_lon','new_lat')],
#                                    years_seq = years_seq,Qnames = Qnames,
#                                    dir.flo1k=dir.maps.cal,NC = 5)
# 
# grdc.wy.sim <- extract_by_station(t = stations.na[,c('grdc_no','new_lon','new_lat')],
#                                   years_seq = years_seq,Qnames = Qnames,
#                                   dir.flo1k=dir.maps.wy,prefix = FALSE,NC = 5)
# 
# write.csv(grdc.cal.sim,paste0(dir.comparison,'flo1k.calendaryear.extracted.to.grdc.csv'))
# write.csv(grdc.wy.sim,paste0(dir.comparison,'flo1k.wateryear.extracted.to.grdc.csv'))

grdc.cal.sim <- read.csv(paste0(dir.comparison,'flo1k.calendaryear.extracted.to.grdc.csv'))
grdc.wy.sim <- read.csv(paste0(dir.comparison,'flo1k.wateryear.extracted.to.grdc.csv'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# retrieve OBS values (GRDC)  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
filter_for_na = TRUE

dir.obs.cal <- paste0('/vol/milkun3/Valerio/FLO1K/tabs/')
dir.obs.wy <- paste0(wd,'tabs/')

yr_threshold = 30

for(mod in 1:2){
  dir.obs = c(dir.obs.cal,dir.obs.wy)[mod]
  tab.sim.in = list(grdc.cal.sim,grdc.wy.sim)[[mod]]
  mods <- c('m.cal','m.wy')
  
  obs.l = list()
  sim.l = list()
  
  for(var.n in 1:length(Qnames)){
    
    Qname = Qnames[var.n]
    col.names <- c('grdc_no',years_seq)
    
    # format OBS tab
    tab = read.csv(paste0(dir.obs,Qname,'.GRDC.obs.filtered.csv'))
    
    tab <- tab[,c('grdc_no',paste0('X',years_seq))]
    if(filter_for_na) tab <- tab[trunc(as.numeric(tab$grdc_no)/10**6) == 4,]
    colnames(tab) <- col.names
    
    tab$tot <- length(years_seq) - apply(is.na(tab[,as.character(years_seq)]),1,sum)
    
    # consider stations monitored at least 30 years
    tab <- tab[tab$tot >= yr_threshold,] 
    
    # format SIM tab
    tab.sim <- tab.sim.in
    tab.sim <- tab.sim[tab.sim$grdc_no %in% tab$grdc_no,c('grdc_no',paste0(Qname,'.',years_seq))]
    
    #sort the tabs
    tab <- tab[order(tab$grdc_no),col.names]
    tab.sim <- tab.sim[order(tab.sim$grdc_no),]
    colnames(tab.sim) <- col.names
    
    #substitute sim values with NAs
    for(i in as.character(years_seq)) tab.sim[is.na(tab[,i]),i] <- NA
    
    tab[,as.character(years_seq)] <- round(tab[,as.character(years_seq)],3)
    
    format_tab <- function(df){
      d <- cbind(df,
                 data.frame(
                   av = apply(df[,as.character(years_seq)],1,function(x) round(mean(x,na.rm = TRUE),3)),
                   me = apply(df[,as.character(years_seq)],1,function(x) round(median(x,na.rm = TRUE),3)),
                   sd = apply(df[,as.character(years_seq)],1,function(x) round(sd(x,na.rm = TRUE),3))
                 )
      )
      
      colnames(d) <- c('grdc_no',paste0(Qname,'.',colnames(d[,2:ncol(d)])))
      
      return(d)
      
    }
    
    obs.l[[var.n]] <- format_tab(tab); sim.l[[var.n]] <- format_tab(tab.sim); 
    
  }
  
  obs <- obs.l[[1]]
  sim <- sim.l[[1]]
  for(n in 2:length(Qnames)){
    obs <- merge(obs,obs.l[[n]],by='grdc_no')
    sim <- merge(sim,sim.l[[n]],by='grdc_no')
    
  }
  
  assign(mods[mod],list(obs = obs,sim = sim))
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# calculate goodness-of-fit stats # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# select only common stations to calendar and water year
common.stations <- m.wy[[1]]$grdc_no[m.wy[[1]]$grdc_no %in% m.cal[[1]]$grdc_no]
for(i in 1:2){
  m.cal[[i]] <- m.cal[[i]][m.cal[[i]]$grdc_no %in% common.stations,]
  m.wy[[i]] <- m.wy[[i]][m.wy[[i]]$grdc_no %in% common.stations,]
}


source(paste0(wd,'scripts/stats.functions.R'))

## diagnostic
plot(log10(m.cal[[2]]$qav.av),log10(m.wy[[2]]$qav.av))
# text(log10(m.cal[[2]]$qav.av),log10(m.wy[[2]]$qav.av),labels = as.character(m.cal[[2]]$grdc_no))
abline(1,1)
abline(-1,1)

## remove off points (disalignments between networks)
st.to.remove <- m.cal[[2]]$grdc_no[abs(log10(m.cal[[2]]$qav.av)-log10(m.wy[[2]]$qav.av)) > 1]
for(i in 1:2){
  m.cal[[i]] <- m.cal[[i]][!m.cal[[i]]$grdc_no %in% st.to.remove,]
  m.wy[[i]] <- m.wy[[i]][!m.wy[[i]]$grdc_no %in% st.to.remove,]
}


stats.tab <- data.frame(matrix(ncol = 6,nrow = 2))
colnames(stats.tab) <-paste0(rep(Qnames,each=2),c('.lt','.yr'))
row.names(stats.tab) <- c('calendar_year','us_water_year')

for(n in 1:length(Qnames)){
  
  Qname = Qnames[n]
  
  for(r in 1:2){
    
    m <- get(mods[r])
    ob <- m[['obs']][,paste0(Qname,'.av')]
    si <- m[['sim']][,paste0(Qname,'.av')]
    
    stats.tab[r,(n*2-1)] <- paste0(
      round(
        r.squared(
          ob,si
        )
        ,3)
      ,' (',length(ob),')')
    
    
    ob <- c(as.matrix(m[['obs']][,paste0(Qname,'.',years_seq)]))
    si <- c(as.matrix(m[['sim']][,paste0(Qname,'.',years_seq)]))
    stats.tab[r,(n*2)] <- paste0(
      round(
        r.squared(
          ob[!is.na(ob)],si[!is.na(ob)]
        )
        ,3)
      ,' (',length(ob),')')
    
    
    
  }
  
}

stats.tab.log <- data.frame(matrix(ncol = 6,nrow = 2))
colnames(stats.tab.log) <-paste0(rep(Qnames,each=2),c('.lt','.yr'))
row.names(stats.tab.log) <- c('calendar_year','us_water_year')

for(n in 1:length(Qnames)){
  
  Qname = Qnames[n]
  
  for(r in 1:2){
    
    m <- get(mods[r])
    
    ob <- log10(m[['obs']][,paste0(Qname,'.av')])
    si <- log10(m[['sim']][,paste0(Qname,'.av')])
    
    stats.tab.log[r,(n*2-1)] <- paste0(
      round(
        r.squared(
          ob,si
          
        )
        ,3)
      ,' (',length(ob),')')
    
    ob <- log10(c(as.matrix(m[['obs']][,paste0(Qname,'.',years_seq)])))
    si <- log10(c(as.matrix(m[['sim']][,paste0(Qname,'.',years_seq)])))
    stats.tab.log[r,(n*2)] <- paste0(
      round(
        r.squared(
          ob[!(is.na(ob) | is.infinite(ob) | is.nan(ob) | is.na(si) | is.infinite(si) | is.nan(si))],
          si[!(is.na(ob) | is.infinite(ob) | is.nan(ob) | is.na(si) | is.infinite(si) | is.nan(si))]
        )
        ,3)
      ,' (',length(ob),')')
    
    
  }
  
}

save.path <- paste0(dir.comparison,'stats.tab.comparison_wy_cal_global.csv')
if(filter_for_na) save.path <- paste0(dir.comparison,'stats.tab.comparison_wy_cal_na.csv')

write.csv(cbind(data.frame(trans = rep(c('','log10'),each=2)),rbind(stats.tab,stats.tab.log)),
          save.path)


system(paste0('rm -r ',dir.tmp))
