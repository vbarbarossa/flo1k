# wd: working directory
# folder.graphs: store graphs
# folder.tabs: store tabs

file.static.var1 = paste0(wd,"GRDC.DATA/STATIC/grdc_relocated_taudem_var2.csv")
file.static.var2 = paste0(wd,"GRDC.DATA/STATIC/grdc_relocated_taudem_anthropogenic.csv")
file.static.var3 = paste0(wd,"GRDC.DATA/STATIC/grdc_relocated_taudem_soil&snow.csv")

folder.Q.obs = paste0(wd,"GRDC.DATA/Q/")
file.Q.obs.names = paste0('GRDC_',Qnames,'.csv')
file.Q.obs.missing.months = paste0(folder.Q.obs,'GRDC_missing_months.csv')


file.Q.obs = paste0(folder.Q.obs,file.Q.obs.names[var.n])

### GRDC DATA READ #######################################################################################################################

# to rename col.names
Q.obs.col.names = c('grdc_no',time_span[1]:time_span[2])

# read discharge data
Q.obs.raw = read.csv(file.Q.obs)[,seq.extr]
colnames(Q.obs.raw) <- Q.obs.col.names
# read lacking months table
Q.obs.missing.months = read.csv(file.Q.obs.missing.months)[,seq.extr]
colnames(Q.obs.missing.months) <- Q.obs.col.names

# FILTER for max_missing_months
Q.obs.raw[,2:ncol(Q.obs.raw)][Q.obs.missing.months[,2:ncol(Q.obs.raw)] > max_missing_months] <- NA
Q.obs.raw = Q.obs.raw[!(Q.obs.raw$grdc_no %in% c('1291200','5101201','5608024','3650928','1159510')),] #exclude suspect stations


# EXCLUDE ZEROES
if(no_zeroes){
  vec = c(as.matrix(Q.obs.raw[,2:ncol(Q.obs.raw)]))
  vec[which(vec <= 0)] <- NA
  Q.obs.raw[,2:ncol(Q.obs.raw)] <- as.matrix(vec)
  
}

# calculate avarage and number of total years monitored
Q.obs.raw$total_years <- length(time_span[1]:time_span[2]) - apply(is.na(Q.obs.raw[,2:ncol(Q.obs.raw)]),1,sum)
Q.obs.raw$MAF_av <- apply(Q.obs.raw[,2:ncol(Q.obs.raw)],1, function(x) mean(x,na.rm = TRUE))

Q.obs <- Q.obs.raw[Q.obs.raw$total_years >= min_years_monitored,]

### STATIC VARIABLES PREPARATION #######################################################################################################################

#read variables table
variables_data = read.csv(file.static.var1)
variables_anthropogenic_data = read.csv(file.static.var2)
variables_soil_snow = read.csv(file.static.var3)
#substitute NAs for reservoirs with 0s
variables_anthropogenic_data$reser_area[is.na(variables_anthropogenic_data$reser_area)] <- 0
variables_anthropogenic_data$reser_vol[is.na(variables_anthropogenic_data$reser_vol)] <- 0

#average snowpack
variables_soil_snow$snow_av = apply(variables_soil_snow[,11:13],1,mean)

variables_data = merge(variables_data,variables_anthropogenic_data[,c(1,10:14)],by='grdc_no')
variables_data = merge(variables_data,variables_soil_snow[,c(1,c(10,14))],by='grdc_no')

slope_alt_NAs = which(variables_data$acc_alt <= 0)
if(length(slope_alt_NAs) > 0) variables_data = variables_data[-slope_alt_NAs,]

### KOPPEN CLIMATE GROUPING

# map at 5' resolution
koppen_map = raster(paste0(wd,'Koppen_zoning_1986_2010.tif'))
locations = variables_data[,c('new_lon','new_lat')]

values = extract(koppen_map,locations)

variables_data$koppen <- values

#variables_data = variables_data[which(!is.na(variables_data$koppen)),]

# group class 4 and 5 together
variables_data$koppen[variables_data$koppen == 5] <- 4
variables_data$koppen <- as.factor(variables_data$koppen)


# EXTRA FILTERS

variables_data = variables_data[variables_data$new_area >= min_area,] #filter for min_area

#FILTER for quality based on data_quality
if(length(data_quality) == 3){variables_data = variables_data[(variables_data$quality == data_quality[1]) | 
                                                              (variables_data$quality == data_quality[2]) | 
                                                              (variables_data$quality == data_quality[3]),]}
if(length(data_quality) == 2){variables_data = variables_data[(variables_data$quality == data_quality[1]) | 
                                                              (variables_data$quality == data_quality[2]),]}
if(length(data_quality) == 1){variables_data = variables_data[(variables_data$quality == data_quality),]}

### TOTAL DATABASE ######################################################################################################################

# define grdc_no vectors for variables and Q.obs
select.var = variables_data[,'grdc_no']
select.Q = Q.obs[,'grdc_no']

#merge with variables database
select.Q = select.Q[select.Q %in% select.var]

variables.data.filtered = variables_data[variables_data$grdc_no %in% select.Q,]
Q.obs.filtered = Q.obs[Q.obs$grdc_no %in% select.Q,]

# store tables
write.csv(variables.data.filtered,paste0(folder.tabs,Qname,'.static.filtered.data.csv'),row.names = FALSE)
write.csv(Q.obs.filtered,paste0(folder.tabs,Qname,'.GRDC.obs.filtered.csv'),row.names = FALSE)

