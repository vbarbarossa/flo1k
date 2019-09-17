#inputs to be set by the user
model.string = "Q ~ A + S + H + pav + pma + pmi + psi + tav + tma + tmi + eav + esi + ai"

# set folders where to store results
folder.graphs = dir_(paste0(wd,"graphs/"))
folder.tabs = dir_(paste0(wd,"tabs/"))

# set names of response vars
Qnames = c('qav','qma','qmi')

min_years_monitored = 10

max_missing_months = 0
min_area = 1 # km2

# select which data quality type GRDC points to employ
data_quality = c('high','medium','low')

#calibration/validation sets
time_span = c(1961,2015)
validation_percentage = 0.1

#set starting year of Q.obs and climatic vars
Q.obs.start = 1961
clim.start = 1961

#
no_zeroes = TRUE

# set function to create database (consider here log transformations)
create_database <- function(data){
  df = data.frame(grdc_no = data$grdc_no,
                  Q      = data$MAF_av,#
                  S      = data$slope_page/data$acc_cells,#
                  H      = data$acc_alt/data$acc_cells,#
                  A      = data$acc_area,#
                  pav    = data$pav/data$acc_cells+1,
                  pma    = data$pma/data$acc_cells+1,
                  pmi    = data$pmi/data$acc_cells+1,
                  psi    = data$psi/data$acc_cells, 
                  tav    = (data$tav/10)/data$acc_cells,
                  tma    = (data$tma/10)/data$acc_cells,
                  tmi    = (data$tmi/10)/data$acc_cells,
                  eav    = data$eav/data$acc_cells+1,  
                  esi    = data$esi/data$acc_cells+1,
                  ai     = (data$eav/data$acc_cells)/(data$pav/data$acc_cells)+1 ) 
  
  return(df)
}


#RUN TUNING?
run.tuning = TRUE

#for TUNING
size.val = seq(10,30,5) #6
decay.val = c(0.005,0.01,0.05,0.1)

#for calibration
n.folds = 10

# replicates for calibration routine
rep.cal.max = 20