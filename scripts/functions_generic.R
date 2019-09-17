### FUNCTIONS

### create directory if it does not exist
dir_ <- function(name_dir){
  if(!dir.exists(name_dir)) dir.create(name_dir)
  return(name_dir)
}

extract_by_station <- function(t,coord.names = c('new_lon','new_lat'),years_seq = years_seq,Qnames = Qnames,
                               dir.flo1k,prefix = TRUE){
  
  library(raster)
  
  to.export <- list()
  for(var.n in 1:length(Qnames)){
    
    Qname = Qnames[var.n]
    dir.rasters <- paste0(dir.flo1k,Qname,'/')
    
    ext1year <- function(year){
      
      name <- year
      if(prefix) name <- paste0('FLO1K.',Qname,'.',year)
      
      e <- round(extract(
        raster(paste0(dir.rasters,name,'.tif')), #load raster layer
        t[,coord.names] # set of coords to extract the values
      ),3)
      
      e[which(e < 0)] <- NA
      
      return(e)
      
    }
    
    ext <- mapply(ext1year,years_seq,SIMPLIFY = FALSE)
    df <- data.frame(matrix(unlist(ext),nrow=length(ext[[1]])))
    colnames(df) <- years_seq
    
    colnames(df) <- paste0(Qname,'.',colnames(df))
    
    to.export[[var.n]] <- df
  }
  
  return(
    cbind(t,do.call('cbind',to.export))
  )
  
}

