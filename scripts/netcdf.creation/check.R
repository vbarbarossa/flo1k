library(raster); library(ncdf4)

r = raster('D:/FLO1K.1.1.ts.1961.2015.qavnew.nc', varname='qav', band = 1)

nc <- nc_open('D:/FLO1K.1.1.ts.1961.2015.qav.nc')
nc2 <- nc_open('F:/pcrglobwb_CRU_30min_qcMONAVG_NC3.nc')
nc_open('F:/CRU_TS_3.24/cru_ts3.24.1901.2015.pre.dat.nc')


nc_atts <- ncatt_get(nc, 0)
names(nc_atts)

t <- ncvar_get(nc, varid='time',verbose = F)

tunits <- ncatt_get(nc,"time","units")
nt <- dim(t)
nt

time_d <- as.Date(t, format="%j", origin=as.Date("1900-01-01"))

qav_array <- ncvar_get(nc,'qav')



date_time_start <- as.POSIXct(tunits$value, format = "%Y%m%dT%H%M%SZ", tz = "UTC")

nc_close(nc)