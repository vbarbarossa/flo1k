#!/usr/bin/env python
'''
Convert a bunch of GDAL readable grids to a NetCDF Time Series.
Here we read a bunch of files that have names like:
/usgs/data0/prism/1890-1899/us_tmin_1895.01
/usgs/data0/prism/1890-1899/us_tmin_1895.02
...
/usgs/data0/prism/1890-1899/us_tmin_1895.12
'''

import numpy as np
import datetime as dt
import os
import gdal
import netCDF4
import re

ds = gdal.Open('/vol/milkun5/Valerio/FLO1K/FINAL.MAPS/FLO1K/prova/FLO1K.qav.1960.tif')
a = ds.ReadAsArray()
nlat,nlon = np.shape(a)

b = ds.GetGeoTransform() #bbox, interval
lon = np.arange(nlon)*b[1]+b[0]
lat = np.arange(nlat)*b[5]+b[3]


basedate = dt.datetime(1900,1,1,0,0,0)

# create NetCDF file
nco = netCDF4.Dataset('time_series.nc','w',clobber=True)

# chunking is optional, but can improve access a lot: 
# (see: http://www.unidata.ucar.edu/blogs/developer/entry/chunking_data_choosing_shapes)
chunk_lon=1646
chunk_lat=636
chunk_time=12

# create dimensions, variables and attributes:
nco.createDimension('lon',nlon)
nco.createDimension('lat',nlat)
nco.createDimension('time',None)
timeo = nco.createVariable('time','f4',('time'))
timeo.units = 'days since 1900-01-00 00:00:00'
timeo.standard_name = 'time'

lono = nco.createVariable('lon','f4',('lon'))
lono.units = 'degrees_east'
lono.standard_name = 'longitude'

lato = nco.createVariable('lat','f4',('lat'))
lato.units = 'degrees_north'
lato.standard_name = 'latitude'

# create container variable for CRS: lon/lat WGS84 datum
crso = nco.createVariable('crs','i4')
crso.long_name = 'Lon/Lat Coords in WGS84'
crso.grid_mapping_name='latitude_longitude'
crso.longitude_of_prime_meridian = 0.0
crso.semi_major_axis = 6378137.0
crso.inverse_flattening = 298.257223563

# create short integer variable for temperature data, with chunking
qavo = nco.createVariable('qav', 'f4',  ('time', 'lat', 'lon'), 
   zlib=True,chunksizes=[chunk_time,chunk_lat,chunk_lon],fill_value=-1)
qavo.units = 'm3/s'
qavo.long_name = 'average yearly streamflow over the 12 mean monthly flow values of the year'
qavo.standard_name = 'Average annual streamflow'
qavo.grid_mapping = 'crs'
qavo.set_auto_maskandscale(False)

nco.Conventions='CF-1.5'

#write lon,lat
lono[:]=lon
lato[:]=lat

itime=0

#step through data, writing time and data to NetCDF
for root, dirs, files in os.walk('/vol/milkun5/Valerio/FLO1K/FINAL.MAPS/FLO1K/prova/'):
    dirs.sort()
    files.sort()
    for f in files:
	
		# read the time values by parsing the filename
		year=int(f[10:14])
		date=dt.datetime(year,1,1,0,0,0)
		print(date)
		dtime=(date-basedate).total_seconds()/86400.
		timeo[itime]=dtime
	   # min temp
		tmn_path = os.path.join(root,f)
		print(tmn_path)
		tmn=gdal.Open(tmn_path)
		a=tmn.ReadAsArray()  #data
		qavo[itime,:,:]=a
		itime=itime+1

nco.close()