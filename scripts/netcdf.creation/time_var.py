#!/usr/bin/env python

import numpy as np
import datetime as dt
import os
import gdal
import netCDF4

basedate = dt.datetime(1900,1,1,0,0,0)

nco = netCDF4.Dataset('/vol/milkun3/Valerio/FLO1K_1.1/upscaled/5min/FLO1K.5min.ts.1961.2015.qmi.nc','a')
timeo = nco.createVariable('time','f4',('time'))
timeo.units = 'days since 1900-01-01 00:00:00'
timeo.standard_name = 'time'
timeo.calendar = 'gregorian'

crso = nco.createVariable('crs','i4')
crso.long_name = 'Lon/Lat Coords in WGS84'
crso.grid_mapping_name='latitude_longitude'
crso.longitude_of_prime_meridian = 0.0
crso.semi_major_axis = 6378137.0
crso.inverse_flattening = 298.257223563

itime=0
years_seq = range(1961,2016)

for year in years_seq:
		
	# read the time values by parsing the filename
	date=dt.datetime(year,1,1,0,0,0)
	print(date)
	dtime=(date-basedate).total_seconds()/86400.
	timeo[itime]=dtime
	itime=itime+1

nco.Conventions='CF-1.6'
nco.title='FLO1K: global maps of mean, maximum and minimum annual streamflow at 1 km resolution from 1960 through 2015'
nco.institution='Radboud University & PBL Netherlands Environmental Assessment Agency'
nco.source='FLO1K v1.0'
nco.scenario='Historical run 1960-2015, yearly timesteps'
nco.comment='Output upscaled to 5m arc min'
nco.references='For documentation, see Barbarossa et al. (Sci. Data, 2017)'
nco.disclaimer='This data was produced with the utmost care, based on methods described in the above article. It can be used by anyone for noncommercial academic research purposes only. The authors are not responsible for any inappropriate use. Please cite the above article when using any part of this data in your work.'
nco.history='Created on 2017-12-05'

nco.close()
