library(raster)
library(rgdal)
library(foreign)
rasterOptions(tmpdir="D:/MAF_30s/R_calculations/temp")
r = raster("D:/MAF_30s/DIR/au_dir")

r[r%in%c(8)] <- 6
r[r%in%c(4)] <- 7
r[r%in%c(2)] <- 8
r[r%in%c(128)] <- 2
r[r%in%c(64)] <- 3
r[r%in%c(32)] <- 4
r[r%in%c(16)] <- 5

writeRaster(r,'eu_dir.tif',format = 'GTiff',datatype='INT1U', overwrite=TRUE)
# dataType(r)

check_file = 'prec_prova2.tif'
if(file.exists(check_file)){
  file.remove(check_file)
  file.remove(paste0(check_file,'.aux.xml'))
}

p = raster("D:/MAF_30s/Py_calculations/clim_acc/clim_res/ptemp_res")
e = extent(r)
pe = crop(p,e)
writeRaster(pe,'prec_prova2.tif',format = 'GTiff',datatype='INT2S', overwrite=TRUE)


shell("cd C:/Program Files/Microsoft MPI/Bin & mpiexec -n 8 AreaD8 -p D:/MAF_30s/R_calculations/au_dir.tif -ad8 D:/MAF_30s/R_calculations/au_acc.tif -nc")

shell("cd C:/Program Files/Microsoft MPI/Bin & mpiexec -n 8 AreaD8 -p D:/MAF_30s/R_calculations/af_dir.tif -ad8 D:/MAF_30s/R_calculations/af_acc.tif -wg D:/MAF_30s/R_calculations/af_pmax_res.tif -nc")
check_file <- 'au_acc_pre2.tif'
if(file.exists(check_file)){
  file.remove(check_file)
}
str = "cd C:/Program Files/Microsoft MPI/Bin & mpiexec -n 8 AreaD8 -p D:/MAF_30s/R_calculations/au_dir.tif -ad8 D:/MAF_30s/R_calculations/au_acc_pre2.tif -wg D:/MAF_30s/R_calculations/prec_prova2.tif -nc"
shell(str)

r = raster('au_acc_pre.tif')
d = raster('au_acc.tif')
rd = r/d
writeRaster(rd,'au_pre.tif')



r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
r[] <- 1:ncell(r)
e <- extent(-180, 0, 0, 90)
re <- extend(r, e)








# r[r%in%c(1)] <- 6
# r[r%in%c(2)] <- 3
# r[r%in%c(4)] <- 2
# r[r%in%c(8)] <- 1
# r[r%in%c(16)] <- 4
# r[r%in%c(32)] <- 7
# r[r%in%c(64)] <- 8
# r[r%in%c(128)] <- 9
# r[r%in%c(0)] <- 5
# 
# writeRaster(r, 'D:/MAF_30s/DIR/au_dir.asc',
#             format = 'ascii', datatype='INT2U', overwrite=TRUE)