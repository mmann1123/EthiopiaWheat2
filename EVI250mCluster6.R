# THIS FILE IS USED TO PROCESS EVI TIME SERIES DATA IN ORDER TO OUTPUT 
# A EVI SMOOTHED RASTER STACK. USED ON COLONIAL ONE
# OUTPUTS SAVED IN G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\EVI_SMOOTH_STACK
# Modules to load into colonial one 


module load slurm
module load gcc/4.9.0
module load proj.4/4.8.0
module load gdal
module load R 

sinfo
salloc -p defq -N 1 -t 30
squeue -u mmann1123




remove(list = ls())

install.packages('raster')
library(raster)
install.packages('rgdal')
library(rgdal)
install.packages('sp')
library(sp)
install.packages('doParallel')
library(doParallel)
install.packages('maptools')
library(maptools)
source('//home//mmann1123//250m_EVI_Data//SplineAndOutlierRemoval.R')


 


####################################################
# Land Cover Classification -----------------------------------------------
 


setwd('//home//mmann1123//250m_EVI_Data//')
# Set up data
flist = list.files(".",glob2rx('*MOD13Q1*EVI.project.tif$'), full.names = TRUE)
EVI_stack = stack(flist[order(flist)])
dates = strptime( gsub("^.*A([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
pred_dates =  seq(min(dates), max(dates),by='14 days' ) 
 


EVI_Smooth = function(EVI_rows){
  require(sp)
  require(raster)      
  require(MESS)
  #smooth new EVI data    
  smooth_holderl = lapply( 1:dim(EVI_rows)[1], function(i)  SplineAndOutlierRemoval(x = EVI_rows[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4) )
  smooth_holder2 =  lapply( 1:length(smooth_holderl), function(i) if( is.na(smooth_holderl[[i]][1]) ){ rep(NA,length(pred_dates))}else{smooth_holderl[[i]]})
  smooth_matrix = matrix(unlist(smooth_holder2), ncol = length(pred_dates), byrow = TRUE)
  return(smooth_matrix) 
} 

#Determine optimal block size for loading in MODIS stack data
EVI_stack_in = EVI_stack
block_width = 3
nrows = dim(EVI_stack_in)[1]
nblocks <- nrows%/%block_width
bs_rows <- seq(1,nblocks*block_width+1,block_width)
bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
print('Working on the following rows')
print(paste(bs_rows))


#Register the parallel backend
registerDoParallel(12)
#
result <- foreach(i = 1:length(bs_rows), .combine = rbind) %dopar% {
  require(raster)
  require(rgdal)
  EVI_v1 = getValues(EVI_stack_in, bs_rows[i], bs_nrows[i])
  pheno_matrix =  EVI_Smooth(EVI_rows=EVI_v1)
  print(paste("Saving pheno_matrix for row",i))
  save(pheno_matrix,file = paste("EVI_SMOOTH_",bs_rows[i],"_",bs_nrows[i],sep = ""))
}
stopImplicitCluster()


##########part 1############
# load multicore output
# broken into two parts to avoid memory issues 
f = list.files(getwd(),'EVI_SMOOTH_*')
row_id = as.numeric(unlist(lapply(1:length(strsplit(f,split = '_')),function(x) strsplit(f,split = '_')[[x]][3])))
f =f[order(row_id)] 
load('EVI_SMOOTH_1_3')
result = pheno_matrix[,1:52]
for(file in f[-1]){
  load(file)
  result = rbind(result, pheno_matrix[,1:52])
}

save(result,file='result1_EVI_SM.RData')
 




