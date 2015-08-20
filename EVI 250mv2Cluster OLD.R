
# this file is outdated. new versions available on colonial one 

# module load gdal
# module load R

remove(list = ls())

#install.packages('raster')
library(raster)
#install.packages('rgdal')
library(rgdal)
#install.packages('sp')
library(sp)
#install.packages('doParallel')
library(doParallel)
#install.packages('maptools')
#library(maptools)
source('//home//mmann1123//250m_EVI_Data//SplineAndOutlierRemoval.R')



# Find representative EVI agricultural signal -----------------------------

# Set up data
  setwd('//home//mmann1123//250m_EVI_Data//')
  flist <- list.files(".",glob2rx('*MOD13Q1*EVI.project.tif$'), full.names = TRUE)
  EVI_stack = stack(flist[order(flist)])
  dates = strptime( gsub("^.*A([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
  pred_dates =  seq(min(dates), max(dates),by='14 days' ) 

# Find agriculture signal
  load('smooth_sample_wetagri_mean.RData')
  load('smooth_sample_dryagri_mean.RData')

# prep DOY rain onset stack
  flist2 <- list.files(path = "//home//mmann1123//DOY//",glob2rx('RS.2.5.9.DOY.prt.*.2.tif$'), full.names = TRUE)
  DOY_stack = stack(flist2) 
  dates2 = strptime( gsub("^.*t.([0-9]+).*$", "\\1", names(DOY_stack)),format='%Y') # create dates to interpolate to
  if(sum(is.na(dates2))==length(dates2)){print('WARNING: change gsub for dates2')}
  names(DOY_stack)=dates2
  if(projection(DOY_stack)!=projection(EVI_stack)){DOY_stack=projectRaster(DOY_stack,EVI_stack,method = 'bilinear')}
  DOY_stack2 = round(DOY_stack, digits=0 )


# Calculate EVI statistics ------------------------------------------------
  
localMaxima <- function(x) {
  if(sum(is.na(x))>0){return()}
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

annualMaxima = function(x,dates){
  # returns location of maximum value by year 
  datesY = format(dates,'%Y')
  a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
  dates[which(x %in% a ) ]
}

annualMinumumNearDOY = function(x,dates,DOY_in){
  #x = EVI values, dates=dates of observation POSIX, DOY_in = '%Y%j' of rain onset
  tempDOY = strptime(DOY_in,'%Y%j')
  tempMINdate = dates[localMaxima(x*-1)]
  grid = expand.grid(tempDOY, tempMINdate)
  
  tempout=do.call(rbind,lapply(split(as.numeric(abs(grid[,1]-grid[,2])),format(grid[,1],'%Y%j')),function(x)x[which.min(x)])) 
  whichwasmin =  which(as.numeric(abs(grid[,1]-grid[,2])) %in% tempout)      
  grid[whichwasmin,2]
}


EVI_Stat = function(EVI_rows,DOY_rows){
  require(sp)
  require(raster)
  #smooth new EVI data    
  smooth_holderl = lapply( 1:dim(EVI_rows)[1], function(i)  SplineAndOutlierRemoval(x = EVI_rows[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4) )
  
  #calculate stats 
  corwet = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) if(!is.na(smooth_holderl[[x]][1])){cor(smooth_holderl[[x]],smooth_sample_dryagri_mean)}else{NA} )  ) ) #correlation with dry agriculture
  cordry = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) if(!is.na(smooth_holderl[[x]][1])){cor(smooth_holderl[[x]],smooth_sample_wetagri_mean)}else{NA} )  ) ) 
  mean = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) mean(smooth_holderl[[x]]) )  ) )
  sd = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) sd(smooth_holderl[[x]]) )  ) )
  max = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) max(smooth_holderl[[x]]) )  ) )
  min = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) min(smooth_holderl[[x]]) )  ) )
  localMax = lapply(1:length(smooth_holderl),function(x)  pred_dates[localMaxima(smooth_holderl[[x]])]   )
  peaks = unlist(lapply(1:length(localMax), function(x) length(localMax[[x]]))) # number of local maximums over period
  
  # find minimum closest to rainy season onset DOY
  colnames(DOY_rows)=format(dates2,"%Y")
  DOY = t(apply(DOY_rows[,],1,function(x) paste(names(x),x,sep='')))
  
  greenupdate = lapply(1:length(smooth_holderl),function(i) annualMinumumNearDOY(x=smooth_holderl[[i]],dates=pred_dates,DOY_in=DOY[i,]))
  greenupdate2= lapply(1:length(smooth_holderl),function(i) annualMaxima((smooth_holderl[[i]]*-1),pred_dates)) # date of annual manimum 
  maxupdate   = lapply(1:length(smooth_holderl),function(i) annualMaxima(smooth_holderl[[i]],pred_dates)) # date of annual maximum 
  require(MESS)
  
  # calculate area under curve for total, increasing (portion), and decreasing (portion) of EVI curve
  aucer = function(row){
    for( elements in 1:(length(greenupdate[[row]])-1) ){
      require(MESS)
      finder_TOTauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
      finder_DECauc = pred_dates>=maxupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
      finder_INCauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <maxupdate[[row]][elements]
      
      finder_TOTauc2 = pred_dates>=greenupdate2[[row]][elements] & pred_dates <greenupdate2[[row]][elements+1]
      finder_DECauc2 = pred_dates>=maxupdate[[row]][elements] & pred_dates <greenupdate2[[row]][elements+1]
      finder_INCauc2 = pred_dates>=greenupdate2[[row]][elements] & pred_dates <maxupdate[[row]][elements]
      #AUC is based on local minimum nearest to DOY of rainfall onset
      TOTauc = auc(pred_dates[finder_TOTauc],smooth_holderl[[row]][finder_TOTauc], type = 'spline')*0.000001  # auc in millions
      DECauc = auc(pred_dates[finder_DECauc],smooth_holderl[[row]][finder_DECauc], type = 'spline')*0.000001
      INCauc = auc(pred_dates[finder_INCauc],smooth_holderl[[row]][finder_INCauc], type = 'spline')*0.000001
      # AUC2 is based on annual minimum
      TOTauc2 = auc(pred_dates[finder_TOTauc2],smooth_holderl[[row]][finder_TOTauc2], type = 'spline')*0.000001  # auc in millions
      DECauc2 = auc(pred_dates[finder_DECauc2],smooth_holderl[[row]][finder_DECauc2], type = 'spline')*0.000001
      INCauc2 = auc(pred_dates[finder_INCauc2],smooth_holderl[[row]][finder_INCauc2], type = 'spline')*0.000001
      return(c(TOTauc,DECauc,INCauc,TOTauc2,DECauc2,INCauc2))
    }
  }
  outer = lapply(1:length(greenupdate),function(x) aucer(x))
  TOTauc = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][1])))
  DECauc = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][2])))
  INCauc = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][3])))
  TOTauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][4])))
  DECauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][5])))
  INCauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][6])))
  
  return(cbind(corwet,cordry,mean,sd,max,min,peaks,TOTauc,DECauc,INCauc,TOTauc2,DECauc2,INCauc2))
} 



#Determine optimal block size for loading in MODIS stack data
block_width = 10 
nrows = dim(EVI_stack)[1]
nblocks <- nrows%/%block_width
bs_rows <- seq(1,nblocks*block_width+1,block_width)
bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
print('Working on the following rows')
print(paste(bs_rows))

#Register the parallel backend
registerDoParallel(4)

result <- foreach(i = 1:length(bs_rows), .combine = rbind) %dopar% {
  require(raster)
  require(rgdal)
  EVI_v1 <- getValues(EVI_stack, bs_rows[i], bs_nrows[i])
  DOY_v1 <- getValues(DOY_stack, bs_rows[i], bs_nrows[i])
  pheno_matrix = EVI_Stat(EVI_v1,DOY_v1)
  print(paste("Saving pheno_matrix for row",i))
  save(pheno_matrix,file = paste("pheno_",bs_rows[i],"_",bs_nrows[i],sep = ""))
}
stopImplicitCluster()



# load multicore output
f = list.files(getwd(),'pheno_*')
row_id = as.numeric(unlist(lapply(1:length(strsplit(f,split = '_')),function(x) strsplit(f,split = '_')[[x]][2])))
f =f[order(row_id)] 
load('pheno_1_20')
result = pheno_matrix
for(file in f[-1]){
  load(file)
  result = rbind(result, pheno_matrix)
}

# put back into raster
for( layer in 1:dim(result)[2]){
  r = EVI_crop[[1]]
  r = setValues(r, matrix(result[,layer],nrow=dim(r)[1],byrow=T))
  names(r) = colnames(result)[layer]
  writeRaster(r,paste('result',colnames(result)[layer],'.tif',sep='_'),overwrite=T)
}


