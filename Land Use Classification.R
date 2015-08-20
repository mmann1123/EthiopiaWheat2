# creates land use classification for agriculture 

library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(doParallel)
library(maptools)

source('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//SplineAndOutlierRemoval.R')


####################################################
# Land Cover Classification -----------------------------------------------
# Create smoothed stack - Completed on colonial 1 - evi250mCluster*.R ------------
# 
# EVI_Smooth = function(EVI_rows){
#   require(sp)
#   require(raster)      
#   require(MESS)
#   #smooth new EVI data    
#   smooth_holderl = lapply( 1:dim(EVI_rows)[1], function(i)  SplineAndOutlierRemoval(x = EVI_rows[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4) )
#   smooth_holder2 =  lapply( 1:length(smooth_holderl), function(i) if(  is.na(smooth_holderl[[i]][1])  ){ rep(NA,length(pred_dates))}else{smooth_holderl[[i]]}) # if spline failed first obs is NA
#   smooth_matrix = matrix(unlist(smooth_holder2), ncol = length(pred_dates), byrow = TRUE)
#   return(smooth_matrix) 
# } 
# 
# #Determine optimal block size for loading in MODIS stack data
# EVI_stack_in = EVI_stack
# block_width = 3
# nrows = dim(EVI_stack_in)[1]
# nblocks <- nrows%/%block_width
# bs_rows <- seq(1,nblocks*block_width+1,block_width)
# bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
# print('Working on the following rows')
# print(paste(bs_rows))
# 
# 
# #Register the parallel backend
# registerDoParallel(6)
# #
# result <- foreach(i = 1:length(bs_rows), .combine = rbind) %dopar% {
#   require(raster)
#   require(rgdal)
#   EVI_v1 = getValues(EVI_stack_in, bs_rows[i], bs_nrows[i])
#   pheno_matrix =  EVI_Smooth(EVI_rows=EVI_v1)
#   print(paste("Saving pheno_matrix for row",i))
#   save(pheno_matrix,file = paste("EVI_SMOOTH_",bs_rows[i],"_",bs_nrows[i],sep = ""))
# }
# stopImplicitCluster()



# Write out smooth EVI signal files ---------------------------------------

# load multicore output
# broken into  parts to avoid memory issues 
  setwd('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Modis Vege\\EVI_Outputs\\EVI_SMOOTH_STACK')
  f = list.files(getwd(),'EVI_SMOOTH_*')
  row_id = as.numeric(unlist(lapply(1:length(strsplit(f,split = '_')),function(x) strsplit(f,split = '_')[[x]][3])))
  f =f[order(row_id)] 
  
  writer = function(colms){
    print(colms)
    load(f[1])
    result = pheno_matrix[,colms]
    for(file in f[-1]){
      load(file)
      #print(paste(file,"   ", length(pheno_matrix[,colms])))
      result = c(result, pheno_matrix[,colms])
    }
    r = EVI_stack[[1]]
    r = setValues(r, matrix(result,nrow=dim(r)[1],byrow=T))
    writeRaster(r,paste('EVI_sm_rs',colms,'.tif',sep='_'),overwrite=T)
    remove(r,result, pheno_matrix)
  }
  
  load(f[1])
  lapply(100:dim(pheno_matrix)[2],function(x){ writer(x)})

# Set up data
  setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//')
  flist = list.files(".",glob2rx('*MOD13Q1*EVI.project.tif$'), full.names = TRUE)
  EVI_stack = stack(flist[order(flist)])
  dates = strptime( gsub("^.*A([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
  pred_dates =  seq(min(dates), max(dates),by='14 days' ) 
  windows() 
  plot(EVI_stack[[1]])

# # get random sample from ag training
# ET = getData("GADM", country="Ethiopia", level=1)  # download ethiopia perimeter
# ET = spTransform(ET, CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs"))
# set.seed(12)
# point_sample = spsample(ET, n = 200, "random")  # create random point sample
# # smooth EVI signal 
# sample = as.data.frame(extract(EVI_stack,coordinates(point_sample)))
# head(sample)
# smooth_sample = as.data.frame(matrix(nrow=nrow(sample),ncol=length(pred_dates)))
# for(i in 1:dim(sample)[1]){
#   smooth_sample[i,] = SplineAndOutlierRemoval(x = sample[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4)
# }

# read in annual EVI stats 
  EVI_stats = list.files('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Modis Vege\\EVI_Outputs',glob2rx('result*b*v5.tif$'),full.names = T)
  EVI_stats_stack = stack(EVI_stats)

# read in smooth EVI stack 
  EVI_smooth = list.files('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Modis Vege\\EVI_Outputs\\EVI_SMOOTH_STACK',glob2rx('EVI_sm_rs*.tif$'),full.names = T)
  row_id = as.numeric(unlist(lapply(1:length(EVI_smooth),function(x) strsplit(EVI_smooth,split = '_')[[x]][7])))
  EVI_smooth =EVI_smooth[order(row_id)]
  EVI_smooth_stack = stack(EVI_smooth)

# combine raster stacks
  all_stack = stack(EVI_stats_stack,EVI_smooth_stack)

# get random sample from ag training
  ET = getData("GADM", country="Ethiopia", level=1)  # download ethiopia perimeter
  ET = spTransform(ET, CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs"))
  set.seed(12)
  point_sample = spsample(ET, n = 200, "random")  # create random point sample
  sample = as.data.frame(extract(all_stack,coordinates(point_sample)))
  save(sample,file='G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Classification Outputs//classification_sample.RData')
  
  load('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Classification Outputs//classification_sample.RData')
  sapply(sample, class)

# classifier locations 

# classifier locations 
  classes=list(dryag=c(2,82,123,129,153,162,198),
  wetag=c(7,11,15,16,18,27,41,47,53,54,77,85,90,91,100,102,105,106,117,119,122,125,133,150,151,152,166,173,178,182,195),
  agforest=c(6,8,21,32,61,95,111,128,131,137,143,145,168,169,186,191,193),
  arid = c(13,29,56,69,71,78,96,149,156),
  semiarid=c(5,14,19,23,24,26,29,30,35,36,37,40,43,44,45,50,57,59,60,63, 64,73,75,76,80,92,103,107,108,114,135,136,141,146,158,167,170,179,197,200),
  shrub=c(1,12,17,22,25,39,46,48,49,52,55,62,66,67,72,87,88,89,93,97,104,109,110,113,116,118,132,138,142,144,147,148,154,159,160,164,175,185,187,188,189,190,192),
  forest=c(3,4,9,10,28,31,38,42,51,58,68,74,79,81,84,86,94,98,99,101,115,120,121,124,126,127,130,139,140,157,161,163,165,171,174,176,180,181,183,184,194,196),
  wetforest=c(20,33,34,65,70,83,112,134,155,199),
  water=c(172,177))

  sample$class = rep(NA,dim(sample)[1])
  for (i in 1:length(classes)){
    sample$class[classes[[i]]]=names(classes)[i]
  }
  sample = na.omit(sample)
  sample$class = as.factor(sample$class)
  sapply(sample, class)
   
  # classifier algorithm! 
  library(tree)
  library(ggplot2)
  library(party)
  library(randomForest)


  ##############
  # randomforest
  
  myFormula <- class ~ .
  iris_ctree <- ctree(myFormula, data=na.omit(sample))
  table(predict(iris_ctree), sample$class)

  rf <- randomForest(myFormula, data=na.omit(sample), ntree=2000, proximity=T)
  table(predict(rf), sample$class)
  print(rf)
  plot(rf)
  varImpPlot(rf)
  
  # predict on test data 

#Determine optimal block size for loading in MODIS stack data
  block_width = 10
  nrows = dim(all_stack)[1]
  nblocks <- nrows%/%block_width
  bs_rows <- seq(1,nblocks*block_width+1,block_width)
  bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
  print('Working on the following rows')
  print(paste(bs_rows))

#Predict out of sample
  for(i in 1:length(bs_rows)){
    all_stack_v1 = as.data.frame(getValues(all_stack, bs_rows[i], bs_nrows[i]))
    rfPred = predict(rf, newdata=all_stack_v1)
    print(paste("Saving classification for row",i))
    save(rfPred,file = paste("G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Classification Outputs\\rfPred",bs_rows[i],"_",bs_nrows[i],sep = ""))
  }

  # load prediction
  load( paste("G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Classification Outputs\\rfPred",bs_rows[1],"_",bs_nrows[1],sep = ""))
  rfPred_hold = rfPred
  for(i in 2:length(bs_rows)){
    load( paste("G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Classification Outputs\\rfPred",bs_rows[i],"_",bs_nrows[i],sep = ""))
    rfPred_hold = c(rfPred_hold,rfPred)
    }

  # convert to matrix
  r = EVI_stack[[1]]
  r = setValues(r, matrix(rfPred_hold,nrow=dim(r)[1],byrow=T))
  writeRaster(r, "G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Classification Outputs\\Classification.tif")

  setwd( "G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Classification Outputs\\")
  library(plotKML)
  proj4string(eberg_grid) <- CRS("+init=epsg:31467")
  data(SAGA_pal)
  kml(r, colour_scale = SAGA_pal[[1]],file='classification.kml') 
, colour = TWISRT6
# library(caret)
# tc <- trainControl( "sample", number=10, repeats=10, classProbs=TRUE, savePred=T) 
# RFFit <- train(class ~., data=sample, method="rf",  preProc=c("center", "scale"))
# summary(RFFit)
# RF_pred_all_stack <- caret::predict(RFFit, all_stack)
