
remove(list = ls())

#############################################################################
# Smooth and remove outliers of MODIS data ---------------------------------
# Applying Josh's code to smooth, and interpolate remote sensing data



# Preprocess Fix extent etc -----------------------------------------------

source('http://r-gis.net/ModisDownload/ModisDownload.R')
library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(doParallel)
library(maptools)

source('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//SplineAndOutlierRemoval.R')


#copy data over to my folder
setwd('G://Graduate//EDolfi//Summer2014//MODIS//')

flist <- list.files(".",glob2rx('*.hdf$'), full.names = TRUE)
file.copy(flist,to='C://Users//mmann//Desktop//EVI_Data')

# reproject 
dates_MOD=GetDates(13, 37,'MOD13Q1')
dates_MOD

MOD_10_13=dates_MOD[228:317]

files2=list.files(pattern='.hdf')

#Sys.setenv(MRT_DATA_DIR  = "C:/Users/mmann/Documents/MRT/data") # correct missing system file

for (i in (1:length(MOD_10_13))){
  print(i)
  print(length(files2[grep(pattern=MOD_10_13[i],files2)]))
  reprojectHDF(files2[grep(pattern=MOD_10_13[i],files2)], filename=paste(x,'.',MOD_10_13[i],'.tif',sep=''), 
               MRTpath="C:/Users/mmann/Documents/MRT/bin", proj_type='SIN', 
               proj_params='6371007.181 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0', 
               bands_subset="1 1 0 0 0 0 0 0 0 0 0 1",datum='NODATUM', pixel_size=250,
               UL=c('3597645.281','1759154.851'),LR=c('5319998.644','349457.99'))
}


MOD13Q1_bands<-GetBands('MOD13Q1')[c(4,9,10,11)]
MOD13Q1_EVI_NDVI<-GetBands('MOD13Q1')[c(10,11)]

Ethiopia<-shapefile('G:/Graduate/EDolfi/Summer2014/Ethiopia.shp')
ETH_proj<-spTransform(Ethiopia, CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))
library(rgeos)

# create polygons buffering each point by 1km, then unioning all
# generated polygons together
ETH_proj = gBuffer(ETH_proj, width=75000)

for (i in (55:length(MOD_10_13))){
    Pixel=raster(paste(x,'.', MOD_10_13[i],'.', MOD13Q1_bands[1], '.tif',sep=''))
    for (j in (1:length(MOD13Q1_EVI_NDVI))){
        print(paste('read in:',paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.tif',sep='')))
        Raster1=raster(paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.tif',sep=''))
        Raster1=mask(Raster1, ETH_proj)
        Raster1[Pixel==-1 | Pixel==2 | Pixel==3]=NA
        Raster1=Raster1*0.0001  
        Raster_project=projectRaster(Raster1, crs='+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs', method='bilinear')
        writeRaster(Raster_project, paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
        print(paste('write out:',paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.project','.tif',sep='')))
    }
}




# Find representative EVI agricultural signal -----------------------------

# Set up data
  setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//')
  flist = list.files(".",glob2rx('*MOD13Q1*EVI.project.tif$'), full.names = TRUE)
  EVI_stack = stack(flist[order(flist)])
  #xy  = coordinates(EVI_stack)
  dates = strptime( gsub("^.*A([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
  pred_dates =  seq(min(dates), max(dates),by='14 days' ) 
  #holder = matrix(NA,nrow =dim(EVI_Data)[1],ncol=length(pred_dates))   # holder for outputs

# Find agriculture signal
  ET = getData("GADM", country="Ethiopia", level=1)  # download ethiopia perimeter
  ET = spTransform(ET, CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs"))
  
  set.seed(12)
  point_sample = spsample(ET, n = 200, "random")  # create random point sample
  sample = as.data.frame(extract(EVI_stack,coordinates(point_sample)))
  head(sample)
  smooth_sample = as.data.frame(matrix(nrow=nrow(sample),ncol=length(pred_dates)))
  # smooth evi sample 
  for(i in 1:dim(sample)[1]){
    smooth_sample[i,] = SplineAndOutlierRemoval(x = sample[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4)
  }

  # xport to KML to evaluate classification
  smooth_stat=data.frame(max = NA ,min = NA,data = "NA",stringsAsFactors = F) # create variables relevant to KML and google charts
  for(i in 1:dim(smooth_sample)[1]){smooth_stat[i,'data'] = paste(round( smooth_sample[i,],2),collapse=',')}
  for(i in 1:dim(smooth_sample)[1]){smooth_stat[i,'max'] = round(max(smooth_sample[i,]),2) }
  for(i in 1:dim(smooth_sample)[1]){smooth_stat[i,'min'] = round(min(smooth_sample[i,]),2) }
  save( smooth_stat,smooth_sample,point_sample,file='C://Users//mmann/Desktop/fiftennlines.RData')

  smooth_sample_spatial = as.data.frame(cbind(coordinates(point_sample),smooth_sample))
  coordinates(smooth_sample_spatial) <- ~x+y
  proj4string(smooth_sample_spatial) <- CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
  smooth_sample_spatial=spTransform(smooth_sample_spatial,CRS("+proj=longlat"))
  save(smooth_sample_spatial,file='smooth_sample_spatial.RData')
  #load('smooth_sample_spatial.RData')

  kmlfile = paste(getwd(),'kml', "A_EVI_Plot2.kml", sep="/")
  kmlname <- "EVI Timeseries Plots"
  kmldescription <- paste("Data for EVI time series.",
                          'See <a href="https://lpdaac.usgs.gov/products/modis_products_table/mod13q1"</a>')
  icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"
  name <- row.names(smooth_sample_spatial)
  description <- paste('<img src="http://chart.googleapis.com/chart?cht=lc&chs=250x150&chco=006600&chd=t:',smooth_stat$data,'&chxt=y,x&chtt=EVI+Time+Series&chs=300x150&chds=',smooth_stat$min,',',smooth_stat$max,',0,1000&chxr=0,',smooth_stat$min,',',smooth_stat$max,'|1,0,1000" width="275" height="200" alt="Housing Density" />',sep='')
  
  kmlPoints(smooth_sample_spatial, kmlfile=kmlfile, name=name, description=description,
            icon=icon, kmlname=kmlname, kmldescription=kmldescription)
  
# classifier locations 
  dryag=c(2,82,123,129,153,162,198)
  wetag=c(7,11,15,16,18,27,41,47,53,54,77,85,90,91,100,102,105,106,117,119,122,125
          ,133,150,151,152,166,173,178,182,195)
  agforest=c(6,8,21,32,61,95,111,128,131,137,143,145,168,169,186,191,193)  
  arid = c(13,29,56,69,71,78,96,149,156)
  semiarid=c(5,14,19,23,24,26,29,30,35,36,37,40,43,44,45,50,57,59,60,63,
             64,73,75,76,80,92,103,107,108,114,135,136,141,146,
             158,167,170,179,197,200)
  shrub=c(1,12,17,22,25,39,46,48,49,52,55,62,66,67,72,87,88,89,93,97,104
          ,109,110,113,116,118,132,138,142,144,147,148,154,159
          ,160,164,175,185,187,188,189,190,192)
  forest=c(3,4,9,10,28,31,38,42,51,58,68,74,79,81,84,86,94,98,99,101,115,120,121,
           124,126,127,130,139,140,157,161,163,165,171,174,176,180,181,183,184,194,196)
  wetforest=c(20,33,34,65,70,83,112,134,155,199)
  water=c(172,177)
186(forest,forest,agforest,agforest)
162(shrub,dryag,dryag,dryag)
153(agforest,dryag,dryag,dryag)
152(agforest, wetag, wetag, wetag)
151(dryag, wetag, wetag, wetag)
146(semiarid, semiarid, shrub, semiarid)
136(shrub, semiarid, shrub,semiarid)
133(dryag,wetag,wetag,wetag)
108(semiarid, semiarid, shrub, shrub)
107(semiarid, shrub, shrub, semiarid)
93(shrub, shrub, semiarid, shrub)
90(agforest, wetag, wetag, agforest)
79(forest,wetagri,wetagri,agforest)
75(semiarid, arid, semiarid, semiarid)
49(shrub, semiarid, shrub, shrub)
44(shrub, semiarid, shrub, semiarid)
43(semiarid,arid,semiarid, arid)
37(arid, semiarid, semiarid, semiarid)
36(arid, arid, semiarid, semiarid)
11(dryag,wetag,dryag,wetag)
22(shrub, arid, shrub, shrub)
30(semiarid, arid, semiarid, arid)

  # find correlation of pixels with 'agriculture' signal 
  smooth_sample_wetagri_mean = colMeans(smooth_sample[c(wetag),])
  smooth_sample_dryagri_mean = colMeans(smooth_sample[c(dryag),])
  smooth_sample_arid_mean = colMeans(smooth_sample[c(arid),])
  smooth_sample_shrub_mean = colMeans(smooth_sample[c(shrub),])
  smooth_sample_wetforest_mean = colMeans(smooth_sample[c(wetforest),])



plot(pred_dates,smooth_sample_wetagri_mean)
plot(pred_dates,smooth_sample_dryagri_mean)
plot(pred_dates,smooth_sample_arid_mean)
plot(pred_dates,smooth_sample_shrub_mean)
plot(pred_dates,smooth_sample_wetforest_mean)

# plots (used in paper)
  library(ggplot2)
  library(lubridate)
  Type=rep(NA,dim(smooth_sample)[1])
   Type[c(wetag)]="Wet Agri" #add data type label
   Type[c(dryag)]="Dry Agri" #add data type label
   Type[c(agforest)]="Mixed Agri Forest" #add data type label
   Type[c(arid)]="Arid" #add data type label
   Type[c(semiarid)]="Semi-arid" #add data type label
   Type[c(shrub)]="Shrub" #add data type label
   Type[c(forest)]="Forest" #add data type label
   Type[c(wetforest)]="Wet Forest" #add data type label
   Type[c(water)]="water" #add data type label

  plotdata= data.frame(EVI=t(smooth_sample[1,]), Dates=pred_dates, ID=1,Type= Type[1]  )
  names(plotdata)=c('EVI','Dates','ID','Type')
  for (row in 2:dim(smooth_sample)[1]){
      moredata= data.frame(EVI=t(smooth_sample[row,]), Dates=pred_dates,ID=row,Type= Type[row])
      names(moredata)=c('EVI','Dates','ID','Type')
      plotdata = rbind(plotdata,moredata)
  }
  plotdata$Type <- factor(plotdata$Type, levels = c("Arid","Semi-arid","Shrub","Dry Agri","Wet Agri","Forest","Mixed Agri Forest","Wet Forest","water"))
  plotdata$Year = factor(year(plotdata$Dates))
  ggplot(plotdata[plotdata$Type=='Semi-arid' |plotdata$Type=='Wet Agri'|plotdata$Type=='Forest'|plotdata$Type=='Wet Forest',], aes(Dates, EVI) )+geom_point(aes(color=Year))+facet_wrap(~Type, ncol=4)+ stat_smooth(formula = y ~ poly(x, 4),fill="blue", colour="black", size=1.5,alpha=0.2,span=0.99)


  #save(smooth_sample_wetagri_mean,file='smooth_sample_wetagri_mean.RData')  
  #save(smooth_sample_dryagri_mean,file='smooth_sample_dryagri_mean.RData')  
  load('smooth_sample_wetagri_mean.RData')  # load in representative evi curves
  load('smooth_sample_dryagri_mean.RData')

# prep DOY rain onset stack
  flist2 <- list.files(path = "G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DOY//",glob2rx('RS.2.5.9.DOY.prt.*.2.tif$'), full.names = TRUE)
  DOY_stack = stack(flist2) 
  dates2 = strptime( gsub("^.*t.([0-9]+).*$", "\\1", names(DOY_stack)),format='%Y') # create dates to interpolate to
  if(sum(is.na(dates2))==length(dates2)){print('WARNING: change gsub for dates2')}
  names(DOY_stack)=dates2
  if(projection(DOY_stack)!=projection(EVI_stack)){DOY_stack=projectRaster(DOY_stack,EVI_stack,method = 'bilinear')}
  DOY_stack2 = round(DOY_stack, digits=0 )
  windows()
  plot(DOY_stack2[[3]])


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

  annualMaximaValue = function(x,dates){
    # returns location of maximum value by year 
    datesY = format(dates,'%Y')
    a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
    a
    #dates[which(x %in% a ) ]
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
      require(MESS)
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
      DOY = t(apply(round(DOY_rows[,],0),1,function(x) paste(names(x),x,sep='')))
      greenupdate = lapply(1:length(smooth_holderl),function(i) annualMinumumNearDOY(x=smooth_holderl[[i]],dates=pred_dates,DOY_in=DOY[i,]))
      #greenupdate2= lapply(1:length(smooth_holderl),function(i) annualMaxima((smooth_holderl[[i]]*-1),pred_dates)) # date of annual manimum 
      maxupdate   = lapply(1:length(smooth_holderl),function(i) annualMaxima(smooth_holderl[[i]],pred_dates)) # date of annual maximum 
      maxupvalue   = lapply(1:length(maxupdate),function(i) annualMaximaValue(smooth_holderl[[i]],pred_dates) ) # date of annual maximum      # calculate area under curve for total, increasing (portion), and decreasing (portion) of EVI curve
      aucer = function(row){
          hold_list= vector('list',1)
          for( elements in 1:(length(greenupdate[[row]])-1) ){     # the -1 restricts it to 2010,2011,2012
              if(length(greenupdate[[row]])==0){ # if row is empty 
                hold_list[[1]][[1]] =c(NA,NA,NA)  # store NAs if no data available
                hold_list[[1]][[2]] =c(NA,NA,NA)  
                hold_list[[1]][[3]] =c(NA,NA,NA)  
                break 
              }
            require(MESS)
            finder_TOTauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
            finder_DECauc = pred_dates>=maxupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
            finder_INCauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <maxupdate[[row]][elements]
            # AUC is based on local minimum nearest to DOY of rainfall onset
            TOTauc = auc(pred_dates[finder_TOTauc],smooth_holderl[[row]][finder_TOTauc], type = 'spline')*0.000001  # auc in millions
            DECauc = auc(pred_dates[finder_DECauc],smooth_holderl[[row]][finder_DECauc], type = 'spline')*0.000001
            INCauc = auc(pred_dates[finder_INCauc],smooth_holderl[[row]][finder_INCauc], type = 'spline')*0.000001
            # AUC2 is based on annual minimum
            hold_list[[1]][[elements]] =c(TOTauc,DECauc,INCauc) #,TOTaucB,DECaucB,INCaucB
          }
          return(hold_list)
       }
      outer = lapply(1:length(greenupdate),function(x) aucer(x))
      TOTauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][1])))
      TOTauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][1])))
      TOTauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][1])))
      DECauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][2])))
      DECauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][2])))
      DECauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][2])))
      INCauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][3])))
      INCauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][3])))
      INCauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][3])))
      max1 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2010',])))
      max2 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2011',])))
      max3 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2012',])))
      if(length(max1)==0){
        max1 = NA
        max2 = NA
        max3 = NA
      }
      return(cbind(corwet,cordry,mean,sd,max,min,peaks,TOTauc1,TOTauc2,TOTauc3,DECauc1,DECauc2,DECauc3,INCauc1,INCauc2,INCauc3,max1,max2,max3))#TOTaucB,DECaucB,INCaucB
  } 


# newextent = extent(500000,600000,1000000,1100000)
# EVI_crop = crop(EVI_stack,newextent)
# DOY_crop = crop(DOY_stack,newextent)
# plot(EVI_crop[[1]])


#Determine optimal block size for loading in MODIS stack data
  EVI_stack_in = EVI_stack
  DOY_stack_in = DOY_stack
  block_width = 3
  nrows = dim(EVI_stack_in)[1]
  nblocks <- nrows%/%block_width
  bs_rows <- seq(1,nblocks*block_width+1,block_width)
  bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
  print('Working on the following rows')
  print(paste(bs_rows))

#Register the parallel backend
  registerDoParallel(6)
  #
  result <- foreach(i = 1:length(bs_rows), .combine = rbind) %dopar% {
    require(raster)
    require(rgdal)
    EVI_v1 = getValues(EVI_stack_in, bs_rows[i], bs_nrows[i])
    DOY_v1 = getValues(DOY_stack_in, bs_rows[i], bs_nrows[i])
    pheno_matrix =  EVI_Stat(EVI_rows=EVI_v1,DOY_rows=DOY_v1)
    print(paste("Saving pheno_matrix for row",i))
    save(pheno_matrix,file = paste("pheno2_",bs_rows[i],"_",bs_nrows[i],sep = ""))
  }
  stopImplicitCluster()


# load multicore output
  setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data\\250m_EVI_Data')

  f = list.files(getwd(),'pheno2_*')
  #put files in ascending order
  row_id = as.numeric(unlist(lapply(1:length(strsplit(f,split = '_')),function(x) strsplit(f,split = '_')[[x]][2])))
  f =f[order(row_id)] 
  load(f[1])  # load first row group
  result = pheno_matrix
  i=1
  for(file in f[-1]){
    load(file)
    if(dim(pheno_matrix)[2]!=19){print(paste(file,'dim:', dim(pheno_matrix)[1],dim(pheno_matrix)[2],'row:',i))}
    result = rbind(result, pheno_matrix)  # rbind in remaining row
    i=i+1
  }

# put back into raster
  for( layer in 1:dim(result)[2]){
    r = EVI_stack[[1]]
    r = setValues(r, matrix(result[,layer],nrow=dim(r)[1],byrow=T))
    names(r) = colnames(result)[layer]
    writeRaster(r,paste('result',colnames(result)[layer],'b.tif',sep='_'),overwrite=T)
  }

a = raster('')
windows()
plot(a)
a = raster('result_TOTauc2_.tif')
windows()
plot(a)

# replace all EVI_crop !


