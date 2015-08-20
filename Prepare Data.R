rm(list=ls(all=TRUE))

library(raster)
library(foreign)
library(maptools)


dem = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DIVA DATA//ETH_msk_alt//ETH_msk_alt.tif')
slope = terrain(dem,'slope')
aspect = terrain(dem,'aspect')
rough = terrain(dem,'roughness')

writeRaster(dem,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DIVA DATA//dem.tif')

writeRaster(slope,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DIVA DATA//slope.tif')
writeRaster(aspect,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DIVA DATA//aspect.tif')
writeRaster(rough,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//DIVA DATA//roughness.tif')



# Plot NDVI curves --------------------------------------------------------

### just to show what I did
# EVI_MOD13Q1_fill<-approxNA(EVI_MOD13Q1_stack, method='linear', rule=2)
# randEVI_MOD13Q1=sampleRandom(EVI_MOD13Q1_fill, size=20)
# df2=data.frame(randEVI_MOD13Q1)
# 
# names(df2)[1:92]<-MOD_10_13
# df2$ID<-1:20
# head(df2)
# 
# df2_melt<-melt(df2, id.var='ID')
# head(df2_melt)

## if you want to run it
setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data')
evi<-read.table('EVItimeseries.csv',sep=" ")
sortnames <- c("ID", "variable")
evi = evi[do.call("order", evi[sortnames]), ]
windows()
ggplot(evi[evi$ID<=5,], aes(x=variable,y=value, group=factor(ID), colour=factor(ID)))+geom_line() 
ggplot(evi[evi$ID>5&evi$ID<=10,], aes(x=variable,y=value, group=factor(ID), colour=factor(ID)))+geom_line() 
ggplot(evi[evi$ID>10&evi$ID<=15,], aes(x=variable,y=value, group=factor(ID), colour=factor(ID)))+geom_line() 
ggplot(evi[evi$ID>15&evi$ID<=20,], aes(x=variable,y=value, group=factor(ID), colour=factor(ID)))+geom_line() 




write.table(df2_melt, file='test1.csv')




# Deal with UDEL CWD data -------------------------------------------------


# read UDEL
for(i in paste('def150',1981:2010,sep='.')){
    data = read.fortran(paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,sep=''),c("F8.3","F8.3","12F8.1"))
    holder = data.frame(lat=data[,1]*1000,lon=data[,2]*1000,mean=NA,sd=NA)
    mean = apply(data[,3:14],1,mean)
    sd = apply(data[,3:14],1,sd)
    holder[,3]=mean
    holder[,4]=sd
    write.csv(holder, paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,'_stat',sep='')) 
    print('1')
}

# calc and write mean rasters
for(i in paste('def150.',1981:2010,'_stat',sep='')){
  data = read.csv(paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,sep=''))
  data2= na.omit(data[,2:4])
  coordinates(data2)= ~lat+lon
  gridded(data2)= T
  data = raster(data2)
  crs(data)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(data,paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,'_mn.tif',sep=''),overwrite=T)
  print('1')
}

# calc and write sd raster
for(i in paste('def150.',1981:2010,'_stat',sep='')){
  data = read.csv(paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,sep=''))
  data2= na.omit(data[,c(2:3,5)])
  coordinates(data2)= ~lat+lon
  gridded(data2)= T
  data = raster(data2)
  crs(data)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(data,paste("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",i,'_sd.tif',sep=''),overwrite=T)
  print(paste(i,extent(data)@xmin,extent(data)@xmax,extent(data)@ymin,extent(data)@ymax))
}

#summarize
setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//')
mean = list.files("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",'_mn')
mean = mean[grep('def150',mean)]
mean.stack = stack(mean)
mean = mean(mean.stack)
writeRaster(mean,'CWD_mn_8110.tif')

sd = list.files("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//UDEL Data//Water Budget//",'_sd')
sd.stack = stack(sd)
sd = mean(sd.stack)
writeRaster(sd,'CWD_sd_8110.tif',overwrite=T)


# Mosaic STRM Data
# setting the working directory:

setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//')
all=list.files(pattern='.tif$')
 
out1 = mosaic(raster(all[1]),raster(all[2]),raster(all[3]),raster(all[4]),raster(all[5]),raster(all[6]),raster(all[7]),raster(all[8]),raster(all[9]),raster(all[10]),raster(all[11]),fun=min)
writeRaster(out1,'STRM_ET90m.tif',overwrite=T)
plot(out1)
slope = terrain(out1,'slope')
aspect = terrain(out1,'aspect')
rough = terrain(out1,'roughness')
writeRaster(slope,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//slope.tif')
writeRaster(aspect,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//aspect.tif')
writeRaster(rough,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//roughness.tif')

dem = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//slope_UTM.tif')
rough = terrain(dem,'roughness')
writeRaster(rough,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//roughness_UTM.tif')
slope_factor = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//slope_UTM.tif')
slope_factor = crop(slope_factor, raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif'))
slope_factor[slope_factor<=5] = 1
slope_factor[slope_factor>5 & slope_factor <=10] = 2
slope_factor[slope_factor >10] = 3
writeRaster(slope_factor,datatype='INT1U','G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//slope_factor.tif', overwrite=T)

#AFSIS soil maps



# Commands used in GME to summarize polygon data
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Agroecology_UTM.shp", field="GRIDCODE2", prefix="AgEco", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\ETH_adm3_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Agroecology_UTM.shp", field="GRIDCODE2", prefix="AgEco", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\ETH_adm2_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Agroecology_UTM.shp", field="GRIDCODE2", prefix="AgEco", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\National_Agro_Ecological_Data_UTM.shp", field="GRIDCODE", prefix="AgZn", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Soil_UTM.shp", field="SOIL_CODE2", prefix="Soil", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Agroecology_UTM.shp", field="GRIDCODE2", prefix="AgEco", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\National_Agro_Ecological_Data_UTM.shp", field="GRIDCODE", prefix="AgZn", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Soil_UTM.shp", field="SOIL_CODE2", prefix="Soil", thematic=TRUE, proportion=TRUE);

isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Long_Year_ARF_UTM.shp", field="Zone_Id", prefix="LY", thematic=TRUE, proportion=TRUE);
isectpolypoly(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", poly="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\WLRC_Data_Original\Long_Year_ARF_UTM.shp", field="Zone_Id", prefix="LY", thematic=TRUE, proportion=TRUE);

# Slope terrain
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\slope_UTM.tif", prefix="Slp", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\aspect_UTM.tif", prefix="Asp", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\flow_accum_UTM.tif", prefix="Flw", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\roughness_UTM.tif", prefix="Rgh", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\srtm_90m_UTM.tif", prefix="Elev", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\STRM\UTM_Projected\slope_factor.tif", prefix="SlpF", thematic=TRUE, proportion=TRUE);

# water deficit
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\UDEL Data\Water Budget//CWD_mn_8110_UTM_RS.tif", prefix="CWD_mn", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\UDEL Data\Water Budget//CWD_mn_8110_UTM_RS.tif", prefix="CWD_mn", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\UDEL Data\Water Budget//CWD_sd_8110_UTM_RS.tif", prefix="CWD_sd", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Ethiopia_adm4_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\UDEL Data\Water Budget//CWD_sd_8110_UTM_RS.tif", prefix="CWD_sd", thematic=FALSE, proportion=FALSE);

# Transport and distance
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif", prefix="DRcap", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Transportation//Transport Cost Surface//EucDist_pp50k_250m.tif", prefix="D50k", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Transportation//Transport Cost Surface//TravelTimev1Hours_Rcap250m.tif", prefix="TTRC", thematic=FALSE, proportion=FALSE);
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Transportation//Transport Cost Surface//TravelTimev1Hours_pp50k250m.tif", prefix="TT50k", thematic=FALSE, proportion=FALSE);

# soil properties from http://www.isric.org/data/soil-property-maps-africa-1-km
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Soil Properties/CEC_sd4_M_UTM_250m.tif", prefix="CEC4", thematic=FALSE, proportion=FALSE );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Soil Properties/PHIHO5_sd4_M_UTM_250m.tif", prefix="PH4", thematic=FALSE, proportion=FALSE );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Soil Properties/ORCDRC_sd4_M_UTM_250m.tif", prefix="OC4", thematic=FALSE, proportion=FALSE );

# DOY of meher rainy season
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\RS.25.9.DOY.2010.tif", prefix="DOY3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\RS.25.9.DOY.2011.tif", prefix="DOY4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\RS.25.9.DOY.2012.tif", prefix="DOY5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# total rainfall of meher rainy season
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_meher.2010.tif", prefix="PPTM3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_meher.2011.tif", prefix="PPTM4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_meher.2012.tif", prefix="PPTM5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# total rainfall of belg rainy season
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_belg.2010.tif", prefix="PPTB3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_belg.2011.tif", prefix="PPTB4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\TRMM\3B42_sm_belg.2012.tif", prefix="PPTB5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# EVI statistics Total AUC
setwd('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\Modis Vege\\EVI_Outputs\\')
flist1 = list.files(".",glob2rx('*result_*auc*_b.tif$'), full.names = TRUE)
flist2 = list.files(".",glob2rx('*result_*max*_b.tif$'), full.names = TRUE)
flist3 = list.files(".",glob2rx('*result_*peak*_b.tif$'), full.names = TRUE)
flist = c(flist1,flist2,flist3)
auc = stack(flist)
auc[auc<0]=NA
cors = raster("result_corwet_b.tif")
cors2 = raster("result_cordry_b.tif")
auc[cors<.50 & cors2<.50]=NA
names(auc) = unlist(strsplit(unlist(lapply(1:length(flist),function(x) strsplit(flist,'./')[[x]][2])),'.tif'))
for(layer in 1:dim(auc)[3]){writeRaster(auc[[layer]],paste(names(auc)[layer],'v5.tif',sep='_'),overwrite=T )} #v2 = auc < 0 is set to NA, v3: auc<0 and dry agri cor < .75 = NA , v3: auc<0 and dry agri cor < .85 = NA, v4 same as v3 but wet agri cor, v5 auc[cors<.50 & cors2<.50]=NA


# EVI statistics Total AUC
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_TOTauc1_b_v2.tif", prefix="EVIT3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_TOTauc2_b_v2.tif", prefix="EVIT4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_TOTauc3_b_v2.tif", prefix="EVIT5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# EVI statistics Increasing AUC
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_INCauc1_b_v2.tif", prefix="EVII3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_INCauc2_b_v2.tif", prefix="EVII4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_INCauc3_b_v2.tif", prefix="EVII5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# EVI statistics Decreasing AUC
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_DECauc1_b_v2.tif", prefix="EVID3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_DECauc2_b_v2.tif", prefix="EVID4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_DECauc3_b_v2.tif", prefix="EVID5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# EVI statistics MAX EVI
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_max1_b_v2.tif", prefix="EVIMX3", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_max2_b_v2.tif", prefix="EVIMX4", thematic=FALSE, proportion=FALSE,metrics=c("MN") );
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_max3_b_v2.tif", prefix="EVIMX5", thematic=FALSE, proportion=FALSE,metrics=c("MN") );

# EVI statistics Other
isectpolyrst(in="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Administrative Areas\Kebele_WLRC_UTM.shp", raster="G:\Faculty\Mann\Projects\Ethiopia Project\GIS Data\Modis Vege\EVI_Outputs\result_peaks_b_v2.tif", prefix="EVIMPK", thematic=FALSE, proportion=FALSE,metrics=c("MN") );  # # of peaks over time series




# reclassify dataset
library(foreign)
data=read.dbf("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//WLRC_Data_Original//Long_Year_ARF_UTM.dbf",as.is=T)
head(data)
uniq=unique(data$Zone)
for(i in 1:length(uniq)){
  data$Zone_Id[data$Zone == uniq[i]] = i
}
write.dbf(data,"G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//WLRC_Data_Original//Long_Year_ARF_UTM.dbf")




##############################################################################################
# Create Transportation Cost Surface --------------------------------------
LC = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Modis Landcover//2012_LC1km_UTM3.tif')
# reclassify landcover as time (hrs) of traversing 1km pixel (Definitions found: G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Transportation\Transport Cost Surface\Land Cover Costs.txt)
Cost = LC
Cost[Cost>=1 & Cost<=6 | Cost==8] = 0.311
Cost[Cost==7|Cost==9|Cost==10|Cost>=12&Cost<=16 ] = 0.207
Cost[Cost==0|Cost==11|Cost==254]=NA

Roads = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Transportation//OCHAroad//Eth_Road_UTM_3.tif')
Roads[Roads==7]=0  # remove railways
Roads[is.na(Roads)]=0
Roads[Roads==6]= 0.0125
Roads[Roads==5]= 0.0166
Roads[Roads==4]= 0.0333
Roads[Roads==3]= 0.0666 
Roads[Roads==3]= 0.0666 
Roads[LC==13]  = 0.0286  # urban speeds are lower

Cost[Roads!=0]=0         # clear landcover for roads
Cost = Cost+Roads
writeRaster(Cost,'G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Transportation//Transport Cost Surface//TimeCostv3.tif',overwrite=T)




#############################################################################################
# Read TRMM bin files -----------------------------------------------------
#The Tropical Rainfall Measuring Mission (TRMM) is a joint U.S.-Japan satellite mission to monitor tropical and subtropical precipitation and to estimate its associated latent heating. TRMM was successfully launched on November 27, at 4:27 PM (EST) from the Tanegashima Space Center in Japan.
#The rainfall measuring instruments on the TRMM satellite include the Precipitation Radar (PR), an electronically scanning radar operating at 13.8 GHz; TRMM Microwave Image (TMI), a nine-channel passive microwave radiometer; and Visible and Infrared Scanner (VIRS), a five-channel visible/infrared radiometer.
#The purpose of the 3B42 algorithm is to produce TRMM-adjusted merged-infrared (IR) precipitation and root-mean-square (RMS) precipitation-error estimates.  The algorithm consists of two separate steps.  The first step uses the TRMM VIRS and TMI orbit data (TRMM products 1B01 and 2A12) and the monthly TMI/TRMM Combined Instrument (TCI) calibration parameters (from TRMM product 3B31) to produce monthly IR calibration parameters.  The second step uses these derived monthly IR calibration parameters to adjust the merged-IR precipitation data, which consists of GMS, GOES-E, GOES-W, Meteosat-7, Meteosat-5, and NOAA-12 data.  The final gridded, adjusted merged-IR precipitation (mm/hr) and RMS precipitation-error estimates have a 3-hourly temporal resolution and a 0.25-degree by 0.25-degree spatial resolution.  Spatial coverage extends from 50 degrees south to 50 degrees north latitude.
#The daily accumulated (beginning at 00Z and ending at 21Z; unit: mm) rainfall product is derived from this 3-hourly product. The data are stored in flat binary. The file size is about 2.25 MB (uncompressed).


for(year in 2010:2013){
  for(day in 1:365){
    setwd(paste('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\',year,'\\',sprintf("%03d", day),sep=''))
    trmm <- raster(xmn=0, xmx=360, ymn=-50, ymx=50, ncol=1440, nrow=400)  # set up empty raster
    extent = extent(31.93,48.5,2.7,15.43) # fix ethiopia extent
    files <- list.files(pattern='.bin$') 
    for (f in files)  {   # for all files 
      trmm[] <- readBin(f, 'double', n=576000, size = 4, endian='big')  # read binary files
      x = flip(rotate(trmm), 'y') 
      x = crop(x,extent)
      #To recover the original floating-point values in mm/h, divide by 100.
      #Missings are given the 2-byte-integer missing value, -31999. The
      x[x==-31999] =NA
      x = x/100 * 24    # convert to mm/hr and then mm/day
      
      write.location = 'C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\'
      writeRaster(x,  paste(write.location,unlist(strsplit(files,'.bin')),'.tif',sep=''),overwrite=T )
      print('done')
    } 
  }
}

# create summaries for belg and meher growing seasons (feb-jun, jun-oct)  http://www.pecad.fas.usda.gov/highlights/2008/11/eth_25nov2008/
setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\')
stacker = stack()
for (year in 2010:2013){
  for(month in 2:5){
    files = list.files(,paste(year,sprintf("%02d", month),sep='.')) 
    stacked = stack(files)
    stacker = stack(stacker, stacked)
  }

  # calc stats
  
  sum.stack = sum(stacker)
  mean.stack = mean(stacker)
  sd.stack = calc(stacker,sd)
  # reproject and resample to 250m 
  sample = raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif')
  print('project 1')
  sum.stack =  projectRaster(from=sum.stack,sample,method='bilinear')
  print('project 2')
  mean.stack =  projectRaster(from=mean.stack,sample,method='bilinear')
  print('project 3')
  sd.stack =  projectRaster(from=sd.stack,sample,method='bilinear')
  
  writeRaster(sum.stack,paste('3B42_sm_belg',year,'tif',sep='.'),overwrite=T)
  writeRaster(mean.stack,paste('3B42_mn_belg',year,'tif',sep='.'),overwrite=T)
  writeRaster(sd.stack,paste('3B42_sd_belg',year,'tif',sep='.'),overwrite=T)
}



setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\')
stacker = stack()
for (year in 2010:2013){
  for(month in 6:10){
    files = list.files(,paste(year,sprintf("%02d", month),sep='.')) 
    stacked = stack(files)
    stacker = stack(stacker, stacked)
  }

  sum.stack = sum(stacker)
  mean.stack = mean(stacker)
  sd.stack = calc(stacker,sd)
  # reproject and resample to 250m 
  sample = raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif')
  print('project 1')
  sum.stack =  projectRaster(from=sum.stack,sample,method='bilinear')
  print('project 2')
  mean.stack =  projectRaster(from=mean.stack,sample,method='bilinear')
  print('project 3')
  sd.stack =  projectRaster(from=sd.stack,sample,method='bilinear')
 
  writeRaster(sum.stack,paste('3B42_sm_meher',year,'tif',sep='.'),overwrite=T)
  writeRaster(mean.stack,paste('3B42_mn_meher',year,'tif',sep='.'),overwrite=T)
  writeRaster(sd.stack,paste('3B42_sd_meher',year,'tif',sep='.'),overwrite=T)
}

# create summaries for annual (jan-dec)   
setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\')
stacker = stack()
for (year in 2010:2013){
    for(month in 1:12){
        files = list.files(,paste(year,sprintf("%02d", month),sep='.')) 
        stacked = stack(files)
        stacker = stack(stacker, stacked)
    }
    
    # calc stats
    
    sum.stack = sum(stacker)
   # mean.stack = mean(stacker)
   # sd.stack = calc(stacker,sd)
    # reproject and resample to 250m   
    sample = raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif')
    print('project 1')
    sum.stack =  projectRaster(from=sum.stack,sample,method='bilinear')
#     print('project 2')
#     mean.stack =  projectRaster(from=mean.stack,sample,method='bilinear')
#     print('project 3')
#     sd.stack =  projectRaster(from=sd.stack,sample,method='bilinear')
#     
    writeRaster(sum.stack,paste('3B42_sm_annl',year,'tif',sep='.'),overwrite=T)
   # writeRaster(mean.stack,paste('3B42_mn_annl',year,'tif',sep='.'),overwrite=T)
   # writeRaster(sd.stack,paste('3B42_sd_annl',year,'tif',sep='.'),overwrite=T)
    print('1')
}

################################################
# Figure out properties of 'the rainy season'  
setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\')
sm2010 = raster('3B42_sm_annl.2010.tif')
sm_meh_2010 = raster('3B42_sm_meher.2010.tif')

windows()
plot(sm_meh_2010/sm2010)
plot(sm_meh_2010) # 20mm is about 10% of total rainfall for highlands
mean(sm_meh_2010[],na.rm=T)




#############################################################################################
# identify day of the year 20 mm rainfall accumulated over three consecutive rainfall events with no dry spell length of 10 consecutive days in the next 30 days. Tesfaye Fantaye, Kindie (CIMMYT)
setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\')

sevenconsecutive = function(rasterin){
  # function calculates the number of consecutive TRUES 
  s = rasterToPoints( rasterin )
  X = s[,'x']
  Y = s[,'y']
  s = s[,3:dim(s)[2]]
  # is maximum dry spell length less than 7 days? 
  s  = apply(s,MARGIN=1, function(x){max( rle(c(x))$lengths[rle(c(x))$values==T], na.rm=T )<7} )
  #s = rasterFromXYZ( data.frame(x=X,y=Y,true=s) )
  return(rasterFromXYZ( data.frame(x=X,y=Y,true=s) ))
}


startyear = 2010
startmonth = 2       # may (5) is ideal start month (Kindie says so)
endmonth = 8         # end of aug (9) is best end 
mmofrain = 25   
prtofrain = 0.025     # enter percent (decimal form) of total rain for the meher season that must occur in 10day period
mmORprt = 'prt'      # enter 'mm' for using mm or 'prt' for using percentage rule
daysforaccumulation = 9  #(NOTE:put in 1 less than actual so put 2 for 3 ) # the # days for mmofrain to accumulate  # 

for (year in startyear:2013){
  stacker = stack() 
  # stack all daily data for period of interest
  for(month in startmonth:endmonth){ 
    files = list.files(,paste(year,sprintf("%02d", month),sep='.')) 
    stacked = stack(files)
    stacker = stack(stacker, stacked)
  }
  # hold total rainfall for period of 
  
  # placeholder for start of rainy season
  dayofyear = stacker[[1]] 
  dayofyear[]=NA
  # convert to day of year (Julian date) -- use POSIXlt
  startdayofyear = strptime(paste(startmonth,1,startyear,sep='/'), "%m/%d/%Y")$yday+1  # formula for day of the year
  
  # find cells that match criteria
  for(day in 1:(dim(stacker)[3]-(1+daysforaccumulation+29))){
      if(mmORprt == 'mm'){
          threedaysumtest = sum(stacker[[day:(day+daysforaccumulation)]]) >= mmofrain      # TRUE 3 day period more than mmofrain 
          # find dry days 
          thirtydayzerorain = stacker[[(day+daysforaccumulation):(day+daysforaccumulation+29)]]==0  # TRUE zero rain days
          # returns answer to "is maximum dry spell length less than 7 days?"
          thirtydaytest = sevenconsecutive(thirtydayzerorain)
          remove(thirtydayzerorain)
          bothtests = threedaysumtest*thirtydaytest  
          # replace day of year if none exists and bothtests passed
          dayofyear[bothtests==T & is.na(dayofyear)]= startdayofyear + day -1 #minus 1 b/c day starts at 1
          print(day/(dim(stacker)[3]-(1+daysforaccumulation+29))*100)  #print progress 
      } else if(mmORprt=='prt'){
          if(day ==1 ){
               # calculate the total rainfall for this meher rain season
               stacker_meher = stack()
                   for(month in 6:10){  # get sum of meher rainfall
                       files = list.files(,paste(year,sprintf("%02d", month),sep='.')) 
                       stacked_meher = stack(files)
                       stacker_meher = stack(stacker_meher, stacked_meher)
                   }
               sum_meher = sum(stacker_meher ,na.rm=T)   
               remove(stacked_meher,stacker_meher)
          }
          # does accumulation exceed the criteria for % of total rainfall?             
          threedaysumtest = (sum(stacker[[day:(day+daysforaccumulation)]]) / sum_meher) >= prtofrain
          # find dry days 
          thirtydayzerorain = stacker[[(day+daysforaccumulation):(day+daysforaccumulation+29)]]==0  # TRUE zero rain days
          # returns answer to "is maximum dry spell length less than 7 days?"
          thirtydaytest = sevenconsecutive(thirtydayzerorain)
          remove(thirtydayzerorain)
          bothtests = threedaysumtest*thirtydaytest  
          # replace day of year if none exists and bothtests passed
          dayofyear[bothtests==T & is.na(dayofyear)]= startdayofyear + day -1 #minus 1 b/c day starts at 1
          print(paste(round(day/(dim(stacker)[3]-(1+daysforaccumulation+29))*100,2),'%'))  #print progress 
        
      } else{print('please enter "mm" or "prt" for mmORprt')}
    }
  
  print('year done')
  plot(dayofyear)
  # fill missing data with highest allowed start date
  dayofyear[is.na(dayofyear)]= max(dayofyear[],na.rm=T)
  # reproject and resample to 250m 
  sample = raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif')
  projectRaster(from=dayofyear,sample,method='ngb')
  if(mmORprt == 'prt'){  writeRaster(dayofyear,paste('RS',(prtofrain*100),daysforaccumulation,'DOY',mmORprt,year,startmonth,'tif',sep='.'), overwrite=T)}else{writeRaster(dayofyear,paste('RS',mmofrain,daysforaccumulation,'DOY',mmORprt,year,'tif',sep='.'), overwrite=T)}
  remove(day,dayofyear,threedaysumtest,thirtydaytest,bothtests,stacker)
}

 

#############################################################################
# 
#   USE EVI 250mv3.R for this section
#
# Smooth and remove outliers of MODIS data ---------------------------------
# Applying Josh's code to smooth, and interpolate remote sensing data
# 
#     # Set up data
#     setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Modis Vege//')
#     flist <- list.files(".",glob2rx('*MOD13Q1*EVI.project.tif$'), full.names = TRUE)
#     EVI_stack = stack(flist[order(flist)])
#     xy = coordinates(EVI_stack)
#     dates = strptime( gsub("^.*A([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
#     pred_dates =  seq(min(dates), max(dates),by='14 days' ) 
#      
#     # spline smoothing, interpolation, and gap filling
#     EVI_Data = getValues(EVI_stack)
#     source('C://Users//mmann//Documents//Ethiopia Project//Test Code//SplineAndOutlierRemoval.R')
#     holder = matrix(NA,nrow =dim(EVI_Data)[1],ncol=length(pred_dates))   # holder for outputs
#     pb <- txtProgressBar(min = 0, max = dim(EVI_Data)[1], style = 3)
#     for(i in 1:dim(EVI_Data)[1]){
#         if(sum(is.na((EVI_Data[i,])))>40){ next } # skip missing value rows
#         else{
#             holder[i,] = SplineAndOutlierRemoval(x = EVI_Data[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4)
#         }
#         setTxtProgressBar(pb, i)
#     }
#     close(pb)
#     save(holder, file='holder.RData')#saves memory, restart R after save
#     EVI_Stack = cbind(xy,holder)
#     remove(holder, xy)
#     save(EVI_Stack, file='EVI_Stack.RData')      
#     load('EVI_Stack.RData')
#     library(rgdal)
#     EVI_Stack = as.data.frame(EVI_Stack)
#     coordinates(EVI_Stack)=~x+y
#     #names(EVI_Stack) = dates
#     proj4string(EVI_Stack)=CRS("+init=epsg:20137") # set it to lat-long
#     gridded(EVI_Stack) = TRUE
#     save(EVI_Stack, file='EVI_Stack_gridded.RData')
#     load(file='EVI_Stack_gridded.RData')
#     # export to individual tif files
#     for(i in 1:dim(EVI_Stack)[2]){  
#         a = raster(EVI_Stack[,i])
#         writeRaster(a, paste('EVI_Spline_',format(pred_dates[i], "%Y%m%d"),'.tif',sep=''),overwrite=T)
#     }
#     
# 
#     #set up smoothed stack
#     setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Modis Vege//')
#     flist <- list.files(".",glob2rx('EVI_Spline_*.tif$'), full.names = TRUE)
#     EVI_smooth_stack = stack(flist[order(flist)])
#     dates = strptime( gsub("^.*_([0-9]+).*$", "\\1", names(EVI_smooth_stack)),format='%Y%m%d') # create dates to interpolate to
#     setZ(EVI_smooth_stack,pred_dates)
#     
#     set.seed(13)
#     sample = sampleRandom(EVI_smooth_stack,50,xy=T)
#     plot(pred_dates,sample[10,-c(1,2)])   #plot without xy
# 
# # plot with DOY of rainy season
#     setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\TRMM\\')
#     flist2 <- list.files(".",glob2rx('RS.2.5.9.DOY.prt.*.2.tif$'), full.names = TRUE)
#     DOY_stack = stack(flist2) 
#     # note: change gsub to extact only numbers
#     dates2 = strptime( gsub("^.*t.([0-9]+).*$", "\\1", names(DOY_stack)),format='%Y') # create dates to interpolate to
#     if(sum(is.na(dates2))==length(dates2)){print('WARNING: change gsub for dates2')}
#     names(DOY_stack)=dates2
# 
#     # check projection, reproject if necesary 
#     if(projection(DOY_stack)!=projection(EVI_smooth_stack)){DOY_stack=projectRaster(DOY_stack,EVI_smooth_stack,method = 'ngb')}
#     # extract DOY of rainy season for same locations
#     DOY = as.data.frame(extract(DOY_stack,sample[,c('x','y')]))
#     names(DOY)=format(dates2,"%Y")
#     DOY = na.omit(cbind(DOY))
#     DOY = t(apply(DOY,1,function(x) paste(names(x),x,sep='')))
#     sampleDOY = sample[as.numeric(row.names(DOY)),]
#     
#     
#     localMaxima <- function(x) {
#         # Use -Inf instead if x is numeric (non-integer)
#         y <- diff(c(-.Machine$integer.max, x)) > 0L
#         rle(y)$lengths
#         y <- cumsum(rle(y)$lengths)
#         y <- y[seq.int(1L, length(y), 2L)]
#         if (x[[1]] == x[[2]]) {
#             y <- y[-1]
#         }
#         y
#     }
# 
#     annualMaxima = function(x,dates){
#           # returns location of maximum value by year 
#           datesY = format(dates,'%Y')
#           a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
#           dates[which(x %in% a ) ]
#       }
#  
# annualMinumumNearDOY = function(x,dates,DOY_in){
#     #x = EVI values, dates=dates of observation POSIX, DOY_in = '%Y%j' of rain onset
#      tempDOY = strptime(DOY_in,'%Y%j')
#      tempMINdate = dates[localMaxima(x*-1)]
#      grid = expand.grid(tempDOY, tempMINdate)
#       
#     tempout=do.call(rbind,lapply(split(as.numeric(abs(grid[,1]-grid[,2])),format(grid[,1],'%Y%j')),function(x)x[which.min(x)])) 
#     whichwasmin =  which(as.numeric(abs(grid[,1]-grid[,2])) %in% tempout)      
#     grid[whichwasmin,2]
#     #cbind(grid[whichwasmin,],abs(grid[whichwasmin,1]-grid[whichwasmin,2])) 
#       
#      #strptime(row.names(do.call(rbind,lapply(split(as.numeric(abs(grid[,1]-grid[,2])),format(grid[,1],'%Y%j')),function(x)x[which.min(x)])) ),'%Y%j')
#    }
# annualMinumumNearDOY(x=sample[rownumber,-c(1,2)],dates=pred_dates,DOY_in=DOY[3,])
# 
#     rownumber= 27 #14 20 27
#     plot( pred_dates,sampleDOY[rownumber,-c(1,2)],xlab=pred_dates)   #plot without xy
#     dated = strptime(DOY[rownumber,],'%Y%j')
#     #names(unclass(dated))
#     dated$mday = dated$mday -0
#     abline(v=as.numeric(dated))
#     #abline(v=as.numeric(pred_dates[localMaxima(sampleDOY[rownumber,-c(1,2)]*-1)]),col='green')
#     #abline(v=as.numeric(pred_dates[localMaxima(sampleDOY[rownumber,-c(1,2)])]),col='red')
#     abline(v=as.numeric(annualMaxima(x=sampleDOY[rownumber,-c(1,2)],dates=pred_dates)),col='orange')
#     #abline(v=as.numeric(pred_dates[annualMaxima(sampleDOY[rownumber,-c(1,2)]*-1,pred_dates)]),col='blue')
#     #Sys.sleep(1)
#     abline(v=as.numeric(annualMinumumNearDOY(x=sampleDOY[rownumber,-c(1,2)],dates=pred_dates,DOY_in=DOY[rownumber,])),col='green')
# 
# 
#  #plot DOY 
#     rownumber= 1
#     plot(format(pred_dates,'%j'),sample[rownumber,-c(1,2)])   #plot without xy
#     abline(v=as.numeric(format(strptime(DOY[rownumber,],'%Y%j'),'%j')))
#     Onset_Greenness_Increase.Num_Modes_01
#     
# # plot with DOY of EVI onset
#     setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Modis Phenology\\TiffsMercator')
#     flist2 <- list.files(".",'Onset_Greenness_Increase.Num_Modes_01.tif$', full.names = TRUE)
#     ONSET_stack = stack(flist2) 
#     # note: change gsub to extact only numbers
#     dates2 = strptime( gsub("^.*.A([0-9]+).*$", "\\1", names(ONSET_stack)),format='%Y') # create dates to interpolate to
#     if(sum(is.na(dates2))==length(dates2)){print('WARNING: change gsub for dates2')}
#     names(ONSET_stack)=dates2
#     # check projection, reproject if necesary 
#     if(projection(ONSET_stack)!="+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs"){ONSET_stack=projectRaster(ONSET_stack,EVI_smooth_stack,method = 'ngb')}
#     #save(ONSET_stack,file='ONSET_stack1.RData')
#     load(file='ONSET_stack1.RData')
# 
#     # extract DOY of rainy season for same locations
#     ONSET = as.data.frame(extract(ONSET_stack,sample[,c('x','y')]))
#     ONSET[ONSET>32766]=NA
#     library(lubridate)
#     #ONSET2 = matrix(nrow = dim(ONSET)[1],ncol = dim(ONSET)[2])
#     for(row in 1:dim(ONSET)[1]){
#         for(col in 1:dim(ONSET)[2]){
#             ONSET[row,col] = dmy("1/1/2000")+days(ONSET[row,col])            
#         }
#     }
# 
#          
#     rownumber= 36
#     plot(as.numeric(pred_dates),sample[rownumber,-c(1,2)],xlab=pred_dates)   #plot without xy
#     abline(v=ONSET[rownumber,])
# 
# 
# 
# # Summarize Stacks for outputs to regressions -----------------------------
# 
#   # take an individual x,y and process summary stastics
#   loc = sample[,c('x','y')][3]
#   EVIloc = extract(EVI_smooth_stack,loc)[1,]
#   DOYloc = extract(DOY_stack,loc)[1,]
#   print(paste(EVIloc[1], DOYloc[1],i))
#   
# 
#   DOYyear     = format(strptime(names(DOYloc),'X%Y.%m.%d'),"%Y")
#   DOYloc      = paste(DOYyear,DOYloc,sep='')  # put in %Y%j format
#   greenupdate = annualMinumumNearDOY(x=EVIloc,dates=pred_dates,DOY_in=DOYloc) # date of local minimum closest to rainy season DOY
#   maxupdate   = annualMaxima(EVIloc,pred_dates) # date of annual maximum 
# 
#   require(MESS)
#   i=1
#   finder = pred_dates>=greenupdate[i] & pred_dates <greenupdate[i+1]
#   TOTauc = auc(pred_dates[finder],EVIloc[finder], type = 'spline')
#   TOTauc
#   finder = pred_dates>=greenupdate[i] & pred_dates <maxupdate[i]
#   INCauc = auc(pred_dates[finder],EVIloc[finder], type = 'spline')
#   INCauc
#   finder = pred_dates>=maxupdate[i] & pred_dates <greenupdate[i+1]
#   DECauc = auc(pred_dates[finder],EVIloc[finder], type = 'spline')
#   DECauc
# 
# 
# # Screen out non-agricultural signals -------------------------------------
#     ET = getData("GADM", country="Ethiopia", level=1)  # download ethiopia perimeter
#     ET = spTransform(ET, CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs"))
#     setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Modis Vege//')
#     # get smoothed stack
#     flist <- list.files(".",glob2rx('EVI_Spline_*.tif$'), full.names = TRUE)
#     EVI_smooth_stack = stack(flist[order(flist)])
#     EVI_smooth_stack = crop(EVI_smooth_stack,extent(ET))
#     dates = strptime( gsub("^.*_([0-9]+).*$", "\\1", names(EVI_smooth_stack)),format='%Y%m%d') # create dates to interpolate to
#     setZ(EVI_smooth_stack,pred_dates)
#     
#     # identify agricultural signals
#     EVI_smooth = getValues(EVI_smooth_stack)
#     xy = coordinates(EVI_smooth_stack)
#     EVI_smooth = cbind(xy, EVI_smooth)
#     remove(xy, EVI_smooth_stack)
#     set.seed(12)
#     rows = sample(1:dim(EVI_smooth)[1],200)
#     rows # last row should be 1395015
#     sampler = EVI_smooth[rows,]
#     sampler=na.omit(sampler)
#     for(rownumber in 1:dim(sampler)[1]){plot( pred_dates,sampler[rownumber,-c(1,2)],xlab=pred_dates,main=paste(rownumber))  }
#     #rows of 'agriculture?"  
#     sampler_agrows = c(142,140,139,138,137,134,130,129,128,126,123,121,119,118,117,114,111,110,109,107,104,102,101,99,93,92,87,85,82,81,80,76,75,74,71,70,65,63,59,55,54,49,47,46,44,42,41,36,35,34,33,28,27,26,22,20,18,16,14,13,10,9,8,7)
#     
#     # Export to KML check locations
#     library(plotKML)  
#     sampler2 = as.data.frame(sampler)
# write.csv(sampler2,'.//kml//sampler2.csv')
# 
#     coordinates(sampler2) <- ~x+y
#     proj4string(sampler2) <- CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
#     agriculture = sampler2[sampler_agrows,]
#     #kml(agriculture, file = "agri_0.kml")
# 
# testout = agriculture[,1]
# names(testout) ='Data'
# testout$max =NA
# testout$min =NA
# 
# for(i in 1:dim(testout)[1]){testout[i,1] = paste(round(slot(agriculture,'data')[i,],2),collapse=',')}
# for(i in 1:dim(testout)[1]){testout[i,'max'] = round(max(slot(agriculture,'data')[i,]),2) }
# for(i in 1:dim(testout)[1]){testout[i,'min'] = round(min(slot(agriculture,'data')[i,]),2) }
# 
# testout=spTransform(testout,CRS("+proj=longlat"))
# kmlfile <- paste(getwd(),'kml', "A_EVI_Plot.kml", sep="/")
# kmlname <- "EVI Timeseries Plots"
# kmldescription <- paste("Data for EVI time series.",
#         'See <a href="https://lpdaac.usgs.gov/products/modis_products_table/mod13q1"</a>')
# icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"
# name <- row.names(testout)
# description <- paste('<img src="http://chart.googleapis.com/chart?cht=lc&chs=250x150&chco=006600&chd=t:',testout$Data,'&chxt=y,x&chtt=EVI+Time+Series&chs=300x150&chds=',testout$min,',',testout$max,',0,1000&chxr=0,',testout$min,',',testout$max,'|1,0,1000" width="275" height="200" alt="Housing Density" />',sep='')
# 
# kmlPoints(testout, kmlfile=kmlfile, name=name, description=description,
#           icon=icon, kmlname=kmlname, kmldescription=kmldescription)
#                         
# 
# 
# rownumber = 8
#     plot( pred_dates,sampler[rownumber,-c(1,2)],xlab=pred_dates,main=paste(rownumber))  
#     actual_agri_sampler_rows = c(8,13,20,26,34,44,54,55,65,74,99,101,107,117,119,121,123,130,134,137)
#     
#     # plot actual agricultural plots
#     actual_agri = as.data.frame(sampler[actual_agri_sampler_rows,])
#     plot( format(pred_dates,'%j'),actual_agri[1,-c(1,2)] )  
#     for(i in 1:20){points( format(pred_dates,'%j'),actual_agri[i,-c(1,2)],col='red' )}
#     plot( pred_dates,actual_agri[1,-c(1,2)] )  
#     for(i in 1:20){points( pred_dates,actual_agri[i,-c(1,2)],col='red' )}
#     smooth_mean = smooth.spline(pred_dates,actual_agri[i,-c(1,2)])
# 
#     # create one data set based on DOY
#     names(actual_agri)=c('x','y',paste(format(pred_dates,'%j')))
#     data_actual_agri = as.data.frame(t(actual_agri[,-c(1,2)]))
#    # data_actual_agri$date = as.numeric(row.names(data_actual_agri))
#     head(data_actual_agri)    
# 
#     # calculate mean observation 
#     actual_agri_mean = rowMeans(data_actual_agri)
#     plot(pred_dates, actual_agri_mean)
#     library(reshape) #    smooth spline does same thing more or less
#     test = melt(actual_agri,id.vars = c('x','y'))
#     smooth_mean = smooth.spline(test$variable,test$value)$y
#     plot(pred_dates,smooth_mean)
# 
#    cbind( apply(actual_agri[,-c(1,2)], 1, function(x) cor(actual_agri_mean, x))
#     ,apply(actual_agri[,-c(1,2)], 1, function(x) cor(smooth_mean, x)))
#     plot( pred_dates,actual_agri[12,-c(1,2)] )  
# 
#     # xport to kml to check what correlation level is best
#     sampler2 = as.data.frame(sampler)
#     sampler2$cor = apply(sampler2[,-c(1,2)], 1, function(x) cor(actual_agri_mean, x))
#     coordinates(sampler2) <- ~x+y
#     proj4string(sampler2) <- CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
#     kml(sampler2, file = "agri_cor.kml",labels=sampler2$cor)
#     
#     # xport to kml all correlations 
# #     save(actual_agri_mean, file= 'actual_agri_mean.RData')
# #     save(EVI_smooth,file = 'EVI_Smooth_Dataframe.Rdata')
#     setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Modis Vege//')
#     load('EVI_Smooth_Dataframe.Rdata')
#     load('actual_agri_mean.RData')
#     cor = apply(EVI_smooth[,-c(1,2)], 1, function(x) cor(actual_agri_mean, x))
#     remove(EVI_smooth)
#     #EVI_smooth= EVI_smooth[,c('x','y','cor')]
#     #save(cor,file = 'EVI_Smooth_Dataframe_cor.Rdata')
#     #load('EVI_Smooth_Dataframe_cor.Rdata')
#     xy = EVI_smooth[,c('x','y')]
#     remove(EVI_smooth)
#     xycor = as.data.frame(cbind(xy,cor))
#     head(xycor)
#     coordinates(xycor) <- ~x+y
#     proj4string(xycor) <- CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
#     gridded(xycor) = TRUE
#     xycor = raster(xycor)
#     data(SAGA_pal)
#     kml(xycor, file = ".//kml//xycor.kml",colour_scale = SAGA_pal[[1]])
#  
# 
# #     # plot with DOY of phenology product
# #     # download phenology product and write to tif files
# #     library(RCurl)
# #     setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Modis Phenology\\')
# #     source('ModisDownload.R')
# #     modisProducts()
# #     
# #     MRT='C:\\Users\\mmann\\Documents\\MRTInstall\\bin'
# #     ModisDownload(x=37,h=c(21:22),v=c(7,8),dates=c('2010.01.01','2011.01.01','2012.01.01'),MRTpath=MRT, mosaic=T,proj=T,proj_type="UTM",utm_zone=27,datum="WGS84",pixel_size=500,resample_type="NEAREST_NEIGHBOR")
# #     
# #     hdfs = c('MCD12Q2.A2010001.h21v07.005.2012151155923.hdf','MCD12Q2.A2010001.h21v08.005.2012151155923.hdf','MCD12Q2.A2010001.h21v08.005.2012151155923.hdf','MCD12Q2.A2010001.h22v08.005.2012151155923.hdf')
# #     mosaicHDF(hdfNames =c('MCD12Q2.A2010001.h21v07.005.2012151155923.hdf','MCD12Q2.A2010001.h21v08.005.2012151155923.hdf','MCD12Q2.A2010001.h21v08.005.2012151155923.hdf','MCD12Q2.A2010001.h22v08.005.2012151155923.hdf'), filename = 'MCD12Q2.A2010001.005.2012151155923.hdf',MRTpath = MRT )
# # #test
# # ModisDownload(x=3,h=c(17,18),v=c(4,5),dates=c('2011.05.01','2011.05.31'),MRTpath=MRT, mosaic=T,proj=T,proj_type="UTM",utm_zone=30,datum="WGS84",pixel_size=1000)
# #   
# #     all=list.files(pattern='.hdf$')
#     #     set= all[grep(all,pattern='A2012001')]
#     #     mosaicHDF(hdfNames=set,filename='Mosaic.hdf',MRTpath=MRT,delete=F)
#   
# 
#  
#     
#     setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM//TRMM')
#     flist2 <- list.files(".",glob2rx('RS.25.9.DOY*.tif$'), full.names = TRUE)
#     DOY_stack = stack(flist2) 
#     # note: change gsub to extact only numbers
#     dates2 = strptime( gsub("^.*t.([0-9]+).*$", "\\1", names(DOY_stack)),format='%Y') # create dates to interpolate to
#     if(sum(is.na(dates2))==length(dates2)){print('WARNING: change gsub for dates2')}
#     names(DOY_stack)=dates2
#     # check projection, reproject if necesary 
#     if(projection(DOY_stack)!=projection(EVI_smooth_stack)){DOY_stack=projectRaster(DOY_stack,EVI_smooth_stack,method = 'ngb')}
#     # extract DOY of rainy season for same locations
#     DOY = as.data.frame(extract(DOY_stack,sample[,c('x','y')]))
#     names(DOY)=format(dates2,"%Y")
#     DOY = na.omit(cbind(DOY))
#     DOY = t(apply(DOY,1,function(x) paste(names(x),x,sep='')))
#     sample = sample[as.numeric(row.names(DOY)),]
#     
#     rownumber= 2
#     plot(as.numeric(pred_dates),sample[rownumber,-c(1,2)],xlab=pred_dates,main='TRMM')   #plot without xy
#     abline(v=as.numeric(strptime(DOY[rownumber,],'%Y%j')))
#     
#     #plot DOY 
#     rownumber= 6
#     plot(format(pred_dates,'%j'),sample[rownumber,-c(1,2)],xlab=pred_dates)   #plot without xy
#     abline(v=as.numeric(format(strptime(DOY[rownumber,],'%Y%j'),'%j')))


# Write Polygon data out to .dta file -------------------------------------
# this section of code is used to write the files for use by stata 
    library(foreign)
    setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//')
    kebele = read.dbf('Administrative Areas//Kebele_WLRC_UTM.dbf')
    # Recode Variables --------------------------------------------------------
    # find create factor of agro ecological zone type with highest proportion coverage
    kebele$AgEcoFactor = as.factor(names(kebele[,grep(colnames(kebele),pattern='AgEcoV')])[apply(kebele[,grep(colnames(kebele),pattern='AgEcoV')], 1, which.max)])
    # change names to match Eco zone name
    lookup = read.dbf('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//WLRC_Data_Original//Agroecology_UTM.dbf',as.is=T)
    lookup = unique(lookup[,c('GRIDCODE','Terminolog')])
    levels = data.frame(levels=as.numeric(sapply(levels(kebele$AgEcoFactor),function(x) substr(x,7,nchar(x)))))
    lookup2 = lookup[match(levels$levels,lookup$GRIDCODE),]
    levels(kebele$AgEcoFactor)=lookup2[,'Terminolog']
    
    # find create factor of soil type with highest proportion coverage
    kebele$SoilFactor = as.factor(names(kebele[,grep(colnames(kebele),pattern='SoilV')])[apply(kebele[,grep(colnames(kebele),pattern='SoilV')], 1, which.max)])
    # change names to match soil name
    lookup = read.dbf('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//WLRC_Data_Original//Soil_UTM.dbf',as.is=T)
    lookup = unique(lookup[,c('SOIL_CODE2','SOILTYPE')])
    levels = data.frame(levels=as.numeric(sapply(levels(kebele$SoilFactor),function(x) substr(x,6,nchar(x)))))
    lookup2 = lookup[match(levels$levels,lookup$SOIL_CODE2),]
    levels(kebele$SoilFactor)=lookup2[,'SOILTYPE']
    
    # find create factor of rainfall mm type with highest proportion coverage
    kebele$LYFactor <- as.factor(names(kebele[,grep(colnames(kebele),pattern='LYV')])[apply(kebele[,grep(colnames(kebele),pattern='LYV')], 1, which.max)])
    # change names to match long year
    lookup = read.dbf('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//WLRC_Data_Original//Long_Year_ARF_UTM.dbf',as.is=T)
    lookup = unique(lookup[,c('Zone_Id','Zone')])
    levels = data.frame(levels=as.numeric(sapply(levels(kebele$LYFactor),function(x) substr(x,4,nchar(x)))))
    lookup2 = lookup[match(levels$levels,lookup$Zone_Id),]
    levels(kebele$LYFactor)=lookup2[,'Zone']
    
    # change missing values for data created by GME
    kebele[kebele==min(kebele$OC4MN)|kebele==-2147483648|kebele==-21474836]=NA

    write.dta(kebele,"G://Faculty//Mann//Projects//Ethiopia Project//Regression Data\\CSA-AgSS\\polygondata_evi.dta")



# setwd('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\TRMM\\')
# 
# name = 'RS.25.9.DOY.2013.tif'
# inner = raster(name)
# sample = raster('C:\\Users\\mmann\\Documents\\Ethiopia Project\\GIS Data\\Transportation//Transport Cost Surface//EucDist_Rcap_250m.tif')
# inner = projectRaster(from=inner,sample,method='ngb')
# plot(inner)
# writeRaster(inner,name, overwrite=T)


# # create unique id for all data
# pop = read.csv('C://Users//mmann//Documents//Ethiopia Project//Regression Data//Kebele_pop_2007.csv')
# pop = pop[pop$Town ==8,]
# pop = pop[pop$kebele !=0,]
# unique(pop$kebele)
# pop$zonecode = pop$Region*100+pop$zone
# pop$weredacode = pop$Region*10000+pop$zone*100+pop$wereda
# pop$RK_CODE8 = pop$Region*10000000+pop$zone*100000+pop$wereda*1000+pop$kebele
# length(unique(pop$RK_CODE8))               
# write.csv(pop,'C://Users//mmann//Documents//Ethiopia Project//Regression Data//Kebele_pop_2007.csv')


# kdata = read.dta('C://Users//mmann//Documents//Ethiopia Project//Regression Data//KEBELE_DATA_MARCH_10_V3.dta')
# kdata$RK_CODE8 = kdata$REG*10000000+kdata$ZONE*100000+kdata$DIST*1000+kdata$FA
# write.csv(kdata,'C://Users//mmann//Documents//Ethiopia Project//Regression Data//KEBELE_DATA_MARCH_10_V4.csv')







# # loading the source of function (the script file should be copied in the working directory):
# source('ModisDownload.R')
# 
# # or alternatively, you can use: #source('http://r-gis.net/ModisDownload/ModisDownload.R')
# library(raster)
# library(RCurl)
# modisProducts( )
# 
# 
# MRT='C:\\Users\\mmann\\Documents\\MRTInstall'
# all=list.files(pattern='.hdf$')
# set= all[grep(all,pattern='A2012001')]
# 
# mosaicHDF(hdfNames=set,filename='Mosaic.hdf',MRTpath=MRT,delete=F)
# 
# 
# ModisDownload(x=35,h=c(21:22),v=c(7,8),dates=c('2012.05.01'),MRTpath=MRT, mosaic=T,proj=T,proj_type="UTM",utm_zone=27,datum="WGS84",pixel_size=500)
# 


