
library(foreign)
library(MASS)
library(sp)
library(rgdal)
library(ggplot2)
library(plyr)
library(maptools)
library(plm)




# Read in Data ------------------------------------------------------------
# shapefile

#proj = proj4string(readOGR("C://Users//mmann//Documents//Ethiopia Project//GIS Data//Administrative Areas","Kebele_WLRC_UTM"))
proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
pdata = readShapePoly("C://Users//mmann//Documents//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
# text files
kdata = read.dta('C://Users//mmann//Documents//Ethiopia Project//Regression Data//CSA-AgSS//AgSS2010-2012_Field_Information_Final_Panel_WHEAT_stata12.dta')
#pdata = read.dbf('C://Users//mmann//Documents//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM - ORIGINAL.dbf',as.is=T)
#hdata = read.csv('C://Users//mmann//Documents//Ethiopia Project//Regression Data//Kebele_pop_2007.csv',as.is=T)
pdata$sort = 1:dim(pdata)[1]
kpdata = merge.data.frame(pdata,kdata,by='RK_CODE',all.x=T,incomparables=NA) 

names(kpdata)
sum(!is.na(kpdata$AREAH))/3



# add road density data
roads = read.dbf('C://Users//mmann//Documents//Ethiopia Project//GIS Data//Transportation//OCHAroad//Eth_Road_Kebele_Intsct.dbf')
roads= roads[,c('RK_CODE','RoadLng_m')]
roads = aggregate(RoadLng_m~RK_CODE,data=roads,sum)  # collapse by Rkcode
kpdata = merge(kpdata,roads,by='RK_CODE',all.x=T,incomparables=NA)
kpdata$RoadLng_m[is.na(kpdata$RoadLng_m)]=0
kpdata$RoadDn_m_ha = kpdata$RoadLng_m/kpdata$hectares
summary(kpdata$RoadDn_m_ha)
kpdata = kpdata[order(kpdata$sort),] # resort data to match polygon perimeter data


# Recode Variables --------------------------------------------------------

# find create factor of agro ecological zone type with highest proportion coverage
kpdata$AgEcoFactor <- as.factor(names(kpdata[,grep(colnames(kpdata),pattern='AgEcoV')])[apply(kpdata[,grep(colnames(kpdata),pattern='AgEcoV')], 1, which.max)])
# change names to match Eco zone name
lookup = read.dbf('C://Users//mmann//Documents//Ethiopia Project//GIS Data//WLRC_Data_Original//Agroecology_UTM.dbf',as.is=T)
lookup = unique(lookup[,c('GRIDCODE','Terminolog')])
levels = data.frame(levels=as.numeric(sapply(levels(kpdata$AgEcoFactor),function(x) substr(x,7,nchar(x)))))
lookup2 = lookup[match(levels$levels,lookup$GRIDCODE),]
levels(kpdata$AgEcoFactor)=lookup2[,'Terminolog']

# find create factor of soil type with highest proportion coverage
kpdata$SoilFactor <- as.factor(names(kpdata[,grep(colnames(kpdata),pattern='SoilV')])[apply(kpdata[,grep(colnames(kpdata),pattern='SoilV')], 1, which.max)])
# change names to match soil name
lookup = read.dbf('C://Users//mmann//Documents//Ethiopia Project//GIS Data//WLRC_Data_Original//Soil_UTM.dbf',as.is=T)
lookup = unique(lookup[,c('SOIL_CODE2','SOILTYPE')])
levels = data.frame(levels=as.numeric(sapply(levels(kpdata$SoilFactor),function(x) substr(x,6,nchar(x)))))
lookup2 = lookup[match(levels$levels,lookup$SOIL_CODE2),]
levels(kpdata$SoilFactor)=lookup2[,'SOILTYPE']

# find create factor of rainfall mm type with highest proportion coverage
kpdata$LYFactor <- as.factor(names(kpdata[,grep(colnames(kpdata),pattern='LYV')])[apply(kpdata[,grep(colnames(kpdata),pattern='LYV')], 1, which.max)])
# change names to match long year
lookup = read.dbf('C://Users//mmann//Documents//Ethiopia Project//GIS Data//WLRC_Data_Original//Long_Year_ARF_UTM.dbf',as.is=T)
lookup = unique(lookup[,c('Zone_Id','Zone')])
levels = data.frame(levels=as.numeric(sapply(levels(kpdata$LYFactor),function(x) substr(x,4,nchar(x)))))
lookup2 = lookup[match(levels$levels,lookup$Zone_Id),]
levels(kpdata$LYFactor)=lookup2[,'Zone']

# change missing values for data created by GME
kpdata[kpdata==min(kpdata$OC4MN)|kpdata==-2147483648|kpdata==-21474836  ]=NA

for(name in names(kpdata)[grep('STD',names(kpdata))]){
  holder = kpdata[,paste(name)]
  holder[holder==-1]=NA   #replace all -1 STD values
  kpdata[,paste(name)]=holder
  print(summary(kpdata[,paste(name)]))
}





# Subset Data -------------------------------------------------------------

kpdata = kpdata[kpdata$R_CODE==1 |kpdata$R_CODE==3 |kpdata$R_CODE==4 |kpdata$R_CODE==7 ,]

kpdata$totwheatprodkebele= kpdata$wheat*kpdata$wheatAREA* kpdata$wheatOPH* kpdata$kweight



# Data Cleaning Rules -----------------------------------------------------

# remove 1 rediculous outlier
kpdata$wheatOPHrulea = kpdata$wheatOPH
kpdata$wheatOPHrulea[kpdata$wheatOPHrulea > 75]=NA

# another possibel method: follow WB methodology throw out top and bottom 1%, "Decomosition of gender differentials in agricultural productivity in ethiopia" World bank 


# Remove observations with less than 5 wheat farmers    NOT WORKING B/C FARMERS count
kpdata$wheatOPHruleb = kpdata$wheatOPH
#kpdata$wheatOPHruleb[kpdata$wheatOPHruleb > 75]=NA
kpdata$wheatOPHruleb[kpdata$wheat < 5]=NA

# Compare to other rules
hist(kpdata$wheatOPHruleb)
hist(kpdata$wheatOPHrulea)



# Change Names to Reflect Year --------------------------------------------

#names(kphdata)=paste(names(kphdata),'12',sep='_')


# Replace Polygon Data ----------------------------------------------------

# pdata@data = kphdata
# write.dbf(kphdata,'C://Users//mmann//Documents//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.dbf')


# Setup panel data  ------------------------------------------------------------


names(kpdata)

panel = pdata.frame(kpdata,c('RK_CODE','YEAR'))
panel=panel[!is.na(panel$wheatOPH),]

pdim(panel)

# # scale and center data
# write.csv(kpdata,'C://Users//mmann//Desktop//kpdata.csv')
# 
# for(col in 13:dim(kpdata)[2]){
#   if(is.numeric(kpdata[,col])==T ){kpdata[,col]=scale(kpdata[,col],center=T)}
# }
# NaNs create for empty columns fill with zeros
# remove NaN variables
# kpdata=kpdata[,-c(grep('NaN',kpdata[1,]))]


write.csv(panel,'C://Users//mmann//Desktop//kphdata2.csv')




# Regressions  ------------------------------------------------------------

test = plm(wheatOPHrulea ~ FlwMN+CWD_mnMN+CWD_sdMN+DRcapMN+D50kMN+OC4MN+wheatdamage_hect+wheatimpseed_hect+wheatchemfertt_hect+wheatirrg_hect ,data=panel ,model=c('pooling'))
summary(test)


test2 = plm(wheatOPH ~FlwMN+CWD_mnMN+CWD_sdMN+DRcapMN+D50kMN+OC4MN+wheatdamage_hect+wheatimpseed_hect+wheatchemfertt_hect+wheatirrg_hect+paste( 'SlpMN','I(SlpMN^2)','SlpSTD','I(SlpSTD^2)','factor(AgEcoFactor)','factor(SoilFactor)','AspMN','I(AspMN^2)','factor(LYFactor)','AREAH5','CWD_mnMN','I(CWD_mnMN^2)','CWD_sdMN','I(CWD_sdMN^2)','I(CWD_sdMN^3)','hectares',"LANDwheat5",'I(LANDwheat5^2)','I(LANDwheat5/wheat5)','I((LANDwheat5/wheat5)^2)','wheat5','I(wheat5^2)',"TTRCMN",'I(TTRCMN^2)',"TT50kMN",'I(TT50kMN^2)',"D50kMN",'I(D50kMN^2)',"PH4MN",'I(PH4MN^2)','CEC4MN','I(CEC4MN^2)','OC4MN','I(OC4MN^2)','I(DRcapMN^2)' ,'I(FlwMN^2)','I(SlpMIN^2)' ,sep='+')   ,data=kpdata,index=c('RK_CODE','YEAR'), model='pooling')
summary(test2)




names(kphdata)
f <- as.formula(paste('wheatOPHruleb ~', paste(paste(names(kphdata)[c(14:175)], collapse='+'),'SlpMN','I(SlpMN^2)','SlpSTD','I(SlpSTD^2)','factor(AgEcoFactor)','factor(SoilFactor)','AspMN','I(AspMN^2)','factor(LYFactor)','AREAH5','CWD_mnMN','I(CWD_mnMN^2)','CWD_sdMN','I(CWD_sdMN^2)','I(CWD_sdMN^3)','hectares',"LANDwheat5",'I(LANDwheat5^2)','I(LANDwheat5/wheat5)','I((LANDwheat5/wheat5)^2)','wheat5','I(wheat5^2)',"TTRCMN",'I(TTRCMN^2)',"TT50kMN",'I(TT50kMN^2)',"D50kMN",'I(D50kMN^2)',"PH4MN",'I(PH4MN^2)','CEC4MN','I(CEC4MN^2)','OC4MN','I(OC4MN^2)','I(DRcapMN^2)','factor(Z_CODE)','I(FlwMN^2)','I(SlpMIN^2)' ,sep='+')   ))

summary( lm(f, kphdata))
outaic = stepAIC(lm(f, kphdata,na.rm=T))
summary(outaic)
mean(outaic$residuals^2)

f <- as.formula(paste('wheatOPH5ruleb ~',paste('AspMN','I(AspMN^2)','SlpMN','I(SlpMN^2)','SlpSTD','AgEcoFactor','SoilFactor','LYFactor','hectares',sep='+'))   )
summary( lm(f, kphdata))

f <- as.formula(paste('log(totwheatprodkebele) ~', paste(colnames(kpdata)[c(16:35,77:103)], collapse='+')))
summary( lm(f, kphdata))
outaic = stepAIC(lm(f, kphdata))





# Neural Net --------------------------------------------------------------

library(mlbench)
library(nnet)
library(caret)

max = max(kphdata$wheatOPH5ruleb,na.rm=T)
f <- as.formula(paste('wheatOPH5ruleb/max ~', paste(paste(colnames(kphdata)[c(16:139)], collapse='+'),'SlpMN','I(SlpMN^2)','SlpSTD','I(SlpSTD^2)','factor(AgEcoFactor)','factor(SoilFactor)','AspMN','I(AspMN^2)','LYFactor','AREAH5','CWD_mnMN','I(CWD_mnMN^2)','CWD_sdMN','I(CWD_sdMN^2)','hectares',"LANDwheat5",'I(LANDwheat5/wheat5)','wheat5',"TTRCMN","TT50kMN",'I(TT50kMN^2)',"D50kMN","PH4MN",'I(PH4MN^2)','CEC4MN','I(CEC4MN^2)','OC4MN','I(OC4MN^2)','I(DRcapMN^2)' ,sep='+')   ))

nnet.fit <- nnet(form, data=kphdata, size=3) 
nnet.predict <- predict(nnet.fit)*max
mean((nnet.predict - kphdata$wheatOPH5ruleb[!is.na(kphdata$wheatOPH5ruleb)])^2) 

nnetfit <- train(form, data=kphdata, method="nnet", maxit=1000,  trace=F)  
nnet.predict2 <- predict(nnet.fit)*max
plot(kphdata$wheatOPH5ruleb[!is.na(kphdata$wheatOPH5ruleb)], nnet.predict2,
     main="Neural network predictions trained vs actual",xlab="Actual")
mean((nnet.predict2 - kphdata$wheatOPH5ruleb[!is.na(kphdata$wheatOPH5ruleb)])^2) 


# Regression Trees --------------------------------------------------------

library(tree)
f <- as.formula(paste('wheatOPH5ruleb~', paste(paste(colnames(kphdata)[c(16:139)], collapse='+'),'SlpMN','I(SlpMN^2)','SlpSTD','I(SlpSTD^2)','factor(AgEcoFactor)','factor(SoilFactor)','AspMN','I(AspMN^2)','LYFactor','AREAH5','CWD_mnMN','I(CWD_mnMN^2)','CWD_sdMN','I(CWD_sdMN^2)','hectares',"LANDwheat5",'I(LANDwheat5/wheat5)','wheat5',"TTRCMN","TT50kMN",'I(TT50kMN^2)',"D50kMN","PH4MN",'I(PH4MN^2)','CEC4MN','I(CEC4MN^2)','OC4MN','I(OC4MN^2)','I(DRcapMN^2)' ,sep='+')   ))
tree.fit = tree(f, data=kphdata) 
plot(tree.fit)
summary(tree.fit)
names(summary(tree.fit))
mean((summary(tree.fit)$residuals)^2)


library(party)
library(randomForest)
terms(f)
f2 <- as.formula(paste('wheatOPH5ruleb~', paste(paste(colnames(kphdata)[c(16:139)], collapse='+'),'SlpMN','I(SlpMN^2)','SlpSTD','I(SlpSTD^2)','AgEcoFactor','SoilFactor','AspMN','I(AspMN^2)','LYFactor','AREAH5','CWD_mnMN','I(CWD_mnMN^2)','CWD_sdMN','I(CWD_sdMN^2)','hectares',"LANDwheat5",'I(LANDwheat5/wheat5)','wheat5',"TTRCMN","TT50kMN",'I(TT50kMN^2)',"D50kMN","PH4MN",'I(PH4MN^2)','CEC4MN','I(CEC4MN^2)','OC4MN','I(OC4MN^2)','I(DRcapMN^2)' ,sep='+')   ))

RF=randomForest(f2,kphdata,na.action=na.omit,importance=T)
summary(RF)

# Plots  ------------------------------------------------------------------


kphdata$cut = as.factor(cut(kphdata$Both.Sexes/kphdata$hectares,50))
kphdata$areacut =as.factor(cut(kphdata$AREAH5,10))
ggplot(kphdata[kphdata$wheatOPH5<75,],aes(cut, wheatOPH5))+geom_boxplot()+xlab('Pop Density')+facet_wrap(~areacut)

ggplot(kphdata[kphdata$wheatOPH5<75,],aes(areacut, wheatOPH5))+geom_violi()+xlab('Farm size')


# Plot Polygons -----------------------------------------------------------

gpclibPermit()
pdata@data$id = rownames(pdata@data)

pdata.points = fortify(pdata, region="id")
pdata.df = join(pdata.points, pdata@data, by="id")   # join the data back in 

a=ggplot(pdata.df) + 
  aes(long,lat,group=group,fill=SoilFactor) + geom_polygon() +geom_path(color="white") +xlab('')+ylab('') 
b= a+ theme(axis.text  = element_blank(), axis.ticks =element_blank(),legend.position="none") #+coord_equal()
windows()
plot(a)



# Summary of Kebele vs Woreda Justify ---------------------------------------------

# count number of unique agro ecological zones by kebele and woreda level
subset = kphdata[,c('W_CODE','RK_CODE', "AgEcoV1","AgEcoV10","AgEcoV11" ,"AgEcoV12","AgEcoV13","AgEcoV14","AgEcoV15","AgEcoV2","AgEcoV3","AgEcoV4","AgEcoV5","AgEcoV6","AgEcoV7","AgEcoV8","AgEcoV9"   )]
# find # by turning proportions into true false 1,0
# only for areas with at least 5% coverage
subset[,3:dim(subset)[2]]=as.numeric(subset[,3:dim(subset)[2]]>0.05)   # replace with true false
# turn into categorical data
types = as.numeric(substr(names(subset[,3:dim(subset)[2]]),7,nchar(names(subset[,3:dim(subset)[2]]))))
for(i in 1:dim(subset)[1]){subset[,3:dim(subset)[2]][i,]=subset[,3:dim(subset)[2]][i,]*types}
# summarize the number of unique values for kebeles
subset$uniqueK=apply(subset[,grep('AgEco',names(subset))],1, function(x)length(unique(x)))
# summarize the number of unique values for woredas
# find the ageco values that exist in each woreda
woreda= aggregate( .~W_CODE,data=subset[,c(1,3:(dim(subset)[2]-1))],  FUN=max)
woreda$uniqueW=apply(woreda[,grep('AgEco',names(woreda))],1, function(x)length(unique(x)))
summary(woreda$uniqueW)
summary(subset$uniqueK)



