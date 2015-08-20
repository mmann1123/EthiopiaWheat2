library(rgdal)
library(maptools)
library(googleVis)
library(sp)
library(ggplot2)
library(plyr)
library(rgeos)
library(dismo)
library(plotKML )
library(spacetime )



# Other Plots -------------------------------------------------------------

# Plot out the Woreda FE gap, (% change due to membership in woreda i)
  inner = read.csv('C:\\Users\\mmann\\Dropbox\\Ethiopia Writeup\\FE outputs.csv')
  head(inner)
  qplot(X..Difference, data=inner, geom="histogram",xlab=('% Difference'), ylab=('Count'),binwidth = 15)
  sum(inner$X..Difference < 0) / sum(!is.na(inner$X..Difference))
  sum(inner$X..Difference < 0 & inner$X..Difference >-25) / sum(!is.na(inner$X..Difference)) # 82% growing below expected levels. 
 



#   (always run this, rerun after each kml output) ------------------------------------------------------------
  
    preds_wide = read.csv('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//Regression_Preds_wide5.csv',as.is=T)
    preds_long = read.csv('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//Regression_Preds_long5.csv',as.is=T)
    preds_long$YEAR[preds_long$YEAR==3] = 2010
    preds_long$YEAR[preds_long$YEAR==4] = 2011
    preds_long$YEAR[preds_long$YEAR==5] = 2012
    preds_wide = preds_wide[!is.na(preds_wide$Cen_X), ]   # drop missing XY values MUST DO THIS
    preds_long = preds_long[!is.na(preds_long$Cen_X), ]   # drop missing XY values
  
  # Assign initial projection and then reproject to lat lon
  
    coordinates(preds_wide)= ~ Cen_X+Cen_Y
    proj4string(preds_wide) <- CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
    preds_wide=spTransform(preds_wide,CRS("+proj=longlat"))
    
    head(preds_long)


  # Cross Section Plots -----------------------------------------------------
  
  
  # Write out a Google Earth KML file calling google charts interactively
  # set kml parameters
    kmlfile = paste(getwd(), "Plot.kml", sep="/")
    kmlname <- "Wheat Output per Hectare"
    kmldescription <- paste("Target range to Wheat Output per Hectare."  )
    icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"  # set which image to use as point on map
    name <- preds_wide$RK_NAME # add a label for each point
    # call google charts API and paste in EVI timeseries data 
    #for help go to https://developers.google.com/chart/image/docs/chart_playground
    in_data = preds_long[1:3,]
    chart =  gvisCandlestickChart(in_data, xvar="YEAR", low="rank_min",
                                  open="rank_bottom", close="rank_top",
                                  high="rank_max",
                                  options=list(legend='none',title=paste('Reg:',in_data$R_NAME[1],'Wor:',in_data$W_NAME[1],'Keb:',in_data$RK_NAME[1],sep=" "),vAxes="[{title:'Output Per Hectare'}]"))
    # store multiline html as text
    chart_text = paste( capture.output(cat(unlist(chart$html))),collapse='\r')
    description =  chart_text
    for(rk in unique(preds_long$RK_CODE)[-1]){
      in_data = preds_long[rk==preds_long$RK_CODE,]
      chart =  gvisCandlestickChart(in_data, xvar="YEAR", low="rank_min",
                                    open="rank_bottom", close="rank_top",
                                    high="rank_max",
                                    options=list(legend='none',title=paste('Reg:',in_data$R_NAME[1],'Wor:',in_data$W_NAME[1],'Keb:',in_data$RK_NAME[1],sep=" "),vAxes="[{title:'Output Per Hectare'}]"))
      chart_text = paste( capture.output(cat(unlist(chart$html))),collapse='\r')
      description = c(description,chart_text) 
    }
   
    # write kml file will parameters as set above
    kmlPoints(preds_wide, kmlfile=kmlfile, name=name, description=description,
              icon=icon, kmlname=kmlname, kmldescription=kmldescription)
  
  
   
  # Zone FE Data Plots 2  (USED IN PAPER WORKING CORRECTLY )--------------------------------------------------------
  
  # PROBLEM!!! ??? seems like not a problem
  #paste('[','\"',in_data$YEAR[1],'\"',',',round(exp(in_data$lb_yhatOPH_pn[1]),3) ,',', diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )),',', paste('"','Values',round(exp(in_data$pred_lnwheatOPH_pn[1]),2),round(exp(in_data$lb_yhatOPH_pn[1]),2),round(exp(in_data$ub_yhatOPH_pn[1]),2),'"', sep=' : ')  ,',',-1*diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )),',',  2*diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )) ,',',round(exp(in_data$pred_lnwheatOPH_pn[1]),2),'],'),
  # round(exp(in_data$lb_yhatOPH_pn[1]),3)  shouldn't point to mean, it should point to bottom of 95% confidence bound
  
  #create variables for descriptiosn
  # variables used for weird google charts inputs 
  preds_long$exp_lb_yhatOPH_ols =  exp(preds_long$lb_yhatOPH_ols)
  preds_long$rng_lb_yhatOPH_ols = unlist(lapply(1:dim(preds_long)[1],function(x) diff(range(exp(preds_long$lb_yhatOPH_ols[x]),exp(preds_long$ub_yhatOPH_ols[x])))  ))
  # break out estimates and actual data to display differently 
  # replace actual OPH values where possible
  preds_long$actual_data='Estimate'
  preds_long$actual_data[!is.na(preds_long$lnwheatOPH)]='Actual'
  preds_long$pred_lnwheatOPH_ols[!is.na(preds_long$lnwheatOPH)]=preds_long$lnwheatOPH[!is.na(preds_long$lnwheatOPH)]
  preds_long$actual_data_color = '#43459d'
  preds_long$actual_data_color[preds_long$actual_data=='Estimate'] = '#f1ca3a'
  
  
  # create html code
  # iterate through actual and estimated create seperate kmls
  for(datatype in c('Actual','Estimate')){
    html_desc_holder = c()  # holder for google chart html code
    RK_CODE_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points
    RK_NAME_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points
    
      for(RK_CODES in unique(preds_long$RK_CODE[preds_long$actual_data == datatype]) ){
          
          in_data = preds_long[preds_long$RK_CODE==RK_CODES,]   # store data for one RK_CODE
          
          if(is.na(in_data$lb_yhatOPH_pn[1])){next}  # avoid missing values 
          RK_CODE_STORE =c(RK_CODE_STORE,RK_CODES)   # if not skipped store RK_CODE for creation of XY points
          RK_NAME_STORE =c(RK_NAME_STORE,in_data$RK_NAME[!is.na(in_data$RK_NAME)][1])   # if not skipped store RK_NAME for creation of XY points
          
          html_desc=c('<html>',
                      '<head>',
                      '<script type="text/javascript" src="https://www.google.com/jsapi"></script>',
                      '<script type="text/javascript">',
                      'google.load("visualization", "1", {packages:["corechart"]});',
                      'google.setOnLoadCallback(drawVisualization);',
                      
                      'function drawVisualization() {',
                      '// Create and populate the data table.',
                      'var data = new google.visualization.DataTable();',
                      'data.addColumn("string", "Day");',
                      'data.addColumn("number", "Estimate");',
                      'data.addColumn("number", "Range");',
                      'data.addColumn({type: "string", role: "tooltip"});',              
                      'data.addColumn({type: "number", role: "interval"});',
                      'data.addColumn({type: "number", role: "interval"});',
                      paste('data.addColumn(\'number\',\'',in_data$actual_data[1], '\');',sep=""),
                      'data.addRows([', 
                      paste('[','\"',in_data$YEAR[1],'\"',',',round(exp(in_data$lb_yhatOPH_ols[1]),3) ,',', diff(range(exp(in_data$lb_yhatOPH_ols[1]),exp(in_data$ub_yhatOPH_ols[1]) )),',', paste('"','Values',round(exp(in_data$pred_lnwheatOPH_ols[1]),2),round(exp(in_data$lb_yhatOPH_ols[1]),2),round(exp(in_data$ub_yhatOPH_ols[1]),2),'"', sep=' : ')  ,',',-1*diff(range(exp(in_data$lb_yhatOPH_ols[1]),exp(in_data$ub_yhatOPH_ols[1]) )),',',  2*diff(range(exp(in_data$lb_yhatOPH_ols[1]),exp(in_data$ub_yhatOPH_ols[1]) )) ,',',round(exp(in_data$pred_lnwheatOPH_ols[1]),2),'],'),
                      paste('[','\"',in_data$YEAR[2],'\"',',',round(exp(in_data$lb_yhatOPH_ols[2]),3) ,',', diff(range(exp(in_data$lb_yhatOPH_ols[2]),exp(in_data$ub_yhatOPH_ols[2]) )),',', paste('"','Values',round(exp(in_data$pred_lnwheatOPH_ols[2]),2),round(exp(in_data$lb_yhatOPH_ols[2]),2),round(exp(in_data$ub_yhatOPH_ols[2]),2),'"', sep=' : ')  ,',', -1*diff(range(exp(in_data$lb_yhatOPH_ols[2]),exp(in_data$ub_yhatOPH_ols[2]) )) ,',', 2*diff(range(exp(in_data$lb_yhatOPH_ols[2]),exp(in_data$ub_yhatOPH_ols[2]) )) ,',',round(exp(in_data$pred_lnwheatOPH_ols[2]),2),'],'),
                      paste('[','\"',in_data$YEAR[3],'\"',',',round(exp(in_data$lb_yhatOPH_ols[3]),3) ,',', diff(range(exp(in_data$lb_yhatOPH_ols[3]),exp(in_data$ub_yhatOPH_ols[3]) )),',', paste('"','Values',round(exp(in_data$pred_lnwheatOPH_ols[3]),2),round(exp(in_data$lb_yhatOPH_ols[3]),2),round(exp(in_data$ub_yhatOPH_ols[3]),2),'"', sep=' : ')  ,',', -1*diff(range(exp(in_data$lb_yhatOPH_ols[3]),exp(in_data$ub_yhatOPH_ols[3]) )) ,',', 2*diff(range(exp(in_data$lb_yhatOPH_ols[3]),exp(in_data$ub_yhatOPH_ols[3]) )) ,',',round(exp(in_data$pred_lnwheatOPH_ols[3]),2),'],'),
                      ' ]);',
                      
                      '// Create and draw the visualization.',
                      'var options = {',
                      'tooltip: {isHtml: true},',
                      'legend: "none"',
                      '};',
                      
                      'var ac = new google.visualization.ComboChart(document.getElementById("visualization"));',
                      'ac.draw(data, {',
                      'title : "Estimated and target wheat yields",',
                      'width: 600,',
                      'height: 400,',
                      'vAxis: {title: "Wheat Yield"},',
                      'hAxis: {title: "Year"},',
                      'isStacked: true,',
                      'seriesType: "bars",',
                      paste('series: {0: {color: "transparent"}, 2: {type: "line",color: "', in_data$actual_data_color[1] ,'"}   }',sep=""),
                      ' },options);',
                      '}',
                      '</script>',
                      '</head>',
                      '<body>',
                      '<div id="visualization" style="width: 600px; height: 500px;"></div>',
                     # round(exp(in_data$pred_lnwheatOPH_ols[1]),2),
                      
                      '</body>',
                      '</html>')
          
          html_desc_holder =c(html_desc_holder,paste(html_desc,collapse='\r'))
        }  
        
        
        # Assign initial projection and then reproject to lat lon
        preds_wide2 = read.csv('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//Regression_Preds_wide.csv',as.is=T)
        # limit sample to correct RK_CODES
        preds_wide2 = preds_wide2[ preds_wide2$RK_CODE %in% RK_CODE_STORE,]
        # get points into same order as html  
        library(plyr)
        preds_wide2 = join(data.frame(RK_CODE=unlist(RK_CODE_STORE)),preds_wide2)
        preds_wide2 = preds_wide2[!is.na(preds_wide2$Cen_X), ]   # drop missing XY values MUST DO THIS
        
        # find unique locations 
        coordinates(preds_wide2) = ~ Cen_X+Cen_Y
        proj4string(preds_wide2) = CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
        preds_wide2=spTransform(preds_wide2,CRS("+proj=longlat"))
        
        # WRite KML
        kmlname <- paste(datatype,"Wheat Output per Hectare",sep=' ')
        kmldescription <- paste("Target range to Wheat Output per Hectare."  )
        icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"  # set which image to use as point on map
        #icon = rep("http://maps.google.com/mapfiles/kml/pal4/icon57.png",times = length(preds_long$actual_data))
        #icon[preds_long$actual_data=='Actual']='http://maps.google.com/mapfiles/kml/pal4/icon63.png'
        preds_wide2$RK_NAME = sub('&',replacement = 'and',x = preds_wide2$RK_NAME) # avoid issue with &s 
        name <- preds_wide2$RK_NAME    # add a label for each point
        kmlfile = paste('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//', paste("ComboPlot3FE_",datatype,".kml",sep=''), sep="/")
        
        kmlPoints(preds_wide2, kmlfile=kmlfile, name=name, description= html_desc_holder,
                  icon=icon, kmlname=kmlname, kmldescription=kmldescription)
      }
      
  
   
  
# 90th% Cluster Plots 3 Distribution based on Cluster (used in paper) --------------------------------------------------------
  # compare mean Actual  OPH by cluster 
  
    #xvar="YEAR", low="rank_min",
    #open="rank_bottom", close="rank_top",
    #high="rank_max",
    #             btm  up  downer upper  line
    #    '["Mon", 28,  10,    -28,    32,  42.8],',
    #        x  , btm 95, dif to upper 95, neg diff 99 from btm 95, add to get to upper 99

    # read in polygon data & add area as attribute
    proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
    pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
    pdata2 = pdata[pdata@data$RK_CODE %in% preds_wide@data$RK_CODE, ]
    pdata2@data$areas = sapply(slot(pdata2, "polygons"), slot, "area")
    pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
    
    # create cluster statistics
    # summarize by cluster and year 
    preds_long$ClusterNumEnvYear = paste(preds_long$ClusterNumEnv,preds_long$YEAR,sep='')
    ClusterMeans = aggregate(exp(preds_long$lnwheatOPH ),by=list(preds_long$ClusterNumEnvYear),function(x) mean(x,na.rm=T))
    names(ClusterMeans)=c('ClusterNumEnvYear','ClusterMean')
    # deal with one missing value
    ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12012','ClusterMean'] = mean(ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12010','ClusterMean'],ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12011','ClusterMean'])
    ClusterSD = aggregate(exp(preds_long$lnwheatOPH),by=list(preds_long$ClusterNumEnvYear),function(x) sd(x,na.rm=T))
    names(ClusterSD)=c('ClusterNumEnvYear','ClusterSD')
    # AFAR DOESN'T MATTER   
    ClusterSD[ClusterSD$ClusterNumEnvYear=='12010','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52010','ClusterSD'])
    ClusterSD[ClusterSD$ClusterNumEnvYear=='12011','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52011','ClusterSD'])
    ClusterSD[ClusterSD$ClusterNumEnvYear=='12012','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52012','ClusterSD'])

    # create 90 percentile estimates for each woreda based on cluster membership
    Cluster90th = aggregate(exp(preds_long$lnwheatOPH ),by=list(preds_long$ClusterNumEnvYear),function(x) quantile(x,.9,na.rm=T))
    names(Cluster90th)=c('ClusterNumEnvYear','Cluster90th')
    # join stats back in 
    library(plyr)
    preds_long = join(preds_long,ClusterMeans)
    preds_long = join(preds_long,ClusterSD)
    preds_long = join(preds_long,Cluster90th)

    # create tables of mean sd by woreda for paper
    table_data_l = preds_long[,c("W_NAME",'R_NAME','YEAR',"ClusterMean","ClusterSD")] 
    library(reshape2)
    table_data_w = reshape(table_data_l,timevar = "YEAR", idvar = c('R_NAME','W_NAME' ), direction = "wide")[,c(1:8)] 
    head(table_data_w,10)    
    # limit to the 4 major regions
    table_data_w=table_data_w[table_data_w$R_NAME %in% c('Amahara','SNNP','Tigray','Oromiya') ,]
    write.csv(table_data_w,'G://Faculty//Mann//Projects//Ethiopia Project//Write Up//ClusterMnSD_Woreda10-12_5.csv')    
    table_data_w$R_NAME = as.factor(table_data_w$R_NAME)

    table_data_r = aggregate(cbind(ClusterMean.2010,ClusterSD.2010, ClusterMean.2011,ClusterSD.2011, ClusterMean.2012 ,ClusterSD.2012)~R_NAME, mean,data=table_data_w)
    write.csv(table_data_r,'G://Faculty//Mann//Projects//Ethiopia Project//Write Up//ClusterMnSD_Region10-12_5.csv')    
    
    # Create area weighted average in order to create woreda level estimates
    # calculate area weights (smaller, higher population woredas given more weight)
    preds_long = join(preds_long,pdata2@data[,c('RK_CODE','areas','W_CODE' )] )
    Woredaarea = aggregate(preds_long$areas,by=list(preds_long$W_CODE),function(x) sum(x,na.rm=T))
    Woredaarea = data.frame((Woredaarea))
    names(Woredaarea)=c('W_CODE','Woredaarea')
    preds_long = join(preds_long,Woredaarea)
    preds_long$WoredaParea = preds_long$areas / preds_long$Woredaarea
    # currently doing area weighted not inverse area weighting (kebeles) are uniformly small relative to woreda)
    # weight yeilds by area ( not specific to year )
    W_pred_lnwheatOPH_ols =sapply(split(preds_long,preds_long$W_CODE), function(x) weighted.mean(x$pred_lnwheatOPH_ols,x$WoredaParea,na.rm =T))
    w_preds = data.frame(preds=W_pred_lnwheatOPH_ols, W_CODE=names(W_pred_lnwheatOPH_ols))
    W_90th =sapply(split(preds_long,preds_long$W_CODE), function(x) weighted.mean(x$Cluster90th,x$WoredaParea,na.rm =T))
    w_90th= data.frame(target=W_90th, W_CODE=names(W_pred_lnwheatOPH_ols))
    w_preds = join(w_preds, w_90th)
    w_preds$gap=exp(w_preds$preds)/w_preds$target
    w_preds$gap2 =(1-w_preds$gap) *-100
    w_preds$exprpeds=exp(w_preds$preds) 
    w_preds = join(w_preds, unique(preds_long[,c('W_CODE','R_NAME')]),type = 'left')

    # write out table of gaps by region
    table_data_g = aggregate(cbind( exprpeds,target,gap2)~R_NAME, median,data=w_preds)
    table_data_g = table_data_g[table_data_g$R_NAME %in% c('Amahara','SNNP','Tigray','Oromiya') ,]
    table_data_gmx = aggregate(cbind( exprpeds,target,gap2)~R_NAME, max,data=w_preds)
    names(table_data_gmx) =c("R_NAME","exprpedsmax" ,"targetmax" ,  "gapmax" )
    table_data_gmin = aggregate(cbind( exprpeds,target,gap2)~R_NAME, min,data=w_preds)
    names(table_data_gmin) =c("R_NAME","exprpedsmin" ,"targetmin" ,  "gapmin" )
    table_data_g = join(table_data_g,table_data_gmx)
    table_data_g = join(table_data_g,table_data_gmin)

    write.csv(table_data_g,'G://Faculty//Mann//Projects//Ethiopia Project//Write Up//Gaps_Region10-12_5.csv')    

        #join into polygons
    proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
    pwdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Woreda_WLRC_UTM.shp", proj4string=CRS(proj))
    pwdata@data = join(pwdata@data,w_preds) 
    pwdata@data$id = rownames(pwdata@data)
    #poly.points = fortify(pwdata, region="id")
    source('G://Faculty//Mann//Scripts//fortify2.R')
    poly.points = fortify2(pwdata, region="id")
    
    # avoid memory issues??
    save(w_preds,pwdata,poly.points,file='G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplotcluster5.RData' ,version = NULL)
    rm(list = ls())
    load('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplotcluster5.RData' )
    
  # join in wide data 
  poly.df = join(poly.points, pwdata@data, by="id",type = 'left')
  poly.df$W_CODE=factor(poly.df$W_CODE)
  poly.df = join(poly.df,w_preds ,by = 'W_CODE',type = 'left')
  head(poly.df)
  
  poly.df = poly.df[,-c(10:12)]
  poly.df$gap2 = (1 - poly.df$gap)*-100 # poly.df$gap*-1*100
  
  # where w_preds$gap=exp(w_preds$preds)/w_preds$target
  
  #import admin boundaries
  ogrInfo("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas", "Country_WLRC_UTM_Dissolve")
  boundary = readOGR("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas", "Country_WLRC_UTM_Dissolve_Mu")
  plot(boundary)
  boundary@data$id = rownames(boundary@data)
  source('G://Faculty//Mann//Scripts//fortify2.R')
  boundary.df =  fortify2(boundary) 
  boundary.df =  join(boundary.df, boundary@data, by="id",type = 'left')

  # data for region labels
  state = readOGR("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas", "ETH_adm2_UTM")
  centroids = as.data.frame(coordinates(gCentroid(state,byid=TRUE)))
  centroids$label = as.character(state$NAME_1)
  centroids=aggregate(cbind(x,y)~label,data=centroids,FUN=mean)  # find center of all polys
  centroids = centroids[centroids$label %in% c('Addis Ababa','Amhara','Southern Nations, Nationalities and Peoples','Tigray','Oromia') ,]
  centroids[centroids$label %in% c('Southern Nations, Nationalities and Peoples') ,'label']='Southern Nations'

windows() # (USED IN PAPER)
sub = poly.df[poly.df$R_NAME.1 %in% c('Amahara','SNNP','Tigray','Oromiya') ,]
ggplot() +  geom_polygon(data=sub,aes(long,lat,group=as.factor(group),fill= gap2 )) +
  coord_equal() +
  scale_fill_gradient2('% Yield Gap', low = 'red',mid='purple',high = 'green', midpoint=-60) +
  geom_polygon(data=boundary.df[boundary.df$hole==F,] ,aes(long,lat, group=as.factor(id), alpha=1),colour="grey70",size=.75, fill=NA,, show_guide = F)+
  coord_cartesian(xlim=c(-55827.0, 951569.1),ylim=c(357231.7,1659987.2))+
  with(centroids, annotate(geom="text", x = x, y=y, label = label, size = 4,colour='white',fontface='bold')) 

windows() # (USED IN PAPER)
qplot(gap2, data=poly.df[poly.df$R_NAME.1 %in% c('Amahara','SNNP','Tigray','Oromiya') ,], geom="histogram")+xlab('% Difference')



windows() # (USED IN PAPER)
ggplot(poly.df[poly.df$R_NAME.1 %in% c('Amahara','SNNP','Tigray','Oromiya') ,]) + 
  aes(long,lat,group=as.factor(group),fill= exp(preds) ) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2('OPH', low = 'purple',mid='green',high = 'darkgreen', midpoint=15)


windows() # (USED IN update)
ggplot() + geom_polygon(data=sub,aes(long,lat,group=as.factor(group),fill= exp(preds) )) +
  coord_equal() +  coord_cartesian(xlim=c(-55827.0, 951569.1),ylim=c(357231.7,1659987.2))+
  scale_fill_gradient2('OPH', low = 'grey20',mid='purple',high = 'green', midpoint=12)+
  geom_polygon(data=boundary.df[boundary.df$hole==F,] ,aes(long,lat, group=as.factor(id), alpha=1),colour="grey70",size=.75, fill=NA,, show_guide = F)+
  with(centroids, annotate(geom="text", x = x, y=y, label = label, size = 4,colour='white',fontface='bold')) 
 

windows() # (USED IN PAPER)
qplot(exp(preds), data=poly.df, geom="histogram")+xlab('% Difference')


  

windows()   
ggplot(poly.df) + 
  aes(long,lat,group=as.factor(group),fill= gap2 ) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2('% Yield Gap', low = 'gray15',mid='gray43',high = 'gray70', midpoint=-60)



windows() # (USED IN revision) limit to four major growing regions
ggplot(poly.df[poly.df$R_NAME.1 %in% c('Amahara','SNNP','Tigray','Oromiya') ,]) + 
  aes(long,lat,group=as.factor(group),fill= gap2 ) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2('% Yield Gap', low = 'red',mid='purple',high = 'green', midpoint=-60)

windows()  # (USED IN revision)
ggplot(poly.df[poly.df$R_NAME.1 %in% c('Amahara','SNNP','Tigray','Oromiya') ,]) + 
  aes(long,lat,group=as.factor(group),fill= gap2 ) + 
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2('% Yield Gap', low = 'gray15',mid='gray43',high = 'gray70', midpoint=-60)



  windows()  
  ggplot(poly.df) + 
    aes(long,lat,group=as.factor(group),fill= gap2 ) + 
    geom_polygon() +
    coord_equal() +
    scale_fill_gradient2('% Yield Gap', low = 'red',mid='darkgrey',high = 'green', midpoint=-50)
  
    # KML 
    #create variables for descriptiosn
    # variables used for weird google charts inputs 
    preds_long$exp_yhatOPH_pn      =  round(exp(preds_long$ClusterMean),3)   
    preds_long$btm_95lb_yhatOPH_pn =  round(preds_long$ClusterMean - preds_long$ClusterSD,3)
    preds_long$rng_95lb_yhatOPH_pn =  round(2* preds_long$ClusterSD,3)
    preds_long$btmhhead(ead_99lb_yhatOPH_pn = preds_long$rng_95lb_yhatOPH_pn - round(3* preds_long$ClusterSD ,3)
    preds_long$tps_99lb_yhatOPH_pn = abs(preds_long$btm_99lb_yhatOPH_pn) +round(3* preds_long$ClusterSD,3)
    # create tooltip data
    preds_long$exp_yhatOPH_pn =  round(exp(preds_long$ClusterMean),3)
    preds_long$exp_yhatOPH_pn_95btm =  round(exp(preds_long$ClusterMean),3) -round(2* preds_long$ClusterSD,3)
    preds_long$exp_yhatOPH_pn_95top =  round(exp(preds_long$ClusterMean),3) +round(2* preds_long$ClusterSD,3)
    
    
    # break out estimates and actual data to display differently 
    # replace actual OPH values where possible
    preds_long$actual_data='Estimate'
    preds_long$actual_data[!is.na(preds_long$lnwheatOPH)]='Actual'
    preds_long$pred_lnwheatOPH_ols[!is.na(preds_long$lnwheatOPH)]=preds_long$lnwheatOPH[!is.na(preds_long$lnwheatOPH)]
    preds_long$actual_data_color = '#43459d'
    preds_long$actual_data_color[preds_long$actual_data=='Estimate'] = '#f1ca3a'
    

    #     

    
    # create html code for KML of FE estimate vs cluster distribution
  for(datatype in c('Actual','Estimate')[1]){
    
      html_desc_holder = c()  # holder for google chart html code
      RK_CODE_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points
      RK_NAME_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points

      for(RK_CODES in unique(preds_long$RK_CODE[preds_long$actual_data == datatype]) ){
        
          in_data = preds_long[preds_long$RK_CODE==RK_CODES,]   # store data for one RK_CODE
          
          if(is.na(in_data$lb_yhatOPH_pn[1])){next}  # avoid missing values 
          RK_CODE_STORE =c(RK_CODE_STORE,RK_CODES)   # if not skipped store RK_CODE for creation of XY points
          RK_NAME_STORE =c(RK_NAME_STORE,in_data$RK_NAME[!is.na(in_data$RK_NAME)][1])   # if not skipped store RK_NAME for creation of XY points
          
          html_desc=c('<html>',
                      '<head>',
                      '<script type="text/javascript" src="https://www.google.com/jsapi"></script>',
                      '<script type="text/javascript">',
                      'google.load("visualization", "1", {packages:["corechart"]});',
                      'google.setOnLoadCallback(drawVisualization);',
                      
                      'function drawVisualization() {',
                      '// Create and populate the data table.',
                      'var data = new google.visualization.DataTable();',
                      'data.addColumn("string", "Day");',
                      'data.addColumn("number", "Estimate");',
                      'data.addColumn("number", "Range");',
                      'data.addColumn({type: "string", role: "tooltip"});',              
                      'data.addColumn({type: "number", role: "interval"});',
                      'data.addColumn({type: "number", role: "interval"});',
                      paste('data.addColumn(\'number\',\'',in_data$actual_data[1], '\');',sep=""),
                      'data.addRows([', 
                      
                      paste('[','\"',in_data$YEAR[1],'\"',',', in_data$btm_95lb_yhatOPH_pn[1] ,',', in_data$rng_95lb_yhatOPH_pn[1],',', paste('"','Values',in_data$exp_yhatOPH_pn[1],in_data$exp_yhatOPH_pn_95btm[1],in_data$exp_yhatOPH_pn_95top[1],'"', sep=' : ')  ,',',in_data$btm_99lb_yhatOPH_pn[1],',',  in_data$tps_99lb_yhatOPH_pn[1] ,',',round(exp(in_data$pred_lnwheatOPH_ols[1]),2),'],'),
                      paste('[','\"',in_data$YEAR[2],'\"',',', in_data$btm_95lb_yhatOPH_pn[2] ,',', in_data$rng_95lb_yhatOPH_pn[2],',', paste('"','Values',in_data$exp_yhatOPH_pn[2],in_data$exp_yhatOPH_pn_95btm[2],in_data$exp_yhatOPH_pn_95top[2],'"', sep=' : ')  ,',',in_data$btm_99lb_yhatOPH_pn[2],',',  in_data$tps_99lb_yhatOPH_pn[2] ,',',round(exp(in_data$pred_lnwheatOPH_ols[2]),2),'],'),
                      paste('[','\"',in_data$YEAR[3],'\"',',', in_data$btm_95lb_yhatOPH_pn[3] ,',', in_data$rng_95lb_yhatOPH_pn[3],',', paste('"','Values',in_data$exp_yhatOPH_pn[3],in_data$exp_yhatOPH_pn_95btm[3],in_data$exp_yhatOPH_pn_95top[3],'"', sep=' : ')  ,',',in_data$btm_99lb_yhatOPH_pn[3],',',  in_data$tps_99lb_yhatOPH_pn[3] ,',',round(exp(in_data$pred_lnwheatOPH_ols[3]),2),'],'),
                      ' ]);',
                      
                      '// Create and draw the visualization.',
                      'var options = {',
                      'tooltip: {isHtml: true},',
                      'legend: "none"',
                      '};',
                      
                      'var ac = new google.visualization.ComboChart(document.getElementById("visualization"));',
                      'ac.draw(data, {',
                      'title : "Estimated and target wheat yields",',
                      'width: 600,',
                      'height: 400,',
                      'vAxis: {title: "Wheat Yield"},',
                      'hAxis: {title: "Year"},',
                      'isStacked: true,',
                      'seriesType: "bars",',
                      paste('series: {0: {color: "transparent"}, 2: {type: "line",color: "', in_data$actual_data_color[1] ,'"}   }',sep=""),
                      ' },options);',
                      '}',
                      '</script>',
                      '</head>',
                      '<body>',
                      '<div id="visualization" style="width: 600px; height: 500px;"></div>',
                     # paste(in_data$pred_lnwheatOPH_pn),
                      '</body>',
                      '</html>')
          
          html_desc_holder =c(html_desc_holder,paste(html_desc,collapse='\r'))
      }  
      
      
      # Assign initial projection and then reproject to lat lon
      preds_wide2 = read.csv('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//Regression_Preds_wide.csv',as.is=T)
      # limit sample to correct RK_CODES
      preds_wide2 = preds_wide2[ preds_wide2$RK_CODE %in% RK_CODE_STORE,]
      # get points into same order as html  
      library(plyr)
      preds_wide2 = join(data.frame(RK_CODE=unlist(RK_CODE_STORE)),preds_wide2)
      preds_wide2 = preds_wide2[!is.na(preds_wide2$Cen_X), ]   # drop missing XY values MUST DO THIS
      preds_wide2 = preds_wide2[!is.na(preds_wide2$Cen_Y), ]   # drop missing XY values MUST DO THIS
      
      # find unique locations 
      coordinates(preds_wide2) = ~ Cen_X+Cen_Y
      proj4string(preds_wide2) = CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
      preds_wide2=spTransform(preds_wide2,CRS("+proj=longlat"))
      
      # WRite KML
      kmlname <- paste(datatype ,"Cluster Gaps Wheat Output per Hectare",sep=' ')
      kmldescription <- paste("Target range to Wheat Output per Hectare."  )
      icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"  # set which image to use as point on map
      #icon = rep("http://maps.google.com/mapfiles/kml/pal4/icon57.png",times = length(preds_long$actual_data))
      #icon[preds_long$actual_data=='Actual']='http://maps.google.com/mapfiles/kml/pal4/icon63.png'
      preds_wide2$RK_NAME = sub('&',replacement = 'and',x = preds_wide2$RK_NAME) # avoid issue with &s 
      name <- preds_wide2$RK_NAME    # add a label for each point
      kmlfile = paste('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//', paste("ComboPlot3_cluster8",datatype,".kml",sep=''), sep="/")
      
      kmlPoints(preds_wide2, kmlfile=kmlfile, name=name, description= html_desc_holder,
                icon=icon, kmlname=kmlname, kmldescription=kmldescription)
   }
  


  
  # Polygons for Gap Map ----------------------------------------------------
  
  ## Export polygons
  library(sp)
   
  kmlfile2 = paste('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//', "ComboPlotPoly.kml", sep="/")
  proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
  pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
  pdata2 = pdata[pdata@data$RK_CODE %in% preds_wide@data$RK_CODE, ]
  pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
  
  getEdges <- function(x) {
    lapply(x@polygons, function(y) {
      Polygon(y@Polygons[[1]]@coords, hole=y@Polygons[[1]]@hole )
    })
  }
  polys = Polygons(getEdges(pdata2),'OPH')
  
  kmlPolygon( polys, kmlfile=kmlfile2, name="Ethiopia OPH", col="#df0000aa", lwd=5, 
             border=4, description=NULL)
  
    
  
  # Polygons With Descriptions? ---------------------------------------------
  
  
  
  #  This function extends the kmlPolygon() function in package maptools to accept a
  # SpatialPolygonsDataFrame object, and allow labels, descriptions, and polygon
  # fill colors to be based on attributes in the dataframe of the SpatialPolygonsDataFrame object
  #  version 0.5  12 August 2011 Tom Philippi  tom_philippi@nps.gov
  #
  #  parameters: (all passed as character values of the names of the objects
  #      PGdv:                    the character value of the name of the SpatialPolygonsDataFrame object
  #      kmlFileName:         the name of the output *.kml file
  #      Pname:                    the name of the variable in PGdv's dataframe with values for names of the individual polygons
  #      Pdescription:            the name of the variable in PGdv's dataframe with values for descriptions of the individual polygons
  #      Kname:                     a character string name for the kml object in GoogleEarth (one value per kml file)
  #      Kdescription:            a character string description for the kml object in GoogleEarth (one value per kml file)
  #      ColorTheme:            the name of the (numeric or factor) variable in PGdv's dataframe with integer values or
  #                                              factor numbers mapped to colors by ColorMap
  #      ColorMap:                 a vector of character strings in #RRGGBBAA form (AA=alpha 00=transparent FF=solid)
  #                                              this vector must be as long as the number of levels in ColorTheme, or the maximum
  #                                              value if ColorTheme is numeric integers
  
  
  kmlPolygons <- function(PGdv,kmlFileName,Pname,Pdescription,Kname,Kdescription,ColorTheme,ColorMap) {
    if (proj4string(get(PGdv))!="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
      cat("Input SpatialPolygonsDataFrame ",PGdv," re-projected from ",
          proj4string(get(PGdv))," to WGS84 longlat\n",sep="")
      assign(PGdv,spTransform(get(PGdv),CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")))
    } # check projection
    
    kmlFile <- file(kmlFileName, "w")
    Xout <- sapply(slot(get(PGdv), "polygons"), function(x) { 
      kmlPolygon(x,
                 name=as(get(PGdv), "data.frame")[slot(x, "ID"), Pname], 
                 col=get(ColorMap)[as.numeric(as(get(PGdv), "data.frame")[slot(x, "ID"), ColorTheme])],
                 lwd=0.5, border='black', visibility=TRUE,
                 description=as(get(PGdv), "data.frame")[slot(x, "ID"), Pdescription], ) 
    })
    
    cat(kmlPolygon(kmlname=Kname, 
                   kmldescription=Kdescription)$header, 
        file=kmlFile, sep="\n")
    cat(unlist(Xout["style",]), file=kmlFile, sep="\n")
    cat(unlist(Xout["content",]), file=kmlFile, sep="\n")
    cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
    close(kmlFile)
  }
  
  
  # Example Call:
  #kmlPolygons(PGdv="VegMapCRS",kmlFileName="CABR_VegMap.kml",
  #                     Pname="ID",Pdescription="Assn_Code",
  #                     Kname="CABR Draft Vegetation Map",Kdescription="June 2011 Draft CABR Vegetation Map",
  #                     ColorTheme="Assn_Code",ColorMap="ColMapAlpha") 
  
  
  kmlfile2 = paste('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//', "ComboPlotPoly.kml", sep="/")
  proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
  pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
  pdata2 = pdata[pdata@data$RK_CODE %in% preds_wide@data$RK_CODE, ]
  pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
  pdata2@data = join((pdata2@data),preds_wide2@data,by = 'RK_CODE')
  # import html descriptions
  descriptions = data.frame(RK_CODE =unlist(RK_CODE_STORE),desc = unlist(html_desc_holder))  # holder for google chart html code
  pdata2@data = join((pdata2@data),descriptions,by = 'RK_CODE')
  pdata2$gap = c(rep('1',5565),rep('2',10000))
  pdata2$color = c(rep('#df0000aa',5565),rep('#ff0000aa',10000))
  
  kmlPolygons(PGdv="pdata2",kmlFileName="ComboPlotPoly2.kml",
              Pname="RK_NAME",Pdescription="desc",
              Kname="Wheat Yields",Kdescription="Yes wheat",ColorTheme="gap",ColorMap="color") 
    
  
  
  # Polygon GGPLOT Gap map (USED IN PAPER) --------------------------------------------------
    
    proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
    pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
    pdata2 = gSimplify(pdata,tol = 150,topologyPreserve = T) # outputs not spatial dataframe
    pdata2 = SpatialPolygonsDataFrame(pdata2,data=pdata@data) # join back in data
    
    pdata2 = pdata2[pdata2@data$W_CODE %in% preds_wide$W_CODE, ]
    pdata2@data = join((pdata2@data),preds_wide ,by = 'RK_CODE')
    pdata2 = pdata2[!is.na(pdata2$pred_lnwheatOPH_pn4) , ]
    pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
    
    poly = pdata2
    poly@data$id = rownames(poly@data)
    #poly.points = fortify(poly, region="id")
    # avoid memory issues??
    #save(preds_wide,preds_long,poly,poly.points,file='G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplot2.RData' ,version = NULL)
    rm(list = ls())
    load('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplot2.RData' )
    
    # join in wide data 
    poly.df = join(poly.points, poly@data, by="id",type = 'left')
    poly.df = join(poly.df,preds_wide ,by = 'RK_CODE',type = 'left')
    # join in long data (FE alphas)
    pred_long2 = data.frame(fe_u_ln=preds_long[preds_long$YEAR==2011,'fe_u_ln'],fe_u_level=preds_long[preds_long$YEAR==2011,'fe_u_level'],RK_CODE=preds_long[preds_long$YEAR==2011,'RK_CODE'])
    poly.df = join(poly.df,pred_long2 ,by = 'RK_CODE',type = 'left')
    
    # Use woreda fixed effects gap (% difference) (used in paper)
    inner = as.data.frame(read.csv('C:\\Users\\mmann\\Dropbox\\Ethiopia Writeup\\FE outputs.csv'))
    inner = inner[,c("Woreda","Coef.", "X..Difference")]
    names(inner)=c("W_NAME","FE_Coef","FE_Gap_Dif")
    head(inner)
    poly.df = join(poly.df,inner ,by = 'W_NAME',type = 'left')
    
    windows() # (USED IN PAPER)
    ggplot(poly.df) + 
      aes(long,lat,group=as.factor(group),fill= FE_Gap_Dif ) + 
      geom_polygon() +
      coord_equal() +
      scale_fill_gradient2( low = 'red',mid='darkgrey',high = 'green')
  
  
  
  # Polygon GGPLOT Gap map (NOT USED IN PAPER) --------------------------------------------------
  
  proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
  pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
  pdata2 = gSimplify(pdata,tol = 750,topologyPreserve = T) # outputs not spatial dataframe
  pdata2 = SpatialPolygonsDataFrame(pdata2,data=pdata@data) # join back in data
  
  pdata2 = pdata2[pdata2@data$RK_CODE %in% preds_wide$RK_CODE, ]
  pdata2@data = join((pdata2@data),preds_wide ,by = 'RK_CODE')
  pdata2 = pdata2[!is.na(pdata2$pred_lnwheatOPH_pn4) , ]
  pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
  
  poly = pdata2
  poly@data$id = rownames(poly@data)
  #poly.points = fortify(poly, region="id")
  # avoid memory issues??
  #save(preds_wide,preds_long,poly,poly.points,file='G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplot.RData' ,version = NULL)
  rm(list = ls())
  load('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplot.RData' )
  
  # join in wide data 
  poly.df = join(poly.points, poly@data, by="id",type = 'left')
  poly.df = join(poly.df,preds_wide ,by = 'RK_CODE',type = 'left')
  # join in long data (FE alphas)
  pred_long2 = data.frame(fe_u_ln=preds_long[preds_long$YEAR==2011,'fe_u_ln'],fe_u_level=preds_long[preds_long$YEAR==2011,'fe_u_level'],RK_CODE=preds_long[preds_long$YEAR==2011,'RK_CODE'])
  poly.df = join(poly.df,pred_long2 ,by = 'RK_CODE',type = 'left')
  
  names(poly.df)
  poly.df$gap = exp(poly.df$lnwheatOPH4)-exp(poly.df$pred_lnwheatOPH_pa4)
  summary(poly.df$gap)  
  poly.df =poly.df[!is.na(poly.df$gap),]  
  # control outlier
  poly.df$gap[poly.df$gap>30]=30
  
  
  
  windows()
  ggplot(poly.df) + 
    aes(long,lat,group=as.factor(group),fill= gap ) + 
    geom_polygon() +
    coord_equal() +
    scale_fill_gradient2( low = 'red',mid='darkgrey',high = 'green')
  
  
  windows()
  ggplot(poly.df) + 
    aes(long,lat,group=as.factor(group),fill= fe_u_level ) + 
    geom_polygon() +
    coord_equal() +
    scale_fill_gradient2( low = 'red',mid='darkgrey',high = 'green')
  
  
  windows()
  ggplot(poly.df) + 
    aes(long,lat,group=as.factor(group),fill= fe_u_ln ) + 
    geom_polygon() +
    coord_equal() +
    scale_fill_gradient2( low = 'red',mid='darkgrey',high = 'green')
  
  
  
  
  
    
  ###########################################################
  
  # Polygons GAP analysis STDF from 90th percenile compared to FE panel(kebele level) (used in paper) ----------------------------------------------
  

  # inverse area weight mean at woreda level pf gaps in % (reflects population)


  preds_long$ClusterNumEnvYear = paste(preds_long$ClusterNumEnv,preds_long$YEAR,sep='')
  
  ClusterMeans = aggregate(exp(preds_long$lnwheatOPH ),by=list(preds_long$ClusterNumEnvYear),function(x) mean(x,na.rm=T))
  names(ClusterMeans)=c('ClusterNumEnvYear','ClusterMean')
  # deal with one missing value
  ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12012','ClusterMean'] = mean(ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12010','ClusterMean'],ClusterMeans[ClusterMeans$ClusterNumEnvYear=='12011','ClusterMean'])
  ClusterSD = aggregate(exp(preds_long$lnwheatOPH),by=list(preds_long$ClusterNumEnvYear),function(x) sd(x,na.rm=T))
  names(ClusterSD)=c('ClusterNumEnvYear','ClusterSD')
  # AFAR DOESN'T MATTER so don't worry about this. 
  ClusterSD[ClusterSD$ClusterNumEnvYear=='12010','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42010','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52010','ClusterSD'])
  ClusterSD[ClusterSD$ClusterNumEnvYear=='12011','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42011','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52011','ClusterSD'])
  ClusterSD[ClusterSD$ClusterNumEnvYear=='12012','ClusterSD'] = mean(ClusterSD[ClusterSD$ClusterNumEnvYear=='22012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='32012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='42012','ClusterSD'],ClusterSD[ClusterSD$ClusterNumEnvYear=='52012','ClusterSD'])
  
  # create 90 percentile estimates for each woreda based on cluster membership
  Cluster90th = aggregate(exp(preds_long$lnwheatOPH ),by=list(preds_long$ClusterNumEnvYear),function(x) quantile(x,.9,na.rm=T))
  names(Cluster90th)=c('ClusterNumEnvYear','Cluster90th')
  # join stats back in 
  library(plyr)
  preds_long = join(preds_long,ClusterMeans)
  preds_long = join(preds_long,ClusterSD)
  preds_long = join(preds_long,Cluster90th)
  table_data_w = reshape(preds_long,timevar = "YEAR", idvar = c('RK_CODE','R_NAME','W_NAME'), direction = "wide") 
  
  # set up polygons 
  proj = "+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs"
  pdata = readShapePoly("G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//Administrative Areas//Kebele_WLRC_UTM.shp", proj4string=CRS(proj))
  pdata2 = gSimplify(pdata,tol = 200,topologyPreserve = T) # outputs not spatial dataframe
  pdata2 = SpatialPolygonsDataFrame(pdata2,data=pdata@data) # join back in data
  
  # join in gap data  
  pdata2 = pdata2[pdata2@data$RK_CODE %in% table_data_w$RK_CODE, ]
  pdata2@data = join((pdata2@data),table_data_w ,by = 'RK_CODE')
  # dont use # pdata2 = pdata2[!is.na(pdata2$lnwheatOPH.2010) , ]
  pdata2 = spTransform(pdata2,CRS("+proj=longlat"))
  # add in gap estimate  (2010 is actually 2011, just a note)
  pdata2$gap3 = (pdata2$Cluster90th.2010)-exp(pdata2$pred_lnwheatOPH_ols.2010) # backwards to match color
  pdata2$gap4 = (pdata2$Cluster90th.2011)-exp(pdata2$pred_lnwheatOPH_ols.2011)
  pdata2$gap5 = (pdata2$Cluster90th.2012)-exp(pdata2$pred_lnwheatOPH_ols.2012)
  pdata2$RK_NAME = tolower(as.character(pdata2$RK_NAME)) # remove factor levels
  # rbind all data 
  time <- as.POSIXct(strptime(c(rep("2011-01-01", dim(pdata2)[1]), 
                                rep("2012-01-01", dim(pdata2)[1]),rep("2013-01-01", dim(pdata2)[1])), format="%Y-%m-%d"), tz = "GMT")
  data <- data.frame(GAP = c(pdata2$gap3, pdata2$gap4, pdata2$gap5),                            
                     NAMES = c(pdata2$RK_NAME,rep("", dim(pdata2)[1]),rep("", dim(pdata2)[1]))) # rep("", dim(pdata2)[1]) because otherwise there are 3 name labels
  #data$GAP[data$GAP>25]=25   # avoid the outliers
  


  length(time)
  dim(data)
  # copy polygons:
  nc.poly <- rep(slot(pdata2, "polygons"), 3)
  # fix the polygon IDs:
  for(i in 1:length(row.names(data))){nc.poly[[i]]@ID = as.character(i)} 
  #nc.poly[[40000]]@ID # double check values match
  
  # define polygons and space time object
  sp = SpatialPolygons(nc.poly, proj4string=CRS("+proj=longlat +datum=WGS84"))
  # create a STIDF object:
  nct = STIDF(sp, time = time, data = data)
  # write to a KML file:
  data(SAGA_pal)
  #colour_scale=SAGA_pal[[1]]
  #kml(nct, colour = GAP, file = "gapST.kml")
  kml(nct, colour = GAP,colour_scale=SAGA_pal[[1]], file = "G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\Regression Data\\CSA-AgSS\\WheatGap_FEvsCluster90th.kml", labels = NAMES,plot.labpt = T,LabelScale=.5,shape = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png")
  #html.table   optional description block (html) for each GPS point (vertices)
  #colour_scale=SAGA_pal[[1]],
  


  
  
  ############################################################
  # GAP MAP WOREDA FE  --------------------------------------------------------
   
  # read in woreda FE coefficients from "poduction gaps through (Woreda) FE intercepts" in agSS_Field_Info_2010-2013_panel_regression_v3.do
  # this might be OUTDATED... use FE outputs.xlsx instead
  
  coeff= readLines('C:\\Users\\mmann\\Documents\\LnWheatOPHFE_gapln.txt')
  w_code_name = coeff[grep('*W_CODE*', coeff)]   # add 1 to red in coeff rather than name
  W_CODE = gsub("[^0-9]", "", w_code_name)
  WFE_coeff = as.numeric(coeff[grep('*W_CODE*', coeff) +1])  # add 1 to red in coeff rather than name
  out = data.frame(W_CODE,WFE_coeff)
  write.csv(out,'G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\GapMapOutputs\\WFE_GAP_LN.csv') 
  # merge using arcmap and export kml
  
  coeff= readLines('C:\\Users\\mmann\\Documents\\LnWheatOPHFE_gaptot.txt')
  w_code_name = coeff[grep('*W_CODE*', coeff)]   # add 1 to red in coeff rather than name
  W_CODE = gsub("[^0-9]", "", w_code_name)
  WFE_coeff = as.numeric(coeff[grep('*W_CODE*', coeff) +1])  # add 1 to red in coeff rather than name
  out = data.frame(W_CODE,WFE_coeff)
  write.csv(out,'G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\GapMapOutputs\\WFE_GAP_TOT.csv') 
  # merge using arcmap and export kml
  
  
  
  # Panel Data Plots --------------------------------------------------------
  
  #xvar="YEAR", low="rank_min",
  #open="rank_bottom", close="rank_top",
  #high="rank_max",
  #             btm  up  downer upper  line
  #    '["Mon", 28,  10,    -28,    32,  42.8],',
  
  
  #create variables for descriptiosn
  # variables used for weird google charts inputs 
  preds_long$exp_lb_yhatOPH_pn =  exp(preds_long$lb_yhatOPH_pn)
  preds_long$rng_lb_yhatOPH_pn = unlist(lapply(1:dim(preds_long)[1],function(x) diff(range(exp(preds_long$lb_yhatOPH_pn[x]),exp(preds_long$ub_yhatOPH_pn[x])))  ))
  # break out estimates and actual data to display differently 
  # replace actual OPH values where possible
  preds_long$actual_data='Estimate'
  preds_long$actual_data[!is.na(preds_long$lnwheatOPH)]='Actual'
  preds_long$pred_lnwheatOPH_pn[!is.na(preds_long$lnwheatOPH)]=preds_long$lnwheatOPH[!is.na(preds_long$lnwheatOPH)]
  preds_long$actual_data_color = '#43459d'
  preds_long$actual_data_color[preds_long$actual_data=='Estimate'] = '#f1ca3a'
  
  
  # create html code
  html_desc_holder = c()  # holder for google chart html code
  RK_CODE_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points
  RK_NAME_STORE = list()  # store RK_CODES with valid observation (in order) to create XY points
  for(RK_CODES in unique(preds_long$RK_CODE)){
    
    in_data = preds_long[preds_long$RK_CODE==RK_CODES,]   # store data for one RK_CODE
    
    if(is.na(in_data$lb_yhatOPH_pn[1])){next}  # avoid missing values 
    RK_CODE_STORE =c(RK_CODE_STORE,RK_CODES)   # if not skipped store RK_CODE for creation of XY points
    RK_NAME_STORE =c(RK_NAME_STORE,in_data$RK_NAME[!is.na(in_data$RK_NAME)][1])   # if not skipped store RK_NAME for creation of XY points
    
    html_desc=c('<html>',
                '<head>',
                '<script type="text/javascript" src="https://www.google.com/jsapi"></script>',
                '<script type="text/javascript">',
                'google.load("visualization", "1", {packages:["corechart"]});',
                'google.setOnLoadCallback(drawVisualization);',
                
                'function drawVisualization() {',
                '// Create and populate the data table.',
                'var data = new google.visualization.DataTable();',
                'data.addColumn("string", "Day");',
                'data.addColumn("number", "Estimate");',
                'data.addColumn("number", "Range");',
                'data.addColumn({type: "number", role: "interval"});',
                'data.addColumn({type: "number", role: "interval"});',
                paste('data.addColumn(\'number\',\'',in_data$actual_data[1], '\');',sep=""),
                'data.addRows([', 
                paste('[','\"',in_data$YEAR[1],'\"',',',exp(in_data$lb_yhatOPH_pn[1]) ,',', diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )),',', -1*diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )),',',  2*diff(range(exp(in_data$lb_yhatOPH_pn[1]),exp(in_data$ub_yhatOPH_pn[1]) )) ,',',exp(in_data$pred_lnwheatOPH_pn[1]),'],'),
                paste('[','\"',in_data$YEAR[2],'\"',',',exp(in_data$lb_yhatOPH_pn[2]) ,',', diff(range(exp(in_data$lb_yhatOPH_pn[2]),exp(in_data$ub_yhatOPH_pn[2]) )),',', -1*diff(range(exp(in_data$lb_yhatOPH_pn[2]),exp(in_data$ub_yhatOPH_pn[2]) )) ,',', 2*diff(range(exp(in_data$lb_yhatOPH_pn[2]),exp(in_data$ub_yhatOPH_pn[2]) )) ,',',exp(in_data$pred_lnwheatOPH_pn[2]),'],'),
                paste('[','\"',in_data$YEAR[3],'\"',',',exp(in_data$lb_yhatOPH_pn[3]) ,',', diff(range(exp(in_data$lb_yhatOPH_pn[3]),exp(in_data$ub_yhatOPH_pn[3]) )),',', -1*diff(range(exp(in_data$lb_yhatOPH_pn[3]),exp(in_data$ub_yhatOPH_pn[3]) )) ,',', 2*diff(range(exp(in_data$lb_yhatOPH_pn[3]),exp(in_data$ub_yhatOPH_pn[3]) )) ,',',exp(in_data$pred_lnwheatOPH_pn[3]),'],'),
                ' ]);',
                
                '// Create and draw the visualization.',
                'var ac = new google.visualization.ComboChart(document.getElementById("visualization"));',
                'ac.draw(data, {',
                'title : "Estimated and target wheat yeilds",',
                'width: 600,',
                'height: 400,',
                'vAxis: {title: "Wheat Yield"},',
                'hAxis: {title: "Year"},',
                'isStacked: true,',
                'seriesType: "bars",',
                paste('series: {0: {color: "transparent"}, 2: {type: "line",color: "', in_data$actual_data_color[1] ,'"}   }',sep=""),
                ' });',
                '}',
                '</script>',
                '</head>',
                '<body>',
                '<div id="visualization" style="width: 600px; height: 500px;"></div>',
                '</body>',
                '</html>')
    
    html_desc_holder =c(html_desc_holder,paste(html_desc,collapse='\r'))
  }  
  
  
  # Assign initial projection and then reproject to lat lon
  preds_wide2 = read.csv('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//Regression_Preds_wide.csv',as.is=T)
  # limit sample to correct RK_CODES
  preds_wide2 = preds_wide2[ preds_wide2$RK_CODE %in% RK_CODE_STORE,]
  # get points into same order as html  
  library(plyr)
  preds_wide2 = join(data.frame(RK_CODE=unlist(RK_CODE_STORE)),preds_wide2)
  preds_wide2 = preds_wide2[!is.na(preds_wide2$Cen_X), ]   # drop missing XY values MUST DO THIS
  
  # find unique locations 
  coordinates(preds_wide2) = ~ Cen_X+Cen_Y
  proj4string(preds_wide2) = CRS("+proj=utm +zone=37 +ellps=clrk80 +towgs84=-166,-15,204,0,0,0,0 +units=m +no_defs")
  preds_wide2=spTransform(preds_wide2,CRS("+proj=longlat"))
  
  # WRite KML
  kmlname <- "Wheat Output per Hectare"
  kmldescription <- paste("Target range to Wheat Output per Hectare."  )
  icon <- "http://maps.google.com/mapfiles/kml/pal4/icon57.png"  # set which image to use as point on map
  #icon = rep("http://maps.google.com/mapfiles/kml/pal4/icon57.png",times = length(preds_long$actual_data))
  #icon[preds_long$actual_data=='Actual']='http://maps.google.com/mapfiles/kml/pal4/icon63.png'
  preds_wide2$RK_NAME = sub('&',replacement = 'and',x = preds_wide2$RK_NAME) # avoid issue with &s 
  name <- preds_wide2$RK_NAME    # add a label for each point
  kmlfile = paste('G://Faculty//Mann//Projects//Ethiopia Project//Regression Data//CSA-AgSS//', "ComboPlot.kml", sep="/")
  
  kmlPoints(preds_wide2, kmlfile=kmlfile, name=name, description= html_desc_holder,
            icon=icon, kmlname=kmlname, kmldescription=kmldescription)
  
  

   