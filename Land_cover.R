#Papua Barat

#library(tiff)
library(raster)
#library(rgdal)

#Set working directory
df<- "C:/ICRAF/IO/Papua_Barat/Land_cover/input_file/ICRAF"
setwd(df)

#Writing project file

#Read file from rif files
lc_2018<-'lc2018_papbar_100m.tif' 
imported_raster<-raster(lc_2018)
area_of_papbar<-freq(imported_raster)
area_of_papbar<- as.data.frame(area_of_papbar) #calculate area of land cover classess

#Read input files
sector<- read.csv("sector.csv", header=FALSE, sep =",")
land_dist<- read.csv("land_dist.csv", header=FALSE, sep =",")
land_dist.m<- as.matrix(land_dist)
land_cov<- read.csv("land_cover.csv", header=FALSE, sep =",")
land_cov.m<- as.matrix(land_cov)
output<- read.csv("tot_output.csv", header=FALSE, sep =",")
output.m<- as.matrix(output)
BAU_scen<- read.csv("BAU_scenario.csv", header=TRUE, sep =",")


#Calculate matrix diagonal of land cover area
diag_land.cov<- diag(unlist(land_cov[,2]))   

#Calculate land cover requirement
land_req<-land_dist.m%*%diag_land.cov
land_req.sec<- rowSums(land_req)
LRC<-land_req.sec/output.m
LPC<- output.m/land_req.sec


#Forloop
for(i in 3:ncol(BAU_scen)) {
  diag_land.cov<- diag(unlist(land_cov[,2]))   
  land_req<-land_dist.m%*%diag_land.cov
  land_req.sec<- rowSums(land_req)
  
  LRC<-land_req.sec/output.m
  LPC<- output.m/land_req.sec
  
}

#Plot GD



