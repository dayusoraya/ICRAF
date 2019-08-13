#Papua Barat
#Line 50 utk Looping calculationnya masih ada error pak. saya coba perbaiki

library(raster)

#Set working directory
df<- "C:/ICRAF/IO/Papua_Barat/Land_cover/input_file/ICRAF/ICRAF"
setwd(df)

#Writing project file

#Read file from tif files
#calculate area of land cover classess
list_of_raster_file <- list.files(df, pattern="\\.tif$")
length_of_raster<-length(list_of_raster_file)

initial_year<-2018
land_cov<- read.csv("land_cover.csv", header=TRUE, sep =",")
for(i in 1:length_of_raster){

  eval(parse(text=(paste0("lc", initial_year + (i-1)*3, "<-raster('", list_of_raster_file[i], "')"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, "<-freq(lc", initial_year + (i-1)*3, ")"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, "<- as.data.frame(area_papbar_", initial_year + (i-1)*3, ")"))))
  eval(parse(text=(paste0("colnames(area_papbar_", initial_year + (i-1)*3, ")=c('ID', 'lc", initial_year + (i-1)*3, "')"))))
  eval(parse(text=(paste0("land_cov<-merge(land_cov, area_papbar_", initial_year + (i-1)*3, ", by='ID',all=TRUE)"))))
  
}


#Subset No Data from the land_cov table
final_land.cov<- subset(land_cov[2:20,])

#Load another input files
land_dist<- read.csv("land_dist.csv", header=FALSE, sep =",")
land_dist.m<-as.matrix(land_dist)
int_demand<- read.csv("int_demand.csv", header=FALSE, sep=",")
add_val<- read.csv("add_value.csv", header=FALSE, sep =",")
fin_demand<- read.csv("fin_demand.csv", header=FALSE, sep =",")


#Calculation
tot_fin.dem<- rowSums(fin_demand)
output<- tot_fin.dem+rowSums(int_demand)
tot_add.val<-colSums(add_val)

prop_GDP<- tot_add.val/output
prop_inc<- add_val[1,]/output
prop_profit<- add_val[2]/output

#Looping
Results<-NULL
for(i in 3:ncol(final_land.cov)) {
  diag_land.cov<- diag(unlist(final_land.cov[,i]))   
  land_req<-land_dist.m %*% diag_land.cov
  LR<- rowSums(land_req)
  
  tot_fin.dem<- rowSums(fin_demand)
  output<- tot_fin.dem+rowSums(int_demand)
  tot_add.val<-colSums(add_val)
  
  prop_GDP<- tot_add.val[i-2]/output[i-2]
  prop_inc<- add_val[i-2]/output[i-2]
  prop_profit<- add_val[i-2]/output[i-2]
  FD_prop<- tot_fin.dem[i-2]/output[i-2]
  LRC<- LR/output[i-2]
  LPC<- output[i-2]/LR
  GDP<- prop_GDP*output[i-2]
  Income<- prop_inc*output[i-2]
  Profit<- prop_profit*output[i-2]
  
  Results[[i-2]]<-cbind(output[i-2],tot_fin.dem[i-2],FD_prop[i-2],LR,LRC,LPC,GDP,Income,Profit)
  names(Results[[i-2]])<-c("Ouput","FD","FD_Prop","LR","LRC","LPC","GDP","Income","Profit")
  Results[[i-2]]$FD_prop[is.nan(Results[[i-2]]$LRC)] <-0
  Results[[i-2]]$LRC[is.infinite(Results[[i-2]]$FD_prop)] <-0
  names(Results[[i-2]])<-paste(c("Ouput","FD","FD_Prop","LR","LRC","LPC","GDP","Income","Profit"))
  
}

View(Results[[1]])

.  
#Plot GDP