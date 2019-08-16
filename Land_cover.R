#Papua Barat

library(raster)
library(tidyverse)
library(ggplot2)
library(plotly)

#Set working directory
df<- "C:/ICRAF/IO/Papua_Barat/Land_cover/input_file/ICRAF/ICRAF"
setwd(df)

#Writing project file

#Read file from tif files
#calculate area of land cover classess
list_of_raster_file <- list.files(df, pattern="\\.tif$")  #type of data character, class=list, 1 column, 11 variables
length_of_raster<-length(list_of_raster_file)

initial_year<-2018
#
land_cov<- read.csv("land_cover.csv", header=TRUE, sep =",")
for(i in 1:length_of_raster){

  eval(parse(text=(paste0("lc", initial_year + (i-1)*3, "<-raster('", list_of_raster_file[i], "')"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, "<-freq(lc", initial_year + (i-1)*3, ")"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, "<- as.data.frame(area_papbar_", initial_year + (i-1)*3, ")"))))
  eval(parse(text=(paste0("colnames(area_papbar_", initial_year + (i-1)*3, ")=c('ID', 'lc", initial_year + (i-1)*3, "')"))))
  eval(parse(text=(paste0("land_cov<-merge(land_cov, area_papbar_", initial_year + (i-1)*3, ", by='ID',all=TRUE)"))))
  
}

View(land_cov)

#Subset No Data from the land_cov table
final_land.cov<- subset(land_cov[2:20,])

#Load another input files
land_dist<- read.csv("land_dist.csv", header=FALSE, sep =",")
land_dist.m<-as.matrix(land_dist)
int_demand<- read.csv("int_demand.csv", header=FALSE, sep=",")
add_val<- read.csv("add_value.csv", header=FALSE, sep =",")
fin_demand<- read.csv("fin_demand.csv", header=FALSE, sep =",")
sector<-read.csv("sector.csv", header=FALSE, sep =",")
sector.m<-as.matrix(sector)


#CALCULATE INVERS LEONTIEF
int_demand.m<-as.matrix(int_demand)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_demand.m)
int_demand.ctot<-colSums(int_demand.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_demand.ctot+add_val.ctot)
fin_con[is.infinite(fin_con)]<-0
t.input.invers<-diag(fin_con)
A<-int_demand.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)


#Calculation for year 2018
tot_fin.dem<- rowSums(fin_demand)
output<-tot_fin.dem+rowSums(int_demand)
tot_add.val<-colSums(add_val)
diag_land.cov<- diag(unlist(final_land.cov[,3]))   
land_req<-land_dist.m %*% diag_land.cov
LR<- rowSums(land_req)
prop_GDP<- tot_add.val/output
prop_inc<- add_val[1,]/output
prop_profit<- add_val[2,]/output
FD_prop<- tot_fin.dem/output
LRC<- LR/output
LPC<- output/LR
GDP<- prop_GDP*output
Income<- t(prop_inc)*output
Profit<- t(prop_profit)*output
#Include the sectors name
Results<-cbind(output,tot_fin.dem,FD_prop,LR,LRC,LPC,GDP,Income,Profit)
Results[,][is.nan(Results[,])] <- 0
Results[,][is.infinite(Results[,])] <- 0
colnames(Results)<- c("Output-2018","FD-2018","FD_Prop","LR","LRC","LPC","GDP2018","Income-2018","Profit-2018")
Results.dat<-as.data.frame(Results)


#Looping Simulation from year 2021-2051
final_table<-NULL
Results_2<-NULL
initial_year2<-2018
for(i in 4:ncol(final_land.cov)) {                      
  #land_cover=final_land.cov[,4:length(final_land.cov)]
  diag_land.cov2<- diag(unlist(final_land.cov[,i]))   
  land_req2<-land_dist.m %*% diag_land.cov2
  LR2<- rowSums(land_req2)
  FD_sim<-(LR2*LPC)*FD_prop
  
  FD_sim[is.nan(FD_sim)] <- 0
  Output_sim<-Leontief %*% FD_sim
  
  GDP_sim<- Output_sim*prop_GDP
  Income_sim<- t(prop_inc)*Output_sim
  Profit_sim<- t(prop_profit)*Output_sim
  #sector
  Results_2[[i-3]]<-cbind(FD_sim,Output_sim,GDP_sim,Income_sim,Profit_sim)
  
  Results_2[i-3][is.nan(Results_2[i-3])] <- 0
  Results_2[i-3][is.infinite(Results_2[i-3])] <- 0
  
  names(Results_2[[i-3]])<- c("FD_sim", "Output_Sim","GDP_sim","Income_sim","Profit_sim")
  colnames(Results_2[[i-3]])<- paste(c("FD_sim", "Output_Sim","GDP_sim","Income_sim","Profit_sim"),2018+(i-3)*3)
  
  Results_2.dat<-as.data.frame(Results_2)
  
  #Make 5 different tables considering the same ....variables (outside looping) avoid hard-code (11->for the colnames)
  
  #unlist(Results_2,recursive = TRUE, use.names = TRUE)
  
  #final_table[[i-3]]<-cbind(Output_sim,GDP_sim,Income_sim,Profit_sim)
  #colnames(final_table[[i-3]])<-paste(c("Output","GDP","Income","Profit"),2018+(i-3)*3)
  
  
  #eval(parse(text=(paste0("Output", initial_year2 + (i-3)*3, "<- as.data.frame(Output_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("GDP", initial_year2 + (i-3)*3, "<- as.data.frame(GDP_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("Income", initial_year2 + (i-3)*3, "<- as.data.frame(Income_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("Profit", initial_year2 + (i-3)*3, "<- as.data.frame(Profit_sim[",i ,"],)"))))
  
  #eval(parse(text=(paste0("final_table<-cbind(Output", initial_year + (i-1)*3,",","GDP", initial_year + (i-1)*3,",","Income", initial_year + (i-1)*3,",","Profit", initial_year + (i-1)*3,",)"))))
  
  
}

#Combine results of year 2018 and all those simulation

all_tables<- cbind(Results.dat,Results_2.dat)

all_GDP<-cbind(Results.dat$GDP2018,Results_2.dat$GDP_sim.2021, Results_2.dat$GDP_sim.2024,Results_2.dat$GDP_sim.2027,Results_2.dat$GDP_sim.2030,
               Results_2.dat$GDP_sim.2033,Results_2.dat$GDP_sim.2036,Results_2.dat$GDP_sim.2039,Results_2.dat$GDP_sim.2042,
               Results_2.dat$GDP_sim.2045,Results_2.dat$GDP_sim.2048)

all_GDP[,][is.nan(all_GDP[,])] <- 0

#all_GDP.2<-cbind(sector$V1,all_GDP)


GDP_tot<-colSums(all_GDP)
GDP_tot.dat<-as.data.frame(GDP_tot)
#rownames(GDP_avg.dat)<- paste(c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048))
#colnames(GDP_avg.dat)[1]<-"Year"
#colnames(GDP_avg.dat)[2]<-"GDP"
#GDP_tot.dat$newcolumn<-c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
Years<- c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
GDP_tot.dat2<- cbind(Years,GDP_tot.dat)


#Plot GDP ->LINE CHART

GDP_graph2<-ggplot(GDP_tot.dat2,aes(x=Years, y=GDP_tot))+geom_line()
GDP_graph2

#Percentage of economic growth based on GDP rate yoy
#GDP_growth.tables<-NULL
#for(i in 1(GDP_tot.dat2)[[i]]){
  #GDP_growth.tables<- GDP(2018/initial_year+(i-3)*3)
  #GDP_growth.tables.dat<-as.data.frame(GDP_growth.tables)
#}                


#Subset non land-based sector
nlb_sector<-subset(all_GDP[26:35,])
GDP_nlb<- colSums(nlb_sector)
GDP_growth<-GDP_nlb*1.1251

#Plot GDP growth of non land-based sector

install.packages("plotly")
