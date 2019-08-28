#Papua Barat

library(raster)
library(tidyverse)

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
non_lb<- read.csv("non_land_based.csv", header=TRUE, sep = ",")
rate_grad<-read.csv("gradual_rate.csv", header = FALSE, sep=",")


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
Results<-cbind(tot_fin.dem,FD_prop,LR,LRC,LPC,output,GDP,Income,Profit)

Results[,][is.nan(Results[,])] <- 0
Results[,][is.infinite(Results[,])] <- 0
colnames(Results)<- c("FD2018","FD_Prop","LR","LRC","LPC","Output_sim.2018","GDP_sim.2018","Income_sim.2018","Profit_sim.2018")
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
  Results_2[[i-3]]<-cbind(Output_sim,GDP_sim,Income_sim,Profit_sim)
  
  names(Results_2[[i-3]])<- c("Output_Sim","GDP_sim","Income_sim","Profit_sim")
  colnames(Results_2[[i-3]])<- paste(c("Output_sim","GDP_sim","Income_sim","Profit_sim"),2018+(i-3)*3)
  
  Results_2.dat<-as.data.frame(Results_2)
  Results_2.m<-as.matrix(Results_2.dat)
  Results_2.m[is.nan(Results_2.m)] <- 0
  Results_2.m[is.infinite(Results_2.m)] <- 0
  
  #Make 5 different tables considering the same ....variables (outside looping) avoid hard-code (11->for the colnames)
  
  #eval(parse(text=(paste0("Output", initial_year2 + (i-3)*3, "<- as.data.frame(Output_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("GDP", initial_year2 + (i-3)*3, "<- as.data.frame(GDP_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("Income", initial_year2 + (i-3)*3, "<- as.data.frame(Income_sim[",i ,"],)"))))
  #eval(parse(text=(paste0("Profit", initial_year2 + (i-3)*3, "<- as.data.frame(Profit_sim[",i ,"],)"))))
  
  #eval(parse(text=(paste0("final_table<-cbind(Output", initial_year + (i-1)*3,",","GDP", initial_year + (i-1)*3,",","Income", initial_year + (i-1)*3,",","Profit", initial_year + (i-1)*3,",)"))))

  }

#Combine Output, GDP, Income and Profit into 1 table
Results_2.dat<-as.data.frame(Results_2.m)
Final_table<-cbind(sector,Results.dat[,6:9],Results_2.dat)

#Combine results of year 2018 and all those simulation
all_tables<- cbind(Results.dat,Results_2.dat)

#Categorize the tables based on its GDP
GDP_tables<-cbind.data.frame(Final_table[,1],Final_table[,5],Final_table[,9],Final_table[,13],Final_table[,17],Final_table[,21],Final_table[,25],
                  Final_table[,29],Final_table[,33],Final_table[,37],Final_table[,41],Final_table[,45])
colnames(GDP_tables)<- c("Sector","GDP_2018","GDP_2021","GDP_2024","GDP_2027","GDP_2030","GDP_2033","GDP_2036","GDP_2039","GDP_2042","GDP_2045","GDP_2048")
write.csv(GDP_tables,file="/ICRAF/IO/Papua_Barat/Land_cover/input_file/ICRAF/ICRAF/GDP_tables.csv", row.names = FALSE)

#Plot GDP ->LINE CHART
GDP_tot<-colSums(GDP_tables[,2:12])
Year<- c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
GDP_plot<- cbind(Year,GDP_tot)
GDP_plot<-as.data.frame(GDP_plot)
GDP_graph<-ggplot(GDP_plot,aes(x=Year, y=GDP_tot))+geom_line(color="red")+geom_point()


#Percentage of economic growth based on GDP rate yoy
growth_2021= (GDP_tot[2]-GDP_tot[1])/GDP_tot[1]
growth_2024= (GDP_tot[3]-GDP_tot[2])/GDP_tot[2]
growth_2027= (GDP_tot[4]-GDP_tot[3])/GDP_tot[3]
growth_2030= (GDP_tot[5]-GDP_tot[4])/GDP_tot[4]
growth_2033= (GDP_tot[6]-GDP_tot[5])/GDP_tot[5]
growth_2036= (GDP_tot[7]-GDP_tot[6])/GDP_tot[6]
growth_2039= (GDP_tot[8]-GDP_tot[7])/GDP_tot[7]
growth_2042= (GDP_tot[9]-GDP_tot[8])/GDP_tot[8]
growth_2045= (GDP_tot[10]-GDP_tot[9])/GDP_tot[9]
growth_2048= (GDP_tot[11]-GDP_tot[10])/GDP_tot[10]
growth_rate<- rbind(growth_2021,growth_2024,growth_2027,growth_2030,growth_2033,growth_2036,growth_2039,growth_2042,growth_2045,growth_2048)
year_of_year<- c(2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
all_growth.rate<- cbind.data.frame(year_of_year,growth_rate)
colnames(all_growth.rate)<-c("Tahun","GDP_growth_rate")
growth_rate.avg= mean(growth_rate)

#Make a bar chart based on the GDP growth rate over year
growth_rate.graph<-ggplot(data=all_growth.rate,aes(x=Tahun, y=GDP_growth_rate))+geom_bar(color="red", stat = "identity")

#Calculate economic growth using GDP data only for non land-based sector

#Subset GDP non land-based sector
nlb_sector<-subset(GDP_tables[18:35,2:12])
GDP_nlb<- colSums(nlb_sector)


#Calculate average economic growth rate of non land-based only (%) based on GDP data for the past 5 years (2014-2018)
nlb_tot<- colSums(non_lb[,2:7])
nlb_2014=(nlb_tot[2]-nlb_tot[1])/nlb_tot[1]
nlb_2015=(nlb_tot[3]-nlb_tot[2])/nlb_tot[2]
nlb_2016=(nlb_tot[4]-nlb_tot[3])/nlb_tot[3]
nlb_2017=(nlb_tot[5]-nlb_tot[4])/nlb_tot[4]
nlb_2018=(nlb_tot[6]-nlb_tot[5])/nlb_tot[5]
avg_nlb.rate<- mean(c(nlb_2015,nlb_2016,nlb_2017,nlb_2018))


#Projection of GDP growth for non land-based sector using linear rate
growth_nlb.2021=GDP_nlb[2]+(GDP_nlb[2]*avg_nlb.rate)
growth_nlb.2024=GDP_nlb[3]+(GDP_nlb[3]*avg_nlb.rate)
growth_nlb.2027=GDP_nlb[4]+(GDP_nlb[4]*avg_nlb.rate)
growth_nlb.2030=GDP_nlb[5]+(GDP_nlb[5]*avg_nlb.rate)
growth_nlb.2033=GDP_nlb[6]+(GDP_nlb[6]*avg_nlb.rate)
growth_nlb.2036=GDP_nlb[7]+(GDP_nlb[7]*avg_nlb.rate)
growth_nlb.2039=GDP_nlb[8]+(GDP_nlb[8]*avg_nlb.rate)
growth_nlb.2042=GDP_nlb[9]+(GDP_nlb[9]*avg_nlb.rate)
growth_nlb.2045=GDP_nlb[10]+(GDP_nlb[10]*avg_nlb.rate)
growth_nlb.2048=GDP_nlb[11]+(GDP_nlb[11]*avg_nlb.rate)
nlb_growth.rate<-rbind(growth_nlb.2021,growth_nlb.2024,growth_nlb.2027,growth_nlb.2030,growth_nlb.2033,growth_nlb.2036,
                       growth_nlb.2039,growth_nlb.2042,growth_nlb.2045,growth_nlb.2048)
all_nlb.rate<-cbind.data.frame(year_of_year,nlb_growth.rate)
colnames(all_nlb.rate)<-c("Years","GDP_growth_rate_nlb")

#Combine projection GDP growth land-based sector with non land-based sector (linear rate) 
#a. Subset GDP land based sector using LR calculations
GDP_LR<-subset.data.frame(GDP_tables[1:17,])

#b.Project the changes of GDP over years using 6% of linear rate
GDP_Lin<-NULL
for (i in 2:ncol(GDP_tables)) {
  
  GDPval<-GDP_tables[,i]+(GDP_tables[,i]*0.06*(3*(i-1)))
  GDP_Lin[[i-1]]=GDPval
  GDP_Lin[[i-1]]<-cbind(GDPval)
  colnames(GDP_Lin[[i-1]])<- paste(c("GDP_"),2018+(i-2)*3,sep="")
  GDP.dat<-as.data.frame(GDP_Lin)
  
}

#c.According to the calculation of linear rate change, subset only for non land-based sectors
Lin_nlb<- subset(GDP.dat[18:35,])
Lin_nlb<- cbind(sector[18:35,1],Lin_nlb)
colnames(Lin_nlb)[1]<- "Sector"

#d.Combine land-based and non land-based sectors
Linear_all<-rbind(GDP_LR,Lin_nlb)
Linear_all.1<-colSums(Linear_all[,2:12])
Years<- c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
Linear_all.fin<-cbind.data.frame(Years,Linear_all.1)

#e. Plot the GDP growth
linear_nlb.graph<-ggplot(data=Linear_all.fin,aes(x=Years, y=Linear_all.1))+geom_line(color="red", stat = "identity")+geom_point()


#Calculate GDP growth using gradual rate

#a. Calculate the gradual rate from 2015-2018
grad_2015=(nlb_2015-nlb_2014)/nlb_2014
grad_2016=(nlb_2016-nlb_2015)/nlb_2015
grad_2017=(nlb_2017-nlb_2016)/nlb_2016
grad_2018=(nlb_2018-nlb_2017)/nlb_2017
grad_rate=mean(c(grad_2015,grad_2016,grad_2017,grad_2018))

#b.Calculate the changes of GDP rate using gradual rate (1,5%)
#GDP_grad<-NULL
#GDP_grad.2<-NULL
#initial_rate<-nlb_2018

#for (i in 1:29) {
#Grad_val<-initial_rate+(initial_rate*grad_rate)
#GDP_grad[[i]]<-Grad_val[i]+(Grad_val[i]*grad_rate)
  
 # Grad_val[i]=GDP_grad

  #GDP_grad.2[[i]]=GDP_grad[[i]]+(GDP_grad[[i]]*grad_rate)
  #GDP_grad[[i]]=GDP_grad.2[[i]]
  #Grad_dat<-as.data.frame(GDP_grad.2)
  
#}

#c. Projects the GDP using different gradual rates each year
Grad_res.2021=nlb_sector[,2]+((nlb_sector[,2])*(rate_grad[2,2]))
Grad_res.2024=nlb_sector[,3]+((nlb_sector[,3])*(rate_grad[3,2]))
Grad_res.2027=nlb_sector[,4]+((nlb_sector[,4])*(rate_grad[4,2]))
Grad_res.2030=nlb_sector[,5]+((nlb_sector[,5])*(rate_grad[5,2]))
Grad_res.2033=nlb_sector[,6]+((nlb_sector[,6])*(rate_grad[6,2]))
Grad_res.2036=nlb_sector[,7]+((nlb_sector[,7])*(rate_grad[7,2]))
Grad_res.2039=nlb_sector[,8]+((nlb_sector[,8])*(rate_grad[8,2]))
Grad_res.2042=nlb_sector[,9]+((nlb_sector[,9])*(rate_grad[9,2]))
Grad_res.2045=nlb_sector[,10]+((nlb_sector[,10])*(rate_grad[10,2]))
Grad_res.2048=nlb_sector[,11]+((nlb_sector[,11])*(rate_grad[11,2]))

Grad_res<-cbind.data.frame(sector[18:35,1],nlb_sector[,1],Grad_res.2021,Grad_res.2024,Grad_res.2027,Grad_res.2030,
                           Grad_res.2033,Grad_res.2036,Grad_res.2039,Grad_res.2042,Grad_res.2045,Grad_res.2048)
colnames(Grad_res)<- c("Sector","GDP_2018","GDP_2021","GDP_2024","GDP_2027","GDP_2030","GDP_2033","GDP_2036","GDP_2039",
                       "GDP_2042","GDP_2045","GDP_2048")
Grad_all<-rbind(GDP_LR,Grad_res)

#d. Plot the gradual changes of GDP
Gradual_all.1<-colSums(Grad_all[,2:12])
Grad_all.fin<-cbind.data.frame(Years,Gradual_all.1)
grad_nlb.graph<-ggplot(data=Grad_all.fin,aes(x=Years, y=Gradual_all.1))+geom_line(color="red", stat = "identity")+geom_point()


#Plot annual, linear and gradual graph into one
GDP_all<-cbind.data.frame(GDP_plot,Linear_all.fin[,2],Grad_all.fin[,2])
colnames(GDP_all)<-c("Year","GDP_baseline","GDP_linear","GDP_gradual")

all_GDP.graph<- ggplot(GDP_all, aes(x=Year)) + 
  geom_line(aes(y = GDP_baseline), color = "darkred") + 
  geom_line(aes(y = GDP_linear), color="blue") +
  geom_line(aes(y=GDP_gradual), color="green", linetype="twodash") 
