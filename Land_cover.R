#Papua Barat

library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(plyr)

#Set working directory
df<- "D:/IO/Papua_Barat/Land_cover/input_file/ICRAF/ICRAF"
#AE-WD
#df <- "D:/1_Dsoraya/icraf/"
setwd(df)

#Writing project file

#Read file from tif files
#calculate area of land cover classess
list_of_raster_file <- list.files(df, pattern="\\.tif$")  #type of data character, class=list, 1 column, 11 variables
length_of_raster <- length(list_of_raster_file)

initial_year <- 2018
period <- 3
#
land_cov<- read.csv("land_cover.csv", header=TRUE, sep =",")
for(i in 1:length_of_raster){
  
  eval(parse(text=(paste0("lc", initial_year + (i-1)*3, "<-raster('", list_of_raster_file[i], "')"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, " <- freq(lc", initial_year + (i-1)*period, ")"))))
  eval(parse(text=(paste0("area_papbar_", initial_year + (i-1)*3, " <- as.data.frame(area_papbar_", initial_year + (i-1)*3, ")"))))
  eval(parse(text=(paste0("colnames(area_papbar_", initial_year + (i-1)*3, ")=c('ID', 'lc", initial_year + (i-1)*3, "')"))))
  eval(parse(text=(paste0("land_cov <- merge(land_cov, area_papbar_", initial_year + (i-1)*3, ", by='ID',all=TRUE)"))))
  
}

View(land_cov)

#Subset No Data from the land_cov table
# AE: first you need to define the ID value of NODATA please consider the following code
# AE: also consider to use'dplyr' package to help on subsetting dataframe
# AE: take a look at https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

nodatavalue <- 0
final_land.cov <- land_cov  %>% filter(ID != nodatavalue)
#final_land.cov<- subset(land_cov[2:20,])

#Load another input files
land_dist <- read.csv("land_dist.csv", header = FALSE, sep = ",")
land_dist.m <- as.matrix(land_dist)
int_demand <- read.csv("int_demand.csv", header = FALSE, sep = ",")
add_val <- read.csv("add_value.csv", header = FALSE, sep = ",")
fin_demand <- read.csv("fin_demand.csv", header = FALSE, sep = ",")
sector <- read.csv("sector.csv", header = FALSE, sep = ",")
sector.m <- as.matrix(sector)
non_lb <- read.csv("non_land_based.csv", header = TRUE, sep = ",")
rate_grad <- read.csv("gradual_rate.csv", header = FALSE, sep = ",")
sector_nlb <- read.csv("sector_nlb.csv", header = FALSE, sep = ",")
list_nlb<-read.csv("list_sector_nlb.csv", header = FALSE, sep = ",")

#Load inputs for intervention 1 (Pembangunan industri hilir)
add_val.hilir<-read.csv("add_val_hilir.csv", header = FALSE, sep = ",")
int_dem.hilir<-read.csv("int_demand_hilir.csv", header = FALSE, sep = ",")


#CALCULATE INVERS LEONTIEF
int_demand.m <- as.matrix(int_demand)
add_val.m <- as.matrix(add_val)
dim <- ncol(int_demand.m)
int_demand.ctot <- colSums(int_demand.m)
add_val.ctot <- colSums(add_val.m)
fin_con <- 1 / (int_demand.ctot + add_val.ctot)
fin_con[is.infinite(fin_con)] <- 0
t.input.invers <- diag(fin_con)
A <- int_demand.m %*% t.input.invers
I <- as.matrix(diag(dim))
I_A <- I - A
Leontief <- solve(I_A)


#Calculation for year 2018
tot_fin.dem <- rowSums(fin_demand)
output <- tot_fin.dem + rowSums(int_demand)
tot_add.val <- colSums(add_val)
diag_land.cov <- diag(unlist(final_land.cov[, 3])) # AE: What s teh purpose in having "unlist" here?
land_req <- land_dist.m %*% diag_land.cov
LR <- rowSums(land_req)
prop_GDP <- tot_add.val / output
prop_inc <- add_val[1, ] / output
prop_profit <- add_val[2, ] / output
FD_prop <- tot_fin.dem / output
LRC <- LR / output
LPC <- output / LR
GDP <- prop_GDP * output

#transpose
Income<- t(prop_inc)*output
Profit<- t(prop_profit)*output

#Include the sectors name
Results<-cbind(tot_fin.dem,FD_prop,LR,LRC,LPC,output,GDP,Income,Profit)
Results[,][is.nan(Results[,])] <- 0
Results[,][is.infinite(Results[,])] <- 0
colnames(Results)<- c("FD2018","FD_Prop","LR","LRC","LPC","Output_sim.2018","GDP_sim.2018","Income_sim.2018","Profit_sim.2018")
Results.dat<-as.data.frame(Results)


#Looping Simulation from year 2021-2048
final_table <- NULL
Results_2 <- NULL
initial_year2 <- 2018
for(i in 4:ncol(final_land.cov)) {
  #land_cover=final_land.cov[,4:length(final_land.cov)]
  diag_land.cov2 <- diag(unlist(final_land.cov[, i]))
  land_req2 <- land_dist.m %*% diag_land.cov2
  LR2 <- rowSums(land_req2)
  FD_sim <- (LR2 * LPC) * FD_prop
  
  FD_sim[is.nan(FD_sim)] <- 0      #changes
  Output_sim <- Leontief %*% FD_sim
  
  GDP_sim <- Output_sim * prop_GDP
  Income_sim <- t(prop_inc) * Output_sim
  Profit_sim <- t(prop_profit) * Output_sim
  Results_2[[i - 3]] <-
    cbind(Output_sim, GDP_sim, Income_sim, Profit_sim)
  
  names(Results_2[[i - 3]]) <-
    c("Output_Sim", "GDP_sim", "Income_sim", "Profit_sim")
  colnames(Results_2[[i - 3]]) <-
    paste(c("Output_sim", "GDP_sim", "Income_sim", "Profit_sim"),
          2018 + (i - 3) * 3)
  
  Results_2.dat <- as.data.frame(Results_2)
  Results_2.m <- as.matrix(Results_2.dat)
  Results_2.m[is.nan(Results_2.m)] <- 0
  Results_2.m[is.infinite(Results_2.m)] <- 0
  
  #Make 5 different tables considering the same ....variables (outside looping) avoid hard-code (11->for the colnames)
}

#Combine Output, GDP, Income and Profit into 1 table
Results_2.dat<-as.data.frame(Results_2.m)
Final_table<-cbind(sector,Results.dat[,6:9],Results_2.dat) # AE: this is hardcoded

#Combine results of year 2018 and all those simulation
all_tables<- cbind(Results.dat,Results_2.dat)

#Categorize the tables based on its GDP
GDP_tables<-cbind.data.frame(Final_table[,1],Final_table[,5],Final_table[,9],Final_table[,13],Final_table[,17],Final_table[,21],Final_table[,25],
                             Final_table[,29],Final_table[,33],Final_table[,37],Final_table[,41],Final_table[,45])   #hard coded
colnames(GDP_tables)<- c("Sector","GDP_2018","GDP_2021","GDP_2024","GDP_2027","GDP_2030","GDP_2033","GDP_2036","GDP_2039","GDP_2042","GDP_2045","GDP_2048")
Baseline_all<-colSums(GDP_tables[,2:12])
#write.csv(GDP_tables,"GDP_tables.csv")

#Plot GDP ->LINE CHART
GDP_tot<-colSums(GDP_tables[,2:12])
Year<- c(2018,2021,2024,2027,2030,2033,2036,2039,2042,2045,2048)
GDP_plot<- cbind(Year,GDP_tot)
GDP_plot<-as.data.frame(GDP_plot)
GDP_graph<-ggplot(GDP_plot,aes(x=Year, y=GDP_tot))+geom_line(color="red")+geom_point()+xlab("Year")+
  ylab("Rupiah")+labs(title="Proyeksi GDP Papua Barat")


#Percentage of economic growth based on GDP rate yoy   (Do looping!)
GDP_tot.t<-t(GDP_tot)
growth_GDP<-NULL
for (i in 2:ncol(GDP_tot.t)){
  growth_year<-((GDP_tot.t[i]-GDP_tot.t[i-1])/GDP_tot.t[i-1])
  growth_GDP[[i-1]]=growth_year
  growth_GDP[[i-1]]<-cbind(growth_year)
  growth_GDP.dat<-as.data.frame(growth_GDP)
}

growth_avg<-colMeans(growth_GDP.dat)
all_growth.rate<- cbind.data.frame(year_of_year,growth_GDP.dat)
colnames(all_growth.rate)<-c("Tahun", "Pertumbuhan_GDP")

#Make a bar chart based on the GDP growth rate over year
growth_rate.graph<-ggplot(data=all_growth.rate,aes(x=Tahun, y=Pertumbuhan_GDP))+geom_bar(color="red", stat = "identity")+
  xlab("Year")+ylab("(%)")+labs(title="Pertumbuhan ekonomi Papua Barat (GDP)")

#Calculate economic growth using GDP data only for non land-based sector
#Subset GDP non land-based sector
#nlb_sector<-GDP_tables[18:35,2]

#Calculate average economic growth rate of non land-based only (%) based on GDP data for the past 5 years (2014-2018)  #do looping! 
nlb_rate<-NULL
for (i in 3:ncol(non_lb)) {
  growth_nlb.sec= ((non_lb[,i]-non_lb[,i-1])/non_lb[,i-1])
  nlb_rate[[i-2]]=growth_nlb.sec
  nlb_rate[[i-2]]<-cbind(growth_nlb.sec)
  colnames(nlb_rate[[i-2]])<- paste(c("Linear Rate_"),2013+(i-2)*1,sep="")
  nlb_rate.dat<-as.data.frame(nlb_rate)
}

nlb_rate.avg<-rowMeans(nlb_rate.dat)
nlb_rate.fin<-cbind(sector_nlb,nlb_rate.avg)

#Projection of GDP growth for non land-based sector using linear rate
#By projecting only non lan-based sector from year 2018 to 2048 using linear rate per sector
initial_GDP<-as.data.frame(nlb_sector)

#GDP growth non land based sector for category of industri pengolahan (industry) #read evernote regarding this case
rate_lin.ind= 0.03874149
sector_ind<-initial_GDP[1:7,]
growth_lin.ind<-NULL

for(i in 1:11) {
  growth_ind= sector_ind+(sector_ind*(rate_lin.ind*(i-1)*3))
  growth_lin.ind[[i]]<-cbind(growth_ind)
  colnames(growth_lin.ind[[i]])<- paste(c("GDP_"),2018+(i-1)*3,sep="")
  lin_ind.dat<-as.data.frame(growth_lin.ind)
}

#GDP growth non land based sector for category of listik, air, gas dan limbah
rate_lin.elec=0.04577286
sector_elec<-116735.99
growth_lin.elec<-NULL
for (i in 1:11) {
  Lin_electricity=sector_elec+(sector_elec*(rate_lin.elec*(i-1)*3))
  growth_lin.elec[[i]]<-cbind(Lin_electricity)
  Lin_elec2<-t(growth_lin.elec)
  lin_elec.dat<-as.data.frame(Lin_elec2)
}
colnames(lin_elec.dat)<-colnames(lin_ind.dat)

#GDP growth non land based sector for category of construction
rate_lin.cons= 0.09658640
sector_cons=12259348.66
growth_lin.cons<-NULL
for (i in 1:11) {
  Lin_cons=sector_cons+(sector_cons*(rate_lin.cons*(i-1)*3))
  growth_lin.cons[[i]]<-cbind(Lin_cons)
  lin_cons2<-t(growth_lin.cons)
  lin_cons.dat<-as.data.frame(lin_cons2)
}
colnames(lin_cons.dat)<-colnames(lin_ind.dat)

#GDP growth non land based sector for category of trade and acommodation
rate_lin.trade= 0.08272546
sector_trade<-initial_GDP[10:12,]
growth_lin.trade<-NULL
for (i in 1:11) {
  Lin_trade=sector_trade+(sector_trade*(rate_lin.trade*(i-1)*3))
  growth_lin.trade[[i]]<-cbind(Lin_trade)
  colnames(growth_lin.trade[[i]])<- paste(c("GDP_"),2018+(i-1)*3,sep="")
  lin_trade.dat<-as.data.frame(growth_lin.trade)
}

#GDP growth non land based sector for category of transportation and communication
rate_lin.trans= 	0.09097349
sector_trans<-initial_GDP[13:14,]
growth_lin.trans<-NULL
for (i in 1:11) {
  Lin_trans=sector_trans+(sector_trans*(rate_lin.trans*(i-1)*3))
  growth_lin.trans[[i]]<-cbind(Lin_trans)
  colnames(growth_lin.trans[[i]])<- paste(c("GDP_"),2018+(i-1)*3,sep="")
  lin_trans.dat<-as.data.frame(growth_lin.trans)
}

#GDP growth non land based sector for category of finance and insurance
rate_lin.finance= 	0.06910581
sector_finance<-initial_GDP[15:16,]
growth_lin.finance<-NULL
for (i in 1:11) {
  Lin_finance=sector_finance+(sector_finance*(rate_lin.finance*(i-1)*3))
  growth_lin.finance[[i]]<-cbind(Lin_finance)
  colnames(growth_lin.finance[[i]])<- paste(c("GDP_"),2018+(i-1)*3,sep="")
  lin_finance.dat<-as.data.frame(growth_lin.finance)
}

#GDP growth non land based sector for category of other services
rate_lin.other= 	0.07314356
sector_other<-initial_GDP[17:18,]
growth_lin.other<-NULL
for (i in 1:11) {
  Lin_other=sector_other+(sector_other*(rate_lin.other*(i-1)*3))
  growth_lin.other[[i]]<-cbind(Lin_other)
  colnames(growth_lin.other[[i]])<- paste(c("GDP_"),2018+(i-1)*3,sep="")
  lin_other.dat<-as.data.frame(growth_lin.other)
}

nlb_growth.lin<-rbind.data.frame(GDP_tables[1:17,2:12],lin_ind.dat,lin_elec.dat,
                                 lin_cons.dat,lin_trade.dat,lin_trans.dat,lin_finance.dat,lin_other.dat)
nlb_growth.lin2<-cbind.data.frame(sector,nlb_growth.lin)
#write.csv(nlb_growth.lin2,"GDP_nlb.csv")

#d.Plot the GDP growth using linear rates
Linear_all<-colSums(nlb_growth.lin)
Linear_all.fin<-cbind.data.frame(Year,Linear_all)
linear_nlb.graph<-ggplot(data=Linear_all.fin,aes(x=Year, y=Linear_all))+geom_line(color="red", stat = "identity")+geom_point()+
  xlab("Year")+ylab("Rupiah")+labs(title="GDP Papua Barat dengan Linear Rate")


#Calculate GDP growth using gradual rate (Do looping!)
#a. Calculate the gradual rate from 2015-2018

nlb_grad<-NULL
for (i in 2:ncol(nlb_rate.dat)) {
  growth_grad.nlb= ((nlb_rate.dat[,i]-nlb_rate.dat[,i-1])/nlb_rate.dat[,i-1])
  nlb_grad[[i-1]]=growth_grad.nlb
  nlb_grad[[i-1]]<-cbind(growth_grad.nlb)
  colnames(nlb_grad[[i-1]])<- paste(c("Gradual Rate_"),2015+(i-2)*1,sep="")
  nlb_grad.dat<-as.data.frame(nlb_grad)
}

nlb_grad.avg<-rowMeans(nlb_grad.dat)
nlb_grad.fin<-cbind(sector_nlb,nlb_grad.avg)

#Cannot be used due to fluctuative trend of graduate rate results


#b.Calculate the changes of GDP rate using gradual rate (1,5%)
#initial_rate<-nlb_2018
#rate_next.year<-initial_rate+(initial_rate*grad_rate)
#GDP_grad<-rate_next.year
#for (i in 1:29) {
#rate_next.year<-rate_next.year+(rate_next.year*grad_rate)
#GDP_grad<-rbind(GDP_grad,rate_next.year)
#rownames(GDP_grad[[i]])<- paste(c("GDP_"),2018+(i),sep="")

#}

#c. Projects the GDP using different gradual rates each year
#Grad_res.2021=nlb_sector[,2]+((nlb_sector[,2])*(rate_grad[2,2]))
#Grad_res.2024=nlb_sector[,3]+((nlb_sector[,3])*(rate_grad[3,2]))
#Grad_res.2027=nlb_sector[,4]+((nlb_sector[,4])*(rate_grad[4,2]))
#Grad_res.2030=nlb_sector[,5]+((nlb_sector[,5])*(rate_grad[5,2]))
#Grad_res.2033=nlb_sector[,6]+((nlb_sector[,6])*(rate_grad[6,2]))
#Grad_res.2036=nlb_sector[,7]+((nlb_sector[,7])*(rate_grad[7,2]))
#Grad_res.2039=nlb_sector[,8]+((nlb_sector[,8])*(rate_grad[8,2]))
#Grad_res.2042=nlb_sector[,9]+((nlb_sector[,9])*(rate_grad[9,2]))
#Grad_res.2045=nlb_sector[,10]+((nlb_sector[,10])*(rate_grad[10,2]))
#Grad_res.2048=nlb_sector[,11]+((nlb_sector[,11])*(rate_grad[11,2]))

#Grad_res<-cbind.data.frame(sector[18:35,1],nlb_sector[,1],Grad_res.2021,Grad_res.2024,Grad_res.2027,Grad_res.2030,
# Grad_res.2033,Grad_res.2036,Grad_res.2039,Grad_res.2042,Grad_res.2045,Grad_res.2048)
#colnames(Grad_res)<- c("Sector","GDP_2018","GDP_2021","GDP_2024","GDP_2027","GDP_2030","GDP_2033","GDP_2036","GDP_2039",
#"GDP_2042","GDP_2045","GDP_2048")
#Grad_all<-rbind(GDP_LR,Grad_res)

#d. Plot the gradual changes of GDP
#Gradual_all.1<-colSums(Grad_all[,2:12])
#Grad_all.fin<-cbind.data.frame(Years,Gradual_all.1)
#grad_nlb.graph<-ggplot(data=Grad_all.fin,aes(x=Years, y=Gradual_all.1))+geom_line(color="red", stat = "identity")+geom_point()+
#xlab("Year")+ylab("Rupiah")+labs(title="GDP Papua Barat dengan Gradual Rate")


#Plot baseline and linear graphs into one
GDP_all<-cbind.data.frame(Year,Baseline_all,Linear_all)
colnames(GDP_all)<-c("Year","GDP_baseline","GDP_linear")
#a and b below show the same results, for my own purpose I wrote these scripts
#a
all_GDP.graph<- ggplot(GDP_all, aes(x=Year)) + xlab("Year") + ylab("GDP dalam Rupiah")+ labs (title ="GDP Papua Barat")+
  geom_line(aes(y = GDP_baseline), color = "red") + geom_point(aes(y = GDP_baseline), color = "red")+
  geom_line(aes(y = GDP_linear), color="blue") + geom_point(aes(y = GDP_linear), color="blue")

#b
GDP_papbar<-ggplot(data = GDP_all, aes(x = Year)) +
  geom_line(aes(y = GDP_baseline, colour = "GDP_baseline")) + geom_point(aes(y = GDP_baseline, colour = "GDP_baseline"))+
  geom_line(aes(y = GDP_linear, colour = "GDP_linear")) + geom_point(aes(y = GDP_linear, colour = "GDP_linear"))+
  scale_colour_manual("", values = c("GDP_baseline"="green", "GDP_linear"="red")) +
  xlab("Year") +scale_y_continuous("GDP dalam Rupiah", limits = c(0,1000000000)) + labs(title="GDP Papua Barat")


#Adding an intervention of "Pembangunan Industri Hilir (PIH)" into the previous IO

#1. CALCULATE INVERS LEONTIEF for PIH Intervention
int_dem.hilir2 <- as.matrix(int_dem.hilir)
add_val.hilir2 <- as.matrix(add_val.hilir)
dim2 <- ncol(int_dem.hilir2)
col_int.hilir <- colSums(int_dem.hilir2)
col_add.hilir <- colSums(add_val.hilir2)
fin_hilir <- 1 / (col_int.hilir + col_add.hilir)
fin_hilir[is.infinite(fin_hilir)] <- 0
invers_fin.hilir <- diag(fin_hilir)
A_hilir <- int_dem.hilir2 %*% invers_fin.hilir
I_hilir <- as.matrix(diag(dim2))
I_A.hilir <- I_hilir - A_hilir
Leontief_hilir <- solve(I_A.hilir)

#2.Projection of GDP using intervention of PIH IO (2018-2048)

#a. Calculation for year 2018
tot_fin.dem <- rowSums(fin_demand)
output_hilir <- tot_fin.dem + rowSums(int_dem.hilir)
tot_add.hilir <- colSums(add_val.hilir)
diag_land.cov <- diag(unlist(final_land.cov[, 3])) # AE: What's the purpose in having "unlist" here?  -> without using unlist, we couldn't see the results
land_req <- land_dist.m %*% diag_land.cov
LR <- rowSums(land_req)
prop_GDP.h <- tot_add.hilir / output_hilir
prop_inc.h <- add_val.hilir[1, ] / output_hilir
prop_profit.h <- add_val.hilir[2, ] / output_hilir
FD_prop.h <- tot_fin.dem / output_hilir
LRC.h <- LR / output_hilir
LPC.h <- output_hilir / LR
GDP.h <- prop_GDP.h * output_hilir

#transpose
Income.h<- t(prop_inc.h)*output_hilir
Profit.h<- t(prop_profit.h)*output_hilir

#Present the results of LR, LC, LPC in the different table
Calculation_h<-cbind(tot_fin.dem,FD_prop.h,LR,LRC.h,LPC.h)
Results.h<-cbind(output_hilir,GDP.h,Income.h,Profit.h)
Results.h[,][is.nan(Results.h[,])] <- 0
Results.h[,][is.infinite(Results.h[,])] <- 0
colnames(Results.h)<- c("Output_sim.2018","GDP_sim.2018","Income_sim.2018","Profit_sim.2018")
Results.h_dat<-as.data.frame(Results.h)


#b. Looping Simulation from year 2021-2048
final_table.h <- NULL
Results_2.h <- NULL
initial_year2 <- 2018
for(i in 4:ncol(final_land.cov)) {
  
  diag_lc.h <- diag(unlist(final_land.cov[, i]))
  land_req.h <- land_dist.m %*% diag_lc.h
  LR2.h <- rowSums(land_req.h)
  FD_sim.h <- (LR2.h * LPC.h) * FD_prop.h
  
  FD_sim.h[is.nan(FD_sim.h)] <- 0      #changes
  Output_sim.h <- Leontief_hilir %*% FD_sim.h
  
  GDP_sim.h <- Output_sim.h * prop_GDP.h
  Income_sim.h <- t(prop_inc.h) * Output_sim.h
  Profit_sim.h <- t(prop_profit.h) * Output_sim.h
  Results_2.h[[i - 3]] <- cbind(Output_sim.h, GDP_sim.h, Income_sim.h, Profit_sim.h)
  
  names(Results_2.h[[i - 3]]) <- c("Output_Sim", "GDP_sim", "Income_sim", "Profit_sim")
  colnames(Results_2.h[[i - 3]]) <- paste(c("Output_sim", "GDP_sim", "Income_sim", "Profit_sim"), 2018 + (i - 3) * 3)
  
  Results_h.dat <- as.data.frame(Results_2.h)
  Results_h.m <- as.matrix(Results_h.dat)
  Results_h.m[is.nan(Results_h.m)] <- 0
  Results_h.m[is.infinite(Results_h.m)] <- 0
  
}

#3. a. Plot the GDP results from the intervention

#Combine Output, GDP, Income and Profit into 1 table
Results_hilir.dat<-as.data.frame(Results_h.m)
Fin_table.hilir<-cbind(Results.h_dat,Results_hilir.dat) 

#Combine results of year 2018 and all those simulation (including final demand each year, LR, LC, LPC results)
all_tables.h<- cbind(sector,Calculation_h,Fin_table.hilir)


#cbind GDP only from 2018-2048
GDP_h<- cbind.data.frame(Fin_table.hilir$GDP_sim.2018,Fin_table.hilir$GDP_sim.2021,Fin_table.hilir$GDP_sim.2024,Fin_table.hilir$GDP_sim.2027,Fin_table.hilir$GDP_sim.2030,
                         Fin_table.hilir$GDP_sim.2033,Fin_table.hilir$GDP_sim.2036,Fin_table.hilir$GDP_sim.2039,Fin_table.hilir$GDP_sim.2042,
                         Fin_table.hilir$GDP_sim.2045,Fin_table.hilir$GDP_sim.2048)
GDP_hilir<-colSums(GDP_h)

#Plot GDP ->LINE CHART
GDP_plot.h<- cbind(Year,GDP_hilir)
GDP_h.dat<-as.data.frame(GDP_plot.h)
GDP_graph.h<-ggplot(GDP_h.dat,aes(x=Year, y=GDP_hilir))+geom_line(color="red")+geom_point()+xlab("Year")+
  ylab("Rupiah")+labs(title="Proyeksi GDP Papua Barat dengan Intervensi Hilir")


#3.b. Combine 3 different plot of GDP (baseline, linear and hilir)

GDP_all.3<-cbind.data.frame(Year,Baseline_all,Linear_all,GDP_hilir)
colnames(GDP_all.3)<-c("Year","GDP_baseline","GDP_linear","GDP_hilir")

Comb_GDP.3<-ggplot(data = GDP_all.3, aes(x = Year)) +
  geom_line(aes(y = GDP_baseline, colour = "GDP_baseline")) + geom_point(aes(y = GDP_baseline, colour = "GDP_baseline"))+
  geom_line(aes(y = GDP_linear, colour = "GDP_linear")) + geom_point(aes(y = GDP_linear, colour = "GDP_linear"))+
  geom_line(aes(y = GDP_baseline, colour = "GDP_hilir")) + geom_point(aes(y = GDP_baseline, colour = "GDP_hilir"))+
  scale_colour_manual("", values = c("GDP_baseline"="green", "GDP_linear"="red","GDP_hilir"="blue")) +
  xlab("Year") +scale_y_continuous("GDP dalam Rupiah", limits = c(0,1000000000)) + labs(title="GDP Papua Barat")

#Since the GDP results of hilir and baseline are quite similar, in the presented graph above, they looked alike and overlap one to another
#adding a line
#adding a new line again
