#Papua Barat

#=== Testing changes AE


library(XML)
library(reshape2)
library(plyr)
library(ggplot2)

#Set working directory

#Writing project file

#Read input file
sector<- read.csv("sector.csv", header=FALSE, sep =",")
add_val<- read.csv("added_value.csv", header=FALSE, sep =",")
labour<- read.csv("Labour.csv", header=TRUE, sep =",", stringsAsFactors = FALSE)
Population<-read.csv("Population.csv",header=TRUE, sep =",", stringsAsFactors = FALSE)
age_pop<- read.csv("age_population.csv",header=TRUE, sep =",", stringsAsFactors = FALSE)


#Calculate proportion of GDP per category
#1. GDP total per category
add_val.m<-as.matrix(add_val)
tot_GDP.ag<-sum(add_val.m [1:14])
tot_GDP.mining<-sum(add_val.m [15:17])
tot_GDP.industry<-sum(add_val.m [18:24])
tot_GDP.trade<-sum(add_val.m [27:29])
tot_GDP.trans<-sum(add_val.m [30:31])
tot_GDP.services<-sum(add_val.m [33:35])

#2.GDP proportion per category considering the number of labour
prop_GDP.ag<- (add_val.m[1:14]/tot_GDP.ag)*labour$Lab[1]
prop_GDP.ag.m<- as.matrix(prop_GDP.ag)
prop_GDP.mining<- (add_val.m[15:17]/tot_GDP.mining)*labour$Lab[2]
prop_GDP.mining.m<- as.matrix(prop_GDP.mining)
prop_GDP.industry<- (add_val.m[18:24]/tot_GDP.industry)*labour$Lab[3]
prop_GDP.industry.m<-as.matrix(prop_GDP.industry)
prop_GDP.electri<-labour$Lab[4]
prop_GDP.electri.m<-as.matrix(prop_GDP.electri)
prop_GDP.cons<-labour$Lab[5]
prop_GDP.cons.m<-as.matrix(prop_GDP.cons)
prop_GDP.trade<- (add_val.m[27:29]/tot_GDP.trade)*labour$Lab[6]
prop_GDP.trade.m<-as.matrix(prop_GDP.trade)
prop_GDP.trans<- (add_val.m[30:31]/tot_GDP.trans)*labour$Lab[7]
prop_GDP.trans.m<-as.matrix(prop_GDP.trans)
prop_GDP.finance<- labour$Lab[8]
prop_GDP.finance.m<-as.matrix(prop_GDP.finance)
prop_GDP.services<- (add_val.m[33:35]/tot_GDP.services)*labour$Lab[9]
prop_GDP.services.m<-as.matrix(prop_GDP.services)


#3. Make a new table presenting the results
prop_GDP_labour.all<-rbind(prop_GDP.ag.m,prop_GDP.mining.m,prop_GDP.industry.m,prop_GDP.electri.m,
                    prop_GDP.cons.m,prop_GDP.trade.m,prop_GDP.trans.m,prop_GDP.finance.m,
                    prop_GDP.services.m)
labour_all<-cbind(sector,prop_GDP_labour.all)
colnames(labour_all)[1] <- "SECTOR"
colnames(labour_all)[2] <- "CATEGORY"
colnames(labour_all)[3] <- "ENGLISH"
colnames(labour_all)[4] <- "Number of Labour"


#Calculate rate of population growth using data from 2014-2018
rate_pop<-abs(log(Population$Population[1]/Population$Population[5],base = exp(1))*(1/5))

#Project the population growth in few years using the calculated-rate of population growth
years<- Population$Year[6:11]
Projection<- exp(rate_pop*(years-2014))*Population$Population[1]
Pop_growth.projection<- cbind(Population$Year [6:11],Projection)
colnames(Pop_growth.projection)[1] <- "YEARS"
colnames(Pop_growth.projection)[2] <- "PROJECTED POPULATIONS"


#Create a population pyramide
pyramidGH <- ggplot(age_pop, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(data = subset(age_pop, Female == "Female"), stat = "identity") + 
  geom_bar(data = subset(age_pop, Male == "Male"), stat = "identity") + 
  scale_y_continuous(labels = paste0(as.character(c(seq(2, 0, -1), seq(1, 2, 1))), "m")) + 
  coord_flip()
pyramidGH




