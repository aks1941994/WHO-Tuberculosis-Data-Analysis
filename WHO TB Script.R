#Calling Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(basicTrendline)

#Changing Working Directory
setwd('E:/PostGrad/UW Winter Term/MSCI 718/Assignments/Assignment 2/Datasets')
dir()

#Loading the csv data into R data frame
comm_eng <- read_csv('TB_community_engagement_2021-02-10.csv')
cases_noti <- read_csv('TB_notifications_2021-02-10.csv')

#Data Wrangling
test_data1 <- select(cases_noti, c_newinc, country,year) 
test_data2 <- select(comm_eng, bmu,country, year)
test_data1 <- filter(test_data1, between(year,2013,2019) )
test_data2 <- filter(test_data2, between(year,2013,2019) )
test_data1 <- remove_missing(test_data1)
test_data2 <- remove_missing(test_data2)
test_data1 <- rename(test_data1,'New_RelapseCases' = 'c_newinc' )
test_data2 <- rename(test_data2,'TB_MgmtUnits' = 'bmu' )
test_data1 <- arrange(test_data1, country, year)
test_data2 <- arrange(test_data2, country, year)
test_data1
test_data2
test_data_combined <- c()
test_data_combined <- merge(test_data1, test_data2, by.test_data1 = "Country",
                            by.test_data2 = "Country")
test_data_combined

#Describing the Datasets
summary(test_data_combined$New_RelapseCases)
summary(test_data_combined$TB_MgmtUnits)
sd(test_data_combined$New_RelapseCases)
sd(test_data_combined$TB_MgmtUnits)

#Mapping the plots with datasets
ggplot()+
  geom_histogram(aes(New_RelapseCases), test_data_combined, bins = 5 , colour = 'grey') + 
  labs(title="New and Relapse Cases of TB")
ggplot() + geom_jitter(aes(year,New_RelapseCases), test_data_combined)
ggplot()+
  geom_histogram(aes(TB_MgmtUnits),test_data_combined , bins = 5 , colour = 'grey') + 
  labs(title = "Basic TB Units")
ggplot() + geom_jitter(aes(year,TB_MgmtUnits), test_data_combined)
trendline(test_data_combined$New_RelapseCases, test_data_combined$TB_MgmtUnits, model = "line2P", plot = TRUE, linecolor = "red",
          lty = 1, lwd = 1, summary = TRUE, eDigit = 5,
          eSize = 1)
#Checking Normality

shapiro.test(test_data_combined$New_RelapseCases)
shapiro.test(test_data_combined$TB_MgmtUnits)


#Testing the data for correlation

Corr_Test <- cor.test(test_data_combined$New_RelapseCases, test_data_combined$TB_MgmtUnits, 
                      method = c("pearson"))
Corr_Test
