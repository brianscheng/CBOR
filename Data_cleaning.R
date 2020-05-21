#Data clean up and writing
#started 2/16/20

require(here)
require(tidyr)
require(ggplot2)
require(openxlsx)
require(dplyr)
require(lubridate)

#Growth/size-------------------------------------------------------
size<-read.csv(here('CBOR_closedtreat_oystersize_original.csv'))

#getting rid of the notes column
size<-size[-c(13)]

#making sure all variables are in the correct data type
size$Size_cm2<-as.numeric(as.character(size$Size_cm2)) #have to use as.character first because of how things are internally stored
size$size_before_cm2<-as.numeric(as.character(size$size_before_cm2)) 
size$Site<-factor(size$Site, levels = c('Lanis','Cove','A-south','A-central'))
size$Initial.oys<-as.numeric(size$Initial.oys)
size$Alive<-as.numeric(size$Alive)
size$Dead<-as.numeric(size$Dead)
size$Height<-factor(size$Height, levels=c('0.5','1.5','2.5'))

#getting rid of rows with NAs
size <- drop_na(size)

#creating a column of growth for each oyster (Size after experiment - initial size)
size$growth_cm2<-size$Size_cm2-size$size_before_cm2

write.csv(size, file = 'CBOR_closedtreat_oystersize_clean_2020.csv') #write data into file, to be used in all subsequent analyses


#Oyster drill abundance data-----------------------------------------
drill<-read.csv(here('Benthic survey Richardson Bay COUNTS 11 Sept 2018 cjz.csv'))

drill$Year<-factor(drill$Year, levels = c('2017','2018')) #making sure that data is in the correct format

#Catch per unit effort (CPUE)------------------------------------------------
cpue<-read.csv(here('CBOR_cpue_original_2020.csv'))

cpue<-select(cpue, -c(12:14)) #getting rid of extra columns
cpue<-drop_na(cpue) #getting rid of NAs

#making sure that year is a factor
cpue$year<-factor(cpue$year, levels = c('2017','2018'))

#making another column for continuous date
cpue$date2<-as.Date(cpue$Date)

#writing CPUE final data final for analyses
write.csv(cpue, file = 'CBOR_cpue_clean_2020.csv')

#changing 4/22/2018 to 4/21/2018, so that mean is calculated for all drills caught over oth days as on erad event
head(cpue)
cpue$Date[cpue$Date=='2018-04-22']<-'2018-04-21'
cpue$Date<-droplevels(cpue$Date)

#survival data- Brian's code------------------------------------------
#data<-read.csv("Cage experiment recovery data 2018-08-13 RAW.csv")

