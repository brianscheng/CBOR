#Data clean up and writing
#started 2/16/20

require(here)
require(tidyr)
require(ggplot2)
require(openxlsx)
require(dplyr)
require(lubridate)

#Growth/size-------------------------------------------------------
size<-read.csv(here('data','CBOR_closedtreat_oystersize_original.csv'))

#getting rid of the notes column
size<-size[-c(13)]

#getting rid of rows with NAs
size <- drop_na(size)

#creating a column of growth for each oyster (Size after experiment - initial size)
size$growth_cm2<-size$Size_cm2-size$size_before_cm2

write.csv(size, file = 'CBOR_closedtreat_oystersize_clean_2020.csv') #write data into file, to be used in all subsequent analyses


#Oyster drill abundance data-----------------------------------------
drill<-read.csv(here('data', 'Benthic survey Richardson Bay COUNTS 11 Sept 2018 cjz.csv'))

drill$Year<-factor(drill$Year, levels = c('2017','2018')) #making sure that data is in the correct format

#Catch per unit effort (CPUE)------------------------------------------------
cpue<-read.csv(here('data','CBOR_cpue_original_2020.csv'))

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


#Oyster Drill Size Frequency------------------------------------------
data<-read.csv(here('data', 'CBOR_quadrat_OysterDrill_size.csv'))

head(data)
str(data)
#making sure that data is in the right format
data$Site<-as.factor(data$Site)
data$year<-as.factor(data$year)

#making a new column for treatment: control or eradication
data$treatment[data$Site=='Aramburu Central']<-'eradication'
data$treatment[data$Site=='Lanis Beach']<-'eradication'
data$treatment[data$Site=='Lanis Boulders']<-'eradication'
data$treatment[data$Site=='Aramburu South']<-'control'
data$treatment[data$Site=='Aramburu South']<-'control'
data$treatment[data$Site=='Blackies']<-'control'
data$treatment[data$Site=='Bothin']<-'control'
data$treatment[data$Site=='Brickyard Cove']<-'control'
data$treatment[data$Site=='Cove Apts']<-'control'
data$treatment[data$Site=='Cove']<-'control'
data$treatment[data$Site=='Dunphy Park']<-'control'
data$treatment[data$Site=='Hilarita']<-'control'
data$treatment[data$Site=='pink House']<-'control'
data$treatment<-as.factor(data$treatment) #making sure that treatment column is a factor
str(data)
sum(is.na(data$treatment)) #Making sure I got every row

write.csv(data, file = 'CBOR_OysterDrill_sizefequency_clean_2020.csv')
