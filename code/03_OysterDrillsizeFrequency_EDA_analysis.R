##Efficacy of functional eradication over time (size frequency) EDA and analysis
#Jordanna Barley
#Updated 6/2/20

require(here)
require(tidyverse)

data<-read.csv(here('data', 'CBOR_OysterDrill_sizefequency_clean_2020.csv'))

#Plots----------------------------------------------------

#histogram of distribution by site
data %>% 
  ggplot(aes(x=size))+
  geom_histogram()+
  facet_wrap(~Site) #there seems to be a bimodal distribution at almost every site

data %>% 
  ggplot(aes(x=size))+
  geom_histogram()+
  facet_wrap(~treatment) #same bimodal distribution

data %>% 
  ggplot(aes(x=day_of_year, y=size))+
  geom_point(alpha=0.5, position = position_jitter())+
  stat_summary(fun.args = list(mult=1),geom='pointrange', size=0.75, col='red')+
  facet_wrap(~year)+
  ylab('Oyster drill size')+
  xlab('Day of Year') #2017: can maybe see a decrease in oyster drill size

data %>% 
  ggplot(aes(x=day_of_year, y=size, color=factor(year)))+
  geom_point(alpha=0.5, position = position_jitter())+
  stat_summary(fun.args = list(mult=1),geom='pointrange', size=0.75)+
  geom_smooth(method = 'lm')+
  facet_wrap(~treatment)+
  ylab('Oyster drill size (mm)')+
  xlab('Day of Year') 

data %>% 
  ggplot(aes(x=day_of_year, y=size, color=treatment))+
  geom_point(alpha=0.5, position=position_jitter())+
  stat_summary(fun.args = list(mult=1), geom='pointrange', size=0.75)+
  geom_smooth(method = 'lm')+
  facet_wrap(~year)+
  ylab('Oyster drill size (mm)')+
  xlab('Day of Year') #it seems likely that size in 2017 did decrease, but not because of treatment. size at erdication site seems to have increased in 2018. 


#Models------------------------------------------------















