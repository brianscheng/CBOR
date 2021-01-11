##Oyster initial size EDA and Analysis
#Jordanna Barley
#Updated 6/2/20

require(dplyr)
require(tidyr)
require(here)
require(ggplot2)
require(glmmTMB)
require(MuMIn)
require(car)


size<-read.csv('data/CBOR_oysterinitialsizecheck_clean_2020.csv', stringsAsFactors = TRUE)
str(size)
size$tile<-as.factor(size$tile)
levels(size$cage.treat)

#Plots-----------------------------------------------------------------
hist(size$size.before)

plot(size$size.before, col=adjustcolor(as.numeric(size$cage.treat)))

initial.hist<-ggplot(size, aes(x=size.before))
initial.hist+geom_histogram(bins= 50)+
  facet_wrap(~cage.treat)

initial<-ggplot(size, aes(x=cage.treat, y=size.before))
initial+geom_boxplot()+
  geom_point(position='jitter')+
  xlab('Cage Treatment')+
  ylab('Initial Oyster Size (cm2)')

#Models----------------------------------------------------------------

null<-glmmTMB(size.before~1+(1|tile), data = size, family = 'gaussian') #null intercept model
mod1<-glmmTMB(size.before~cage.treat+(1|tile), data = size, family = 'gaussian') #looking at the differences across treatment
mod2<-glmmTMB(size.before~cage.treat*Site+(1|tile), data = size, family = 'gaussian') #looking at the differences across treatment

summary(null)
summary(mod1)
model.sel(null, mod1) #model selection, null and model are pretty similar

#running ANOVA to test treatment effect:
Anova(mod1) #no effect of cage treatment, no need to continue with Tukey HSD




