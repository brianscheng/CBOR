##Catch per unit effort (CPUE) Data exploration and analysis
#Jordanna Barley
#Updated May 21, 2020

require(glmmTMB)
require(AICcmodavg)
require(pscl)
require(MASS)
require(MuMIn)
require(car)
require(ggplot2)
require(lme4)
require(emmeans)
require(brglm)
require(multcomp)
require(viridis)
require(dplyr)
require(pscl)
require(car)
require(here)

#reading in clean data
cpue<-read.csv(here('data/CBOR_cpue_clean_2020.csv'), stringsAsFactors = T)

#Initial Look----------------------------------------------------------------
#An initial look at the data shows that many of the counts that are less than 30 are members of our team:
out<-filter(cpue, drills.caught<25)
out<-data.frame(
  'Fisher'=out$Fisher,
  'drills.caught'=out$drills.caught,
  'CPUE'=out$CPUE
)
out
#Therefore, those data points were not included in the analysis
cpue<-filter(cpue, drills.caught>25)

#making sure that data is in the right format
str(cpue)
cpue$year<-factor(cpue$year)


#Data exploration-----------------------------------------------------------

cpue.plot<-ggplot(cpue, aes(x=Date, y=CPUE)) #initial look at barchart of CPUE
cpue.plot+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  facet_wrap(~Site)+
  ylab('Catch per unit effort')

cpue.plot2<-ggplot(cpue, aes(x=day_of_year, y=CPUE, color=year)) #looking at CPUE across year and site
cpue.plot2+geom_point(alpha=0.4, size=2)+
  stat_summary(fun.args = list(mult=1),geom='pointrange', size=0.75)+
  facet_wrap(~Site)+
  theme_linedraw()+
  theme(text=element_text(size=20))+
  ylab('Catch per Unit Effort')+
  xlab('Day of Year')

#MS plot - Aramburu on left, lanis on right
A_col <- "#2D708EFF"
B_col <- "#3CBB75FF"

cpue.plot3<-ggplot(cpue, aes(x=day_of_year, y=CPUE, color=year)) #looking at CPUE across year and site
plot<-cpue.plot3+geom_point(alpha=0.4, size=4)+
  stat_summary(fun.args = list(mult=1),geom='pointrange', size=1)+
  facet_wrap(~Site)+
  theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=16), axis.title = element_text(size = 18),
        strip.background = element_blank(),
        legend.title = element_text (size = 18),
        legend.text = element_text (size = 16))+
  scale_y_continuous(breaks = c(0,50, 100, 150, 200, 250, 300))+
  coord_cartesian(ylim = c(0,300))+
  ylab('Catch per unit effort')+
  xlab('Day of year')+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) 
plot

ppi=300
png("figures/cpue.png", width=9*ppi, height=6*ppi, res=ppi)
plot
dev.off()
#Models----------------------------------------------------------------------

#started with CLMs with poisson errors
cpue.null<-glm(drills.caught~1, data = cpue, offset = Person.hrs, family = 'poisson')
cpue.moda<-glm(drills.caught~day_of_year, data=cpue, offset = Person.hrs, family = 'poisson')
cpue.modb<-glm(drills.caught~year, data=cpue, offset = Person.hrs, family = 'poisson')
cpue.modc<-glm(drills.caught~day_of_year+year, data = cpue, offset = Person.hrs, family = 'poisson')
cpue.modd<-glm(drills.caught~day_of_year*year, data = cpue, offset = Person.hrs, family = 'poisson')
cpue.mode<-glm(drills.caught~Site, data = cpue, offset = Person.hrs, family = 'poisson')
cpue.modf<-glm(drills.caught~Site+day_of_year+year, data = cpue, offset = Person.hrs, family = 'poisson')
cpue.modg<-glm(drills.caught~Site+day_of_year*year, data = cpue, offset = Person.hrs, family = 'poisson') #top model
model.sel(cpue.null, cpue.moda, cpue.modb, cpue.modc, cpue.modd, cpue.mode, cpue.modf, cpue.modg)

#Overdispersion check on top model
dev<-summary(cpue.modg)$deviance
dof<-summary(cpue.modg)$df.residual
1-pchisq(dev,dof)#chi-squared test; want >0.05
dev/dof #dispersion parameter; want ~1
#these tests indicate that the top model is overdispersed

#looking at Negative binomial GLMs and GLMMs

cpue.null<-glm.nb(drills.caught~1, data = cpue, offset(Person.hrs))
cpue.moda<-glm.nb(drills.caught~day_of_year, data=cpue, offset(Person.hrs))
cpue.modb<-glm.nb(drills.caught~year, data=cpue, offset(Person.hrs))
cpue.modc<-glm.nb(drills.caught~day_of_year+year, data = cpue, offset(Person.hrs))
cpue.modd<-glm.nb(drills.caught~day_of_year*year, data = cpue, offset(Person.hrs))
cpue.mode<-glm.nb(drills.caught~Site, data = cpue, offset(Person.hrs))
cpue.modf<-glm.nb(drills.caught~Site+day_of_year+year, data = cpue,  offset(Person.hrs))
cpue.modg<-glm.nb(drills.caught~Site+day_of_year*year, data = cpue,  offset(Person.hrs))
cpue.glmm<-glmmTMB(drills.caught~day_of_year+(1|Site)+(1|year), data=cpue, offset=Person.hrs, family = nbinom1)
cpue.glmm2<-glmmTMB(drills.caught~day_of_year+(1|Site)+(1|year), data=cpue, offset=Person.hrs, family = nbinom2)
#cpue.glmm3<-glmmTMB(drills.caught~day_of_year*year+(1|Site)+(1|year), data=cpue, offset=Person.hrs, family = nbinom1) error, not able to run
cpue.glmm4<-glmmTMB(drills.caught~day_of_year*year+(1|Site)+(1|year), data=cpue, offset=Person.hrs, family = nbinom2)
test.cpue<-glmmTMB(drills.caught~day_of_year*Site+(1|year), data=cpue, offset=Person.hrs, family = nbinom2) #model we are actually interested in
test2.cpue<-glmmTMB(drills.caught~Site+(1|year), data=cpue, offset=Person.hrs, family = nbinom2) #AIC winner
#model selection
model.sel(test2.cpue, test.cpue,cpue.null, cpue.moda, 
          cpue.modb, cpue.modc, cpue.modd, cpue.mode, 
          cpue.modf, cpue.modg, cpue.glmm, cpue.glmm2, cpue.glmm4)

summary(cpue.glmm2)
summary(test.cpue)
summary(test2.cpue)
#none of these models show significant effects

#looking at differences in site
site1<-glm.nb(drills.caught~Site, offset(Person.hrs), data = cpue)
summary(site1)
#seems to be a difference in cpue across site


#brians modeling
m0<-glmmTMB(drills.caught~1+(1|Site)+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)
m1<-glmmTMB(drills.caught~day_of_year+(1|Site)+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)
m2<-glmmTMB(drills.caught~day_of_year+Site+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)
m3<-glmmTMB(drills.caught~day_of_year*Site+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)

m4<-glmmTMB(drills.caught~day_of_year*year+(1|Site), data = cpue, offset = log(Person.hrs), family = nbinom2)
m5<-glmmTMB(drills.caught~day_of_year*year+(1|Site), data = cpue, offset = log(Person.hrs), family = nbinom1)
m6<-glmmTMB(drills.caught~day_of_year*year+(1|Site), data = cpue, offset = log(Person.hrs), family = poisson)

m7<-glmmTMB(drills.caught~day_of_year+(1|Site)+(1|year), data = cpue, offset = log(Person.hrs), family = poisson)
m8<-glmmTMB(drills.caught~day_of_year+(1|Site)+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)
m9<-glmmTMB(drills.caught~day_of_year*Site+(1|year), data = cpue, offset = log(Person.hrs), family = nbinom2)

all.models<-model.sel(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9) 
all.models

summary(model.avg(all.models, revised.var=FALSE))

summary(m0)
summary(m1)
summary(m2) #no effect of day of year
summary(m3) #full model
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9) #model reported
