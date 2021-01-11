##Oyster growth EDA and analysis
#Jordanna Barley
#Updated May 22, 2020

require(here)
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

#reading in clean data
size<-read.csv(here('data/CBOR_closedtreat_oystersize_clean_2020.csv'))

#Initial look-------------------------------------------------------
#we are only analyzing tidal height 1.5 for this analysis, filtering other tidal hieghts out:
size<-droplevels(filter(size, Height=='1.5'))

#making sure data is in the correct data type
str(size)
size$Size_cm2<-as.numeric(as.character(size$Size_cm2)) #have to use as.character first because of how things are internally stored
size$size_before_cm2<-as.numeric(as.character(size$size_before_cm2)) 
size$Site<-factor(size$Site, levels = c('Lanis','Cove','A-south','A-central'))
size$Initial.oys<-as.numeric(size$Initial.oys)
size$Alive<-as.numeric(size$Alive)
size$Dead<-as.numeric(size$Dead)
size$Height<-factor(size$Height)
size$Tile<-factor(size$Tile)
size$growth_percent<-size$Size_cm2/size$size_before_cm2*100

#summary stats
table(size$Site, size$Tile)
tapply(size$size_before_cm2,size$Site, length)
tapply(size$growth_percent,size$Site, mean)

#plots
hist(size$growth_cm2, main = '', xlab = 'Growth (cm2)') #distribution of growth is fairly normal

growth<-ggplot(size, aes(x=Site, y=growth_cm2))
growth+geom_point(position = (position_dodge(width=0.75)),size=2, alpha=0.5)+
  geom_boxplot()+
  theme_classic()+
  ylab('Closed treatment oyster growth (cm2)')+
  xlab('Site')
#looks like there might be a site effect, Aramburu Central oysters look like they could have grown more
#other sites seem to have very similar distributions

#Models-------------------------------------------------------------
#used GLMMs to model growth against site, with a random effect of Tile

null.size<-glmmTMB(growth_cm2~1+(1|Tile), data = size, family = 'gaussian')
mod.size<-glmmTMB(growth_cm2~Site+(1|Tile), data=size, family = 'gaussian')
model.sel(null.size, mod.size) #intercept and site models have very similar AIC values

#model validation for mod.size

summary(mod.size) #there might be a significant effect of Aramburu Central
resid<-resid(mod.size)
fitted<-fitted(mod.size)

qqPlot(size$growth_cm2, col =alpha('red', 0.4), pch = 16, ylab = 'Oyster Growth (cm2)')
hist(resid)
plot(resid~fitted) #some patterns
boxplot(resid~size$Site, col=adjustcolor(as.numeric(size$Site),0.3),
        ylab = 'Residuals',
        xlab = 'Site') #yass this is what I want
boxplot(resid~size$Tile, col=adjustcolor(as.numeric(size$Site), 0.3),
        ylab = 'Residuals',
        xlab = 'Tile Number')

#using Anova to look into potential Site effects: 
Anova(mod.size)
#There do not seem to be significant site effects overall
#did Tukeys HSD test to make sure

#pairwise comparison
aovmod.size<-aov(mod.size)
aovmod.size
#tukey's honest sig. difference
tukmod.size<-TukeyHSD(aovmod.size)
par(mar=c(5,8,4,1)+.1)
plot(tukmod.size, las=1)