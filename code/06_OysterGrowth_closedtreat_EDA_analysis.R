##Oyster growth EDA and analysis
#Jordanna Barley - Updated May 22, 2020
#Brian Cheng - updated Feb 4, 2021

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
library(Hmisc)

#reading in clean data
data<-read.csv(here('data/CBOR_closedtreat_oystersize_clean_2020.csv'), stringsAsFactors = T)
str(data)
#Initial look-------------------------------------------------------
#we are only analyzing tidal height 1.5 for this analysis, filtering other tidal heights out:
data<-data %>%
  filter (Height == 1.5) %>%
  mutate (Treatment = case_when (Site =="A-south" ~ "Control",
                                 Site =="A-central" ~ "Removal",
                                 Site =="Lanis" ~ "Removal",
                                 Site =="Cove" ~ "Control"))%>%
  mutate (Treatment = factor(Treatment)) %>%
  mutate (Site = case_when (Site == "A-south" ~ "Aramburu",
                            Site == "A-central" ~ "Aramburu",
                            Site == "Lanis" ~ "Lanis Beach",
                            Site == "Cove" ~ "Lanis Beach")) %>%
  mutate (Site = factor(Site)) %>%
  mutate (growth_percent = Size_cm2/size_before_cm2*100) %>%
  mutate (Tile = factor(Tile))
str(data)

#summary stats
table(data$Site, data$Tile)
tapply(data$size_before_cm2,list(data$Site,data$Treatment), length)
tapply(data$growth_percent,data$Site, mean)

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
A_col <- "#2D708EFF"
B_col <- "#3CBB75FF"
plot<-ggplot(data, aes(x = Treatment, y=growth_cm2, fill=Treatment))+
  geom_violin()+facet_grid(.~Site)+
  stat_summary(fun.data = mean_sdl, mult=1, geom="pointrange", color = "black")+theme_bw()+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18, vjust=-0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_blank())+
  labs(y= expression(paste("Oyster growth (",cm^{2},")")))+
scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) 
plot


ppi=300
png("figures/oyster_growth.png", width=9*ppi, height=6*ppi, res=ppi)
plot
dev.off()

#visualizing tile variation
plot2<-ggplot(data, aes(x = Tile, y=growth_cm2, fill = Treatment))+
  geom_boxplot()+facet_grid(.~Site)+theme_bw()+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18, vjust=-0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y= expression(paste("Oyster growth (",cm^{2},")")))+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) 
plot2


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