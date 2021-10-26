#oyster drill eradication caging experiment analysis
#updated 2021-01-04

library(ggplot2)
library(lme4)
library(emmeans)
library(brglm)
library(multcomp)
library(car)
library(viridis)
library(dplyr)
library(here)

#data import
data<-read.csv(here('data/Cage experiment recovery data 2018-08-13 RAW.csv'), stringsAsFactors = TRUE)

#data curation
str(data)
data[117,] #missing experimental unit
data[117,17]
data[117,17]<-"yes" #exclude, it's an NA
summary(data$Exclude)
data<-filter(data,Exclude =="no")
data$Height<-factor(data$Height) #set factor for height

#variable creation
data$prop.surv<-data$Alive/data$Initial.oys #create proportional survival variable
data$Dead2<-data$Initial.oys-data$Alive #create a dead2 variable (dead+missing)data
y<-cbind(data$Alive, data$Dead2)

data.mid<-subset(data,Height ==1.5) #examine middle elevation plots only for this analysis
nrow(data.mid) #91 rows, 1 header + 90 experimental units (1 na and 6 excluded)
length(unique(data.mid$Tile)) #91 tiles
y.mid<-cbind(data.mid$Alive,data.mid$Dead2)
y.mid

#summary stats
str(data.mid)
summary(data.mid$Exclude) #in the end, 4 plots were excluded, 3 others were excluded from other tidal elevations
summary(data.mid$Initial.oys) #how many initial oysters? 8-10 oysters, mean = 9.8
sd(data.mid$Initial.oys) #SD = 0.42
sum(data.mid$Initial.oys)#896 initial oysters
tapply(data.mid$Initial.oys, data.mid$Cage.treat,sum) #oysters deployed; closed = 276, open = 305, partial = 315
tapply(data.mid$Alive, data.mid$Cage.treat,sum)       #oysters alive; closed = 246, open = 0, partial = 5
sum(data.mid$Drilled) #how many drilled? 275
summary(data.mid$Drilled)
tapply(data.mid$Drilled, data.mid$Cage.treat, sum) #148 drilled in open, 127 in partial plots

table(data.mid$Alive, data.mid$Cage.treat) #frequency tables
table(data.mid$Alive, data.mid$Cage.treat, data.mid$Erad.treat)
table(data.mid$Site, data.mid$Cage.treat)

#proportional survival across treatments
initial = tapply(data.mid$Initial.oys, list(data.mid$Cage.treat,data.mid$Erad.treat, data.mid$Site2), sum)
alive = tapply(data.mid$Alive, list(data.mid$Cage.treat,data.mid$Erad.treat, data.mid$Site2), sum)
proportional.survival = alive/initial
proportional.survival

#graphical exploration
cage.plot<-ggplot(data.mid,aes(x=Cage.treat,y=prop.surv, fill=Erad.treat))+geom_boxplot()+facet_grid(Site2~.)+theme_bw()+
  labs(y="Oyster survival", x="Cage treatment")+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.y = element_blank())
cage.plot2<- cage.plot+guides(fill=guide_legend(title="Treatment"))+scale_fill_viridis_d(option="plasma")+
  scale_x_discrete(labels = c("caged", "open", "partial cage"))
cage.plot2

ppi=300
png("figures/oyster cage plot.png", width=12*ppi, height=9*ppi, res=ppi)
cage.plot2
dev.off()

#point estimate plot alternative
data.mid$Cage.treat<-factor(data.mid$Cage.treat, levels = c("open", "partial","closed"))
A_col <- "#2D708EFF"
B_col <- "#3CBB75FF"
cage.plot.alt<-ggplot(data.mid,aes(x=Cage.treat,y=prop.surv))+geom_boxplot(aes(fill=Erad.treat))+
  geom_point(shape = 21,size = 2.5, position = position_jitterdodge(jitter.width = 0.05, jitter.height = 0.05), aes(group=Erad.treat, fill=Erad.treat))+
  facet_grid(Site2~.)+theme_bw()+
  labs(y="Oyster survival", x="Cage treatment", fill = "Treatment")+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18, vjust=-0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.y = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) 
cage.plot.alt

ppi=300
png("figures/oyster cage plot alt.png", width=9*ppi, height=6*ppi, res=ppi)
cage.plot.alt
dev.off()

#drilled plot
data.mid$prop.drilled<-data.mid$Drilled/data.mid$Initial.oys
cage.plot.drilled<-ggplot(data.mid,aes(x=Cage.treat,y=prop.drilled))+geom_boxplot(aes(fill=Erad.treat))+
  geom_point(shape = 21,size = 2.5, position = position_jitterdodge(jitter.width = 0.05, jitter.height = 0.05), aes(group=Erad.treat, fill=Erad.treat))+
  facet_grid(Site2~.)+theme_bw()+
  labs(y="Drilled oysters (proportional)", x="Cage treatment", fill = "Treatment")+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18, vjust=-0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.y = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) 
cage.plot.drilled

ppi=300
png("figures/oyster cage plot drilled.png", width=9*ppi, height=6*ppi, res=ppi)
cage.plot.drilled
dev.off()
#data.mid analysis using glmer
m1<-glmer(y.mid~ Cage.treat * Erad.treat + (1|Site), family="binomial", data=data.mid)
summary(m1)
Anova(m1)
#warnings - evidence for complete separation

#using bias reduced glm
m2<-brglm(formula = y.mid~Cage.treat * Erad.treat, family = "binomial", data=data.mid)
summary(m2)
plot(m2)
Anova(m2)

#posthoc tests if desired
K1 <- glht(m2, mcp(Cage.treat = "Tukey"))$linfct
K2 <- glht(m2, mcp(Erad.treat = "Tukey"))$linfct
summary(glht(m2, linfct = rbind(K1, K2)))

glht(model = m2, linfct = mcp(Erad.treat="Tukey"))

#all tidal heights

data2<-filter(data, Site == "A-south" | Site == "Cove") #select only control sites
table(data2$Height)
A_col <- "#404788FF"
B_col <- "#238A8DFF"
C_col <- "#55C667FF"

cage.plot.allheights<-ggplot(data2,aes(x=Height,y=prop.surv))+geom_boxplot(aes(fill=Cage.treat))+
  facet_grid(Site2~.)+theme_bw()+
  labs(y="Oyster survival", x="Elevation (m)", fill = "Treatment")+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18, vjust=-0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_blank(), strip.background = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col, C_col)) +
  scale_color_manual(values = c(A_col, B_col, C_col)) +
  scale_y_continuous(breaks = c (0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  coord_cartesian (ylim = c(0,1.2))
cage.plot.allheights
str(data)

ppi=300
png("figures/elevation oyster cage plot.png", width=9*ppi, height=6*ppi, res=ppi)
cage.plot.allheights
dev.off()



  