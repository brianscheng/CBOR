#analysis of bivariate relationship between urosalpinx and ostrea in richardson bay
#BSC changed column names within dataset, # Drills = drills, and Live.oysters = olys
library(tidyverse)
library(janitor)

data<-read.csv("data/All Shoreline Survey Data_Oct 23 2018_Master Copy.csv", na.strings = "N/A", stringsAsFactors = TRUE)
str(data)
summary(data$Site) #11 'Sites' because Lanis Boulders is included
summary(data$olys) #24 NAs
which(is.na(data$olys))
data[17,]
data[654,] #tide was coming up, incomplete transect

#remove rows with NA for drills or olys
data1<-drop_na(data, drills) 
data1<-drop_na(data, olys) #now 929 rows

#what is data coverage across elevations?
data1 %>%
  tabyl (Site,Tidal.elevation)%>%
  adorn_totals(c("row","col"))
#if we are focusing on 1.5' elevation for the FE MS then we need to exclude some data

#remove these sites, some are at different elevation
data2<-filter(data1, !Site %in% c("Lanis Boulders", "Bothin","Blackies"))
#remove data from Lanis boulders, bothin, and blackies pasture
#not equal to is declared with ! before variable, %in% is an operator for listing out the condition)
#now with 806 observations

table(data2$Site, data2$Tidal.elevation) #most common elevation is 1.5

data3<-filter(data2, Tidal.elevation == 1.5) #now with 324 quadrats
table(data3$Site)
table(data3$Site, data3$Date)
#Aramburu Central - 06/17, 10/17, 12/17, 04/18, 07/18
#Aramburu South   - 07/17, 10/17, 12/17, 04/18, 07/18
#Brickyard Cove   -        09/17, 12/17, 04/18, 07/18
#Cove Apts        - 06/17, 09/17, 12/17, 04/18, 07/18
#Dunphy Park      -               12/17, 04/18, 07/18
#Lanis Beach      - 06/17, 09/17, 12/17, 04/18, 07/18
#Pink House       -               12/17, 04/18,

#how many quads have both species found in them? 
which(data3$olys>0)
which(data3$olys>0 & data3$drills>0)

####plotting####--------------------------------------------------------------------------------------------

#make a double errorbar plot with site level data
#first with drill data
drills.mean<-tapply(data3$drills, data3$Site, mean)
drills.sd<-tapply(data3$drills, data3$Site, sd)
drills.count<-tapply(data3$drills, data3$Site, length)
drills.sem<-drills.sd/(sqrt(drills.count))
drills.mean
drills.sd
drills.sem
#second with oyster data
olys.mean<-tapply(data3$olys, data3$Site, mean)
olys.sd<-tapply(data3$olys, data3$Site, sd)
olys.count<-tapply(data3$olys, data3$Site, length)
olys.sem<-olys.mean/sqrt(olys.count)
olys.mean
olys.sd
olys.count
olys.sem
#stitch together summary data
data_mean<-data.frame(drills.mean, drills.sem, olys.mean, olys.sem)
data_mean$site<-factor(row.names(data_mean))
str(data_mean)

plot1<-ggplot(data_mean, aes(x=drills.mean, y=olys.mean))+
  geom_errorbar(ymin = olys.mean-olys.sem, ymax = olys.mean+olys.sem, width = 0)+
  geom_errorbarh(xmin = drills.mean-drills.sem, xmax = drills.mean+drills.sem, height = 0)+
  geom_point(fill="white", shape =21, size = 3)+
  coord_cartesian((xlim = c(0, 30)), ylim = c(0,10))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=14), axis.title = element_text(size = 16))+
  labs(x= expression(paste("Drill density (0.25 ",m^{-2},")")),
       y= expression(paste("Oyster density (0.25 ",m^{-2},")")))

ppi=300
png("figures/bivariate quadrat.png", width=9*ppi, height=9*ppi, res=ppi)
plot1
dev.off()

#plot to get site labels
ggplot(data_mean, aes(x=drills.mean, y=olys.mean))+geom_point()+
  geom_errorbar(ymin = olys.mean-olys.sem, ymax = olys.mean+olys.sem, width = 0)+
  geom_errorbarh(xmin = drills.mean-drills.sem, xmax = drills.mean+drills.sem, height = 0)+
  coord_cartesian((xlim = c(0, 30)), ylim = c(0,10))+theme_bw()+
  geom_text(aes(label=site), hjust = -.2, vjust = -.3)


#second approach with spring quadrats only, 2017 for focal four and 2018 for remainder
str(data2)
ggplot(data2,aes(x=drills, y=olys))+geom_point()
ggplot(data2,aes(x=drills, y=olys, color=Site))+geom_point()

#now lets trim the data
#BSC assembled datafile with spring data for the focal four sites (March 2017) and complementary sites (April 2018)
#so each site has 10 quadrats, this is part of the reduced dataset to maximize comparability
#all sites are before eradication efforts and at 1.5 foot elevation

data.b<-read.csv("data/Drill oyster correlation data 2020-04-23.csv")
str(data.b)
#7 sites, with 10 quadrats each to visualize relationship between oysters and drills

ggplot(data.b,aes(x=drills, y=olys, color=Site))+geom_point()+
  geom_jitter(width=.2, height=.2)+theme_bw()+labs(x="Drills (count/quadrat)", y="Oysters (count/quadrat)")

####analysis

cor(data.b$drills,data.b$olys, method = "pearson")
cor.test(data.b$drills,data.b$olys, method = "pearson")
#pearson is inappropriate because it assumes a linear relationship

cor(data.b$drills,data.b$olys, method = "spearman")
cor.test(data.b$drills,data.b$olys, method = "spearman") #ties in rank
#spearman does not assume linearity but cannot compute exact p-value with ties

#permutation approach here to generate p-value
attach(data.b)                                     #for easier to read code
cor.test(drills, olys, method="spearman")$estimate # calculate original rho, yields rho = -0.549
test<-function(drills) suppressWarnings(cor.test(drills,olys, method="spearman")$estimate) 
rho<-test(drills)
p<-replicate(10000, test(sample(drills,length(drills),replace=F))) #simulated permutation distribution
hist(p, xlim=range(-1,1))
abline(v=rho)

p.out <- sum(abs(p) > abs(rho))    # Count of strict (absolute) exceedances, need to take absolute value of rho because it is negative
p.at <- sum(abs(p) == rho)    # Count of equalities, if any
(p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value, which is 0
detach(data.b)


