#time series plots of Urosalpinx at both sites

library(tidyverse)
library(janitor)
library(scales)

data<-read.csv("data/All Shoreline Survey Data_Oct 23 2018_Master Copy.csv", na.strings = "N/A", stringsAsFactors = TRUE)
str(data)
#select only experimental sites
summary(data$Site)
data<-data %>%
  filter (Site == "Aramburu Central" | Site =="Aramburu South" | Site == "Lanis Beach" | Site == "Cove Apts") %>%
  mutate (rdate = as.POSIXct(x=Date, format = "%m/%d/%Y")) %>%
  mutate (Treatment = case_when (Site =="Aramburu South" ~ "control",
                                 Site =="Aramburu Central" ~ "eradication",
                                 Site =="Lanis Beach" ~ "eradication",
                                 Site =="Cove Apts" ~ "control"))%>%
  mutate (Treatment = factor(Treatment)) %>%
  mutate (Site = case_when (Site == "Aramburu South" ~ "Aramburu",
                           Site == "Aramburu Central" ~ "Aramburu",
                           Site == "Lanis Beach" ~ "Lanis Beach",
                           Site == "Cove Apts" ~ "Lanis Beach")) %>%
  mutate (Site = factor(Site))
summary(data$Site)

#summary dataframe with point estimates
summary<-read.csv("data/Benthic survey Richardson Bay MEANS 2018-10-14.csv", na.strings = "N/A", stringsAsFactors = TRUE)
str(summary)
summary <- summary %>%
  filter (Habitat == "cobble") %>%
  mutate (rdate = as.POSIXct(x=Date, format = "%m/%d/%Y")) %>%
  mutate (Treatment = case_when(Treatment == "Control" ~ "Control",
                                Treatment == "Eradication" ~ "Removal"))
  
A_col <- "#2D708EFF"
B_col <- "#3CBB75FF"


plot<-ggplot(summary,aes(x=rdate, y=mean.uro, color=Treatment))+
  geom_point(size=3)+facet_grid(Site~., scales="free_y")+
  geom_errorbar(data=summary,aes(ymin=mean.uro-sem,ymax=mean.uro+sem), width=0, size=1)+geom_line()+
  labs(y=expression(paste("Drill density (0.25 ",m^{-2},")")), x="Date (year-month)")+theme_bw()+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
        axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
        axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
        strip.background = element_blank(),strip.text = element_blank(),        
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) +
  scale_x_datetime(date_breaks = "3 months",labels = date_format("%y-%m"))+
  geom_hline(yintercept = 1.25, linetype = "dashed")

plot
ppi=300
png("figures/drill_time_series.png", width=9*ppi, height=6*ppi, res=ppi)
plot
dev.off()

labs(x= expression(paste("Drill density (0.25 ",m^{-2},")")),
     y= expression(paste("Oyster density (0.25 ",m^{-2},")")))
