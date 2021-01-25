
library(rgdal)
library(ggplot2)

sfb<-st_read("data/map/bayarea_general.shp")

st_geometry_type(sfb)
st_crs(sfb)             #check coordinate reference system
st_bbox(sfb)            #find extent
sfb                     #view all metadata

ggplot()+geom_sf(data=sfb, size=1, color="black", fill = "white")+coord_sf(crs = "NAD83")  #entire map

ggplot()+geom_sf(data=sfb, size=1, color="black", fill = "grey")+
  coord_sf(crs = "NAD83", xlim = c(-122.6, -122), ylim = c(37.4,38.2))  #sfb regional


ggplot()+geom_sf(data=sfb, size=1, color="black")+       #sfb regional tweaked
  coord_sf(crs = "NAD83", xlim = c(-122.6, -122), ylim = c(37.4,38.2))+  
  theme_bw()+
  theme(panel.background =  element_rect (fill="aliceblue"))

rb<-ggplot()+geom_sf(data=sfb, size=0.8, color="black", fill = "white")+
  coord_sf(crs = "NAD83", xlim = c(-122.53, -122.44), ylim = c(37.85, 37.9)) #richardson bay
rb


#blog https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")

world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                      26.83)))


ggplot(data = world) +
  geom_sf() +
  geom_point(data=sites,aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

bay<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-122.8, -122), ylim = c(37.4, 38.2), expand = FALSE)

ppi=300
png("figures/bay.png", width=12*ppi, height=9*ppi, res=ppi)
bay
dev.off()
