
library(rgdal)
library(ggplot2)

sfb<-st_read("data/map/bayarea_general.shp")

st_geometry_type(sfb)
st_crs(sfb)             #check coordinate reference system
st_bbox(sfb)            #find extent
sfb                     #view all metadata

ggplot()+geom_sf(data=sfb, size=1, color="black", fill = "white")+coord_sf(crs = "NAD83")  #entire map

ggplot()+geom_sf(data=sfb, size=1, color="black", fill = "white")+
  coord_sf(crs = "NAD83", xlim = c(-122.6, -122), ylim = c(37.4,38.3))  #sfb regional

ggplot()+geom_sf(data=sfb, size=1, color="black", fill = "white")+
  coord_sf(crs = "NAD83", xlim = c(-122.55, -122.4), ylim = c(37.84, 37.9)) #richardson bay
