## ---------------------------
## Purpose of script: Create map of field sites. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------

library(tidyverse)
library(sp)
library(sf)
library(ggspatial)#annotation_map_tile
library(grid) #overlay inset map onto main map



# read in data ------------------------------------------------------------
lakesbasin<-read_csv("data/sites.csv")
colnames(lakesbasin)



# set up ------------------------------------------------------------------
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
# see Manuel Gimond's "Understanding the Proj4 coordinate syntax" for the above 2 lines -- https://mgimond.github.io/Spatial/coordinate-systems-in-r.html 

nad83spatial<-with(lakesbasin, sp::SpatialPoints(coords=cbind(lakesbasin$Long, lakesbasin$Lat), proj4string = CRS(crsnad83)))
# "lakesbasin" "Long" "Lat" refer to read-in dataframe. Change these based on your dataframe. 

sptransformnad83<-sp::spTransform(nad83spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
sierra_nad83 <- as.data.frame(sptransformnad83@coords) #convert back to df 
names(sierra_nad83)<-c("Longitude_albers", "Latitude_albers")
SierraPoints<-cbind(lakesbasin, sierra_nad83)




# CA state outline --------------------------------------------------------
# Not used in final map plot. 

states<-map_data('state') #all USA states
ca<-subset(states, region=="california") #just CA lat/long data
wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ca_coords <- sp::SpatialPoints(coords = with(ca, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj)) #here are the coordinates in that frame, and this is the CRS they're in 
ca_albers <- spTransform(ca_coords, CRSobj = CRS(crsaea)) #put latlong to this CRS

ca$long <- ca_albers@coords[,1]
ca$lat <- ca_albers@coords[,2]

ca_map<-ggplot(ca, aes(x=long,y=lat))

ca_map+
  geom_path(color="black",aes(group=group))+
  geom_point(data = SierraPoints, aes(x = Longitude_albers, y = Latitude_albers), color = 'cyan4', alpha = 1, shape=16,size=4)+
  theme_bw()+
  theme(axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", size=2))+
  ggsn::scalebar(ca, dist = 100, dist_unit="km",st.size=3, height=0.01,transform =FALSE, location="bottomleft")+
  ggsn::north(ca, scale=0.1,symbol=1,location="topright")




# CA state outline, with county delineation ---------------------------------------------------------
# used in final map plot
# package ggsn: http://oswaldosantos.github.io/ggsn/

county<-map_data('county') #all USA states
ca<-subset(county, region=="california") #just CA lat/long data

#yolo solano napa lake - counties... 

wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
ca_coords <- SpatialPoints(coords = with(ca, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj)) #here are the coordinates in that frame, and this is the CRS they're in 
ca_albers <- spTransform(ca_coords, CRSobj = CRS(crsaea)) #put latlong to this CRS

ca$long <- ca_albers@coords[,1]
ca$lat <- ca_albers@coords[,2]

ca_map<-ggplot(ca, aes(x=long,y=lat))
a<-ca_map+
  geom_path(color="black",aes(group=group),linewidth=0.1)+
  geom_point(data = SierraPoints, aes(x = Longitude_albers, y = Latitude_albers), 
             color = 'black', alpha = 0.9, shape=16,size=2)+
  theme_bw()+
  theme(axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "white", size=2))
  



# County zoom-in with tiles -----------------------------------------------
# used in final map plot
AllSites<-read_csv("data/sites.csv")

colnames(AllSites)
LBsites<-AllSites %>% 
  dplyr::filter(!drainage=="Carson")

lakesbasin_sf <- st_as_sf(LBsites, coords = c("Long", "Lat"), remove = F, crs = 4326)

plot(lakesbasin_sf$geometry)
plot(lakesbasin_sf["drainage"], graticule = TRUE, axes = TRUE)

lakesbasin_sf$Habitat <- factor(lakesbasin_sf$flow_type, 
                                levels = c("lake",
                                           "stream"),
                                labels = c("Lake",
                                           "Stream")) #for legend text to look nicer

b<-ggplot() + 
  annotation_map_tile(zoom = 16, forcedownload = FALSE) + # this can take a few seconds the first time
  geom_sf(data=lakesbasin_sf, aes(shape=Habitat), alpha=0.8, size=3, inherit.aes = FALSE)+
  #geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4, inherit.aes = FALSE) +
  labs(x="Longitude", y="Latitude")+ 
  #scale_fill_manual(name="Flow Type", labels=c("Lake","Stream"))+
  # spatial-aware automagic scale bar
  annotation_scale(location = "br",style = "ticks") +
  # spatial-aware automagic north arrow
  annotation_north_arrow(width = unit(.3,"in"), 
                         pad_y = unit(.1, "in"),location = "tr", 
                         which_north = "true") +
  theme_bw(base_family = "Helvetica")



# Overlay A + B  ----------------------------------------------------------
# for final manuscript map figure

#main map
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
#inset map - where will it be and how big?
insetmap <- viewport(width = 0.2, height = 0.3, x = 0.28, y = 0.24)

#save the map in this location, with these specs
tiff(filename = "figures/Map3Insect.tiff",
     width = 6, height = 5, units = "in", pointsize = 12, res=600,
     compression = "none")

#make map
grid.newpage()
print(b, vp = mainmap)
print(a, vp = insetmap)
dev.off() # the png or pdf or jpg won't save until you run this command








# Don't trust Preview? https://www.r-bloggers.com/2013/03/high-resolution-figures-in-r/
# Decent calculator: https://www.pixelconverter.com/pixels-to-dpi-converter/#google_vignette
