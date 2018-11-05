library(readr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#read in occurrences compiled from literature for B. variegatus and B. tridactylus
Bradypus_variegatus_litdata <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv")
Bradypus_tridactylus_litdata <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")

#making geographic region a factor
Bradypus_variegatus_litdata$geographic_region <- as.factor(Bradypus_variegatus_litdata$geographic_region)
#making management unit a factor
Bradypus_variegatus_litdata$management_unit <- as.factor(Bradypus_variegatus_litdata$management_unit)
#making population region a factor
Bradypus_variegatus_litdata$population_region <- as.factor(Bradypus_variegatus_litdata$population_region)


#delineating the latitudes and longitudes of interest for south and central america
southamerica<-map_data("world",xlim = c(-100,-25), ylim = c(-30,25),lforce='e')

#plot that region
gg1 <- ggplot() + 
  geom_polygon(data = southamerica, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
gg1

#add data from literature
#for variegatus
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude))
#for tridactylus
gg1 +
  geom_point(data = Bradypus_tridactylus_litdata, aes(x = longitude, y = latitude))
#for both
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude), color = "green") +
  geom_point(data = Bradypus_tridactylus_litdata, aes(x = longitude, y = latitude), color = "blue")

#variegatus data by source
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude, color = source))
#variegatus data based on "geographic region"
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude, color=geographic_region))
#variegatus data by management unit
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude, color=management_unit))
#variegatus data by population region (Silva et al. 2018)
gg1 +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude, color=population_region))




#Use Google maps
api_key = "AIzaSyBK7lLbqoqnYFdzf-idYYposb-1gwyRAlQ"
register_google(key = api_key)

#Satellite map
bbox <- make_bbox(lon = Bradypus_variegatus_litdata$longitude, lat = Bradypus_variegatus_litdata$latitude, f = 0.2)
map <- get_map(location = bbox, source = "google", maptype = "satellite")
ggmap(map)
ggmap(map) +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude), color = "darkorange1") +
  geom_point(data = Bradypus_tridactylus_litdata, aes(x = longitude, y = latitude), color = "deepskyblue")

#Zoomed in map
bbox2 <- make_bbox(lon = c(-65, -45), lat = c(-5,0), f = 0.1)
map2 <- get_map(location = bbox2, source = "google", maptype = "roadmap")
ggmap(map2)
ggmap(map2) +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude), color = "darkgreen") +
  geom_point(data = Bradypus_tridactylus_litdata, aes(x = longitude, y = latitude), color = "blue")

#Zoomed in to Central America
bbox3 <- make_bbox(lon = c(-90, -70), lat = c(2,12), f = 0)
map3 <- get_map(location = bbox3, source = "google", maptype = "roadmap")
ggmap(map3)
ggmap(map3) +
  geom_point(data = Bradypus_variegatus_litdata, aes(x=longitude, y=latitude), color = "darkgreen") +
  geom_point(data = Bradypus_tridactylus_litdata, aes(x = longitude, y = latitude), color = "blue")


