library(readr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#read in processed occurrence files for the three species
#all have been spatially thinned to 10km
Bradypus_variegatus_processed_occs <- read_csv("OneDrive - AMNH/Wallace/Bradypus_variegatus_processed_occs.csv")
Bradypus_tridactylus_processed_occs <- read_csv("OneDrive - AMNH/Wallace/Bradypus_tridactylus_processed_occs.csv")
Bradypus_torquatus_processed_occs <- read_csv("OneDrive - AMNH/Wallace/Bradypus_torquatus_processed_occs.csv")

southamerica<-map_data("world",xlim = c(-100,-25), ylim = c(-30,25),lforce='e')

gg1 <- ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
gg1

gg2 <- gg1 + coord_fixed(xlim = c(-100,-25), ylim = c(-75,25))
gg2

gg2 + 
  geom_point(data = Bradypus_variegatus_processed_occs, aes(x = longitude, y = latitude), color = "green",size = .75) +
  geom_point(data = Bradypus_tridactylus_processed_occs, aes(x = longitude, y = latitude), color = "blue",size = .75) +
  geom_point(data = Bradypus_torquatus_processed_occs, aes(x = longitude, y = latitude), color = "red",size = .75)

gg2 + 
  geom_point(data = Bradypus_variegatus_processed_occs, aes(x = longitude, y = latitude), pch = 1) +
  geom_point(data = Bradypus_tridactylus_processed_occs, aes(x = longitude, y = latitude), pch = 17) +
  geom_point(data = Bradypus_torquatus_processed_occs, aes(x = longitude, y = latitude), pch = 15)

