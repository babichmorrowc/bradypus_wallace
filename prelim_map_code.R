library(readr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#read in processed occurrence files from Wallace for the three species
#all have been spatially thinned to 10km
Bradypus_variegatus_processed_occs <- read_csv("~/OneDrive - AMNH/Wallace/Data/Wallace_downloads/Bradypus_variegatus_processed_occs.csv")
Bradypus_tridactylus_processed_occs <- read_csv("~/OneDrive - AMNH/Wallace/Data/Wallace_downloads/Bradypus_tridactylus_processed_occs.csv")
Bradypus_torquatus_processed_occs <- read_csv("~/OneDrive - AMNH/Wallace/Data/Wallace_downloads/Bradypus_torquatus_processed_occs.csv")

#read in occurrences compiled from literature for B. variegatus and B. tridactylus
Bradypus_variegatus_litdata <- read_delim("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv", 
                                          +     "\t", escape_double = FALSE, trim_ws = TRUE)
Bradypus_tridactylus_litdata <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")

#delineating the latitudes and longitudes of interest for south and central america
southamerica<-map_data("world",xlim = c(-100,-25), ylim = c(-30,25),lforce='e')

#plot that region
gg1 <- ggplot() + 
  geom_polygon(data = southamerica, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
gg1

#add processed occurrence points from Wallace
gg1 + 
  geom_point(data = Bradypus_variegatus_processed_occs, aes(x = longitude, y = latitude), color = "green",size = .75) +
  geom_point(data = Bradypus_tridactylus_processed_occs, aes(x = longitude, y = latitude), color = "blue",size = .75) +
  geom_point(data = Bradypus_torquatus_processed_occs, aes(x = longitude, y = latitude), color = "red",size = .75)

gg1 + 
  geom_point(data = Bradypus_variegatus_processed_occs, aes(x = longitude, y = latitude), pch = 1) +
  geom_point(data = Bradypus_tridactylus_processed_occs, aes(x = longitude, y = latitude), pch = 17) +
  geom_point(data = Bradypus_torquatus_processed_occs, aes(x = longitude, y = latitude), pch = 15)

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


duplicated(Bradypus_tridactylus_lit_data)
