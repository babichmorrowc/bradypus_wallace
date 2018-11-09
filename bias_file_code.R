#Load packages
library(spocc)
library(plyr)

#import csv of species list for bias file
#includes primates, sloths, margay, olingos, mountain coatis, coatis, and kinkajous
sp_list <- taxonomy <- read.csv("~/OneDrive - AMNH/Wallace/BiasFile/redlist_species_data_1c60e640-3b63-4897-a3c3-ae6a0c44df70/taxonomy.csv")
View(sp_list)

#Create vector of species names
sp_vec <- paste(sp_list$genusName, sp_list$speciesName, sep = " ")

#Taxonomic resolution
sp_vec[1] <- "Lagothrix lagothricha"

#Get occurrences for the first species:
occ_1 <- occ(query = sp_vec[1], from = "gbif", limit = 1500)
df_1 <- occ2df(occ_1)
bg_locations <- df_1

for(i in 2:length(sp_vec)){
  occ_i <- occ(query = sp_vec[i], from = "gbif", limit = 1000)
  df_i <- occ2df(occ_i)
  if(length(df_i) == 0){
    print(paste(i, ": No observations in GBIF", sep = ""))
  } else{
    bg_locations <- rbind.fill(bg_locations, df_i)
  }
}

#Plot the background points
#Make dataframe:
world_map <- map_data("world")
#Assign the world map plot to the variable "world"
world <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
#map background points on map
world +
  geom_point(data = bg_locations, aes(x = longitude, y = latitude),
             color = "green",
             size = 1)
