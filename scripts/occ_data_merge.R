library(rgdal)


# Format old data ---------------------------------------------------------


# load WWF ecoregions shape file
ecoregions <- readOGR(dsn = "/Users/hellenfellows/Desktop/WWF_ecoregions", layer = "wwf_terr_ecos")

# Load in original occurrence data
variegatus_litdata_merge <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/variegatus_litdata_merge.csv")
tridactylus_litdata_merge <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/tridactylus_litdata_merge.csv")
torquatus_litdata_merge <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/torquatus_litdata_merge.csv")

# Get G200_REGIO for the original literature data

# Make original literature data into SpatialPoints
variegatus_merge_sp <- SpatialPoints(variegatus_litdata_merge[ ,2:3])
tridactylus_merge_sp <- SpatialPoints(tridactylus_litdata_merge[ ,2:3])
torquatus_merge_sp <- SpatialPoints(torquatus_litdata_merge[ ,2:3])

# Set proj4string to match CRS of ecoregions
proj4string(variegatus_merge_sp) <- proj4string(ecoregions)
proj4string(tridactylus_merge_sp) <- proj4string(ecoregions)
proj4string(torquatus_merge_sp) <- proj4string(ecoregions)

# add a column to the datasets for the G200_REGIO
variegatus_litdata_merge$G200_REGIO <- over(variegatus_merge_sp, ecoregions)$G200_REGIO
View(variegatus_litdata_merge)
tridactylus_litdata_merge$G200_REGIO <- over(tridactylus_merge_sp, ecoregions)$G200_REGIO
View(tridactylus_litdata_merge)
torquatus_litdata_merge$G200_REGIO <- over(torquatus_merge_sp, ecoregions)$G200_REGIO
View(torquatus_litdata_merge)


# Format new data ---------------------------------------------------------


# load in new data
NEOTROPICAL_XENARTHRANS_QUALITATIVE <- read.csv("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Occurrence_Data/Xenarthra_data/NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv")
View(NEOTROPICAL_XENARTHRANS_QUALITATIVE)

# extract only Bradypus
sloth_occurrences <- NEOTROPICAL_XENARTHRANS_QUALITATIVE[NEOTROPICAL_XENARTHRANS_QUALITATIVE$GENUS == "Bradypus", ]
View(sloth_occurrences)
# remove unidentified occurrences
sloth_occurrences <- sloth_occurrences[sloth_occurrences$SPECIES != "Bradypus sp.", ]
# remove Bradypus pygmaeus
sloth_occurrences <- sloth_occurrences[sloth_occurrences$SPECIES != "Bradypus pygmaeus", ]

# Split up into different datasets
variegatus_santos <- sloth_occurrences[sloth_occurrences$SPECIES == "Bradypus variegatus", ]
tridactylus_santos <- sloth_occurrences[sloth_occurrences$SPECIES == "Bradypus tridactylus", ]
torquatus_santos <- sloth_occurrences[sloth_occurrences$SPECIES == "Bradypus torquatus", ]

# Select out species, coordinates, reference, and year/month info
variegatus_santos_merge <- variegatus_santos[,c("SPECIES", "LONG_X", "LAT_Y", "REFERENCE", "COL_STRT_YR", "COL_STRT_MO", "COL_END_YR", "COL_END_MO")]
tridactylus_santos_merge <- tridactylus_santos[,c("SPECIES", "LONG_X", "LAT_Y", "REFERENCE", "COL_STRT_YR", "COL_STRT_MO", "COL_END_YR", "COL_END_MO")]
torquatus_santos_merge <- torquatus_santos[,c("SPECIES", "LONG_X", "LAT_Y", "REFERENCE", "COL_STRT_YR", "COL_STRT_MO", "COL_END_YR", "COL_END_MO")]

# Fix SPECIES column to match my original data
variegatus_santos_merge$SPECIES <- "bradypus_variegatus"
tridactylus_santos_merge$SPECIES <- "bradypus_tridactylus"
torquatus_santos_merge$SPECIES <- "bradypus_torquatus"

# Rename latitude and longitude columns
colnames(variegatus_santos_merge)[2:3] <- c("LONGITUDE", "LATITUDE")
colnames(tridactylus_santos_merge)[2:3] <- c("LONGITUDE", "LATITUDE")
colnames(torquatus_santos_merge)[2:3] <- c("LONGITUDE", "LATITUDE")

# Add POPULATION_REGION column
variegatus_santos_merge$POPULATION_REGION <- NA
tridactylus_santos_merge$POPULATION_REGION <- NA
torquatus_santos_merge$POPULATION_REGION <- NA

# Add G200_REGIO column
variegatus_santos_merge$G200_REGIO <- variegatus_santos$OlsonG200r
tridactylus_santos_merge$G200_REGIO <- tridactylus_santos$OlsonG200r
torquatus_santos_merge$G200_REGIO <- torquatus_santos$OlsonG200r


# Merge data --------------------------------------------------------------

variegatus_occs <- rbind(variegatus_santos_merge, variegatus_litdata_merge)
View(variegatus_occs)
tridactylus_occs <- rbind(tridactylus_santos_merge, tridactylus_litdata_merge)
View(tridactylus_occs)
torquatus_occs <- rbind(torquatus_santos_merge, torquatus_litdata_merge)
View(torquatus_occs)

# Remove duplicate occurrences --------------------------------------------

variegatus_occs <- variegatus_occs[!duplicated(variegatus_occs[c('LONGITUDE', 'LATITUDE')]), ]
tridactylus_occs <- tridactylus_occs[!duplicated(tridactylus_occs[c('LONGITUDE', 'LATITUDE')]), ]
torquatus_occs <- torquatus_occs[!duplicated(torquatus_occs[c('LONGITUDE', 'LATITUDE')]), ]


# Visualize ---------------------------------------------------------------

ggmap(map) +
  geom_point(data = variegatus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "darkorange2", cex = 0.75) +
  geom_point(data = tridactylus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "springgreen3", cex = 0.75) +
  geom_point(data = torquatus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "deepskyblue", cex = 0.75)
  

