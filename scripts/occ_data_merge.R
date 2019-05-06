library(rgdal)


# Ecoregions --------------------------------------------------------------

# load WWF ecoregions shape file
ecoregions <- readOGR(dsn = "/Users/hellenfellows/Desktop/WWF_ecoregions", layer = "wwf_terr_ecos")

# List of the G200_NUM values for my data
G200_nums <- c(48, 44, 47, 147, 93, 141, 59, 40, 39, 45, 63, 46, 58, 44, 43, 39, 142)

plot(ecoregions[ecoregions@data$G200_NUM %in% G200_nums, ], col = 1:length(G200_nums))

# Format old data ---------------------------------------------------------

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


# Go back over G200 regions -----------------------------------------------

# Make original literature data into SpatialPoints
variegatus_occs_sp <- SpatialPoints(variegatus_occs[ ,2:3])
tridactylus_occs_sp <- SpatialPoints(tridactylus_occs[ ,2:3])
torquatus_occs_sp <- SpatialPoints(torquatus_occs[ ,2:3])

# Set proj4string to match CRS of ecoregions
proj4string(variegatus_occs_sp) <- proj4string(ecoregions)
proj4string(tridactylus_occs_sp) <- proj4string(ecoregions)
proj4string(torquatus_occs_sp) <- proj4string(ecoregions)

variegatus_occs$G200_REGIO <- over(variegatus_occs_sp, ecoregions)$G200_REGIO
View(variegatus_occs)
sum(is.na(variegatus_occs$G200_REGIO))
tridactylus_occs$G200_REGIO <- over(tridactylus_occs_sp, ecoregions)$G200_REGIO
View(tridactylus_occs)
sum(is.na(tridactylus_occs$G200_REGIO))
torquatus_occs$G200_REGIO <- over(torquatus_occs_sp, ecoregions)$G200_REGIO
View(torquatus_occs)
sum(is.na(torquatus_occs$G200_REGIO))

variegatus_occs$ECO_NAME <- over(variegatus_occs_sp, ecoregions)$ECO_NAME
sum(is.na(variegatus_occs$ECO_NAME))
tridactylus_occs$ECO_NAME <- over(tridactylus_occs_sp, ecoregions)$ECO_NAME
sum(is.na(tridactylus_occs$ECO_NAME))
torquatus_occs$ECO_NAME <- over(torquatus_occs_sp, ecoregions)$ECO_NAME
sum(is.na(torquatus_occs$ECO_NAME))

unique(variegatus_occs$ECO_NAME)
unique(variegatus_occs$ECO_NAME[is.na(variegatus_occs$G200_REGIO)])

# Visualize ---------------------------------------------------------------

ggmap(map) +
  geom_point(data = variegatus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "darkorange2", cex = 0.75) +
  geom_point(data = tridactylus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "springgreen3", cex = 0.75) +
  geom_point(data = torquatus_occs, aes(x=LONGITUDE, y=LATITUDE), color = "deepskyblue", cex = 0.75)
  

# Add population regions to variegatus ------------------------------------

# Map all of the different population regions
ggmap(map) +
  geom_point(data = variegatus_occs, aes(x=LONGITUDE, y=LATITUDE, color = POPULATION_REGION))

ggmap(map) +
  geom_point(data = variegatus_occs[grepl("Rio Negro", variegatus_occs$G200_REGIO), ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
     geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Southwestern Amazonian Moist Forests", ], aes(x=LONGITUDE, y=LATITUDE, color = POPULATION_REGION))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Cerrado Woodlands and Savannas", ], aes(x=LONGITUDE, y=LATITUDE, color = POPULATION_REGION))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Amazon-Orinoco-Southern Caribbean mangroves", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Atlantic Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Atlantic Dry Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Northern Andean Montane Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Choc??-Dari??n Moist Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Guianan Highlands Moist Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Mesoamerican Pine-Oak Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Central Andean Yungas", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Chiquitano Dry Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Napo Moist Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Chocó-Darién Moist Forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "South American Pacific mangroves", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$G200_REGIO == "Coastal Venezuela Montane Forests", ], aes(x=LONGITUDE, y=LATITUDE))

# Map the ECO_NAME areas where G200_REGIO is NA
ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Mato Grosso seasonal forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Maranhão Babaçu forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Southern Atlantic mangroves", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Uatuma-Trombetas moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Tocantins/Pindare moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Xingu-Tocantins-Araguaia moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Tapajós-Xingu moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Guianan savanna", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Caatinga", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Isthmian-Atlantic moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Isthmian-Pacific moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Costa Rican seasonal moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Beni savanna", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Bolivian montane dry forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Dry Chaco", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Apure-Villavicencio dry forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Catatumbo moist forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Guajira-Barranquilla xeric scrub", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Sinú Valley dry forests", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "La Costa xeric shrublands", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Mesoamerican Gulf-Caribbean mangroves", ], aes(x=LONGITUDE, y=LATITUDE))

ggmap(map) +
  geom_point(data = variegatus_occs[variegatus_occs$ECO_NAME == "Central American Atlantic moist forests", ], aes(x=LONGITUDE, y=LATITUDE))


# Add population region identifier
G200_AF_regions <- variegatus_occs$G200_REGIO == "Atlantic Forests"
G200_AMZ_regions <- variegatus_occs$G200_REGIO %in% c("Central Andean Yungas", "Chiquitano Dry Forests", "Napo Moist Forests") | grepl("Rio Negro", variegatus_occs$G200_REGIO) | grepl("Amazon", variegatus_occs$G200_REGIO)
G200_TA_regions <- variegatus_occs$G200_REGIO %in% c("Northern Andean Montane Forest", "Mesoamerican Pine-Oak Forests", "South American Pacific mangroves") | grepl("Choc", variegatus_occs$G200_REGIO)

variegatus_occs$POPULATION_REGION <- ifelse(AF_regions, variegatus_occs$POPULATION_REGION <- "AF",
       ifelse(AMZ_regions, variegatus_occs$POPULATION_REGION <- "AMZ",
              ifelse(TA_regions, variegatus_occs$POPULATION_REGION <- "TA", NA)))

ECO_AF_regions <- variegatus_occs$ECO_NAME == "Southern Atlantic mangroves"
ECO_AMZ_regions <- variegatus_occs$ECO_NAME %in% c("Mato Grosso seasonal forests", "Maranhão Babaçu forests", "Uatuma-Trombetas moist forests", "Tocantins/Pindare moist forests", "Xingu-Tocantins-Araguaia moist forests", "Tapajós-Xingu moist forests", "Guianan savanna", "Beni savanna", "Bolivian montane dry forests", "Dry Chaco")
ECO_TA_regions <- variegatus_occs$ECO_NAME %in% c("Isthmian-Atlantic moist forests", "Isthmian-Pacific moist forests", "Costa Rican seasonal moist forests", "Apure-Villavicencio dry forests", "Catatumbo moist forests", "Guajira-Barranquilla xeric scrub", "Sinú Valley dry forests", "Mesoamerican Gulf-Caribbean mangroves", "Central American Atlantic moist forests")

for(i in 1:nrow(variegatus_occs)){
  if(variegatus_occs$G200_REGIO[i] %in% G200_AF_regions | variegatus_occs$ECO_NAME[i] %in% ECO_AF_regions){
    variegatus_occs$POPULATION_REGION[i] <- "AF"
  } else {
    if(variegatus_occs$G200_REGIO[i] %in% G200_AMZ_regions | variegatus_occs$ECO_NAME[i] %in% ECO_AMZ_regions){
      variegatus_occs$POPULATION_REGION[i] <- "AMZ"
    } else{
      if(variegatus_occs$G200_REGIO[i] %in% G200_TA_regions | variegatus_occs$ECO_NAME[i] %in% ECO_TA_regions){
        variegatus_occs$POPULATION_REGION[i] <- "TA"
      }
    }
  }
}

# Plot variegatus occurrences colored by POPULATION_REGION
ggmap(map) +
  geom_point(data = variegatus_occs, aes(x = LONGITUDE, y = LATITUDE, color = POPULATION_REGION))



