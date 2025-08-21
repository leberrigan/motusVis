# Plot GWWA Data
library(tidyverse)
source('animate-track.fn.r')
source('get_bbox_with_aspect_ratio.fn.r')

# Directory where input data is stored
dir <- "D:/Data/"
# Filename of input data
file.name <- "GWWA_Movement_data.csv"
  
  # Expected columns:
  # tagDeployID, speciesEN, ts, recvDeployLat, recvDeployLon, recvDeployName

# Format provided dataset to match requirements of function
gwwa.df <- read.csv(paste0(dir, file.name)) %>%
  filter(Include == 1, Detection_type != "Capture") %>%
  rename(
    tagDeployID = Band_number,
    speciesEN = Species,
    recvDeployLat = Latitude,
    recvDeployLon = Longitude
  ) %>%
  mutate(
    Detection_Date = ifelse( grepl( " ", Detection_Date, fixed = T), Detection_Date, paste(Detection_Date,"12:00", sep = " ")),
    ts = Detection_Date %>% as.POSIXct(format = "%d/%m/%Y %H:%M"),
    recvDeployName = paste(recvDeployLat, recvDeployLon, sep = ""),
  ) %>%
  group_by(tagDeployID) %>%
  mutate(
    departureDate = ts[which(Detection_type == "Departure_date")] %>% format(format = "%j") %>% as.numeric
  ) %>%
  filter( ! (max(recvDeployLon) > -85 & recvDeployLat[which.max(recvDeployLon)] > 20) ) %>%
  select(tagDeployID, speciesEN, ts, recvDeployLat, recvDeployLon, recvDeployName, departureDate)

bbox <- get_bbox_with_aspect_ratio.fn(gwwa.df, 1, coords = c("recvDeployLon","recvDeployLat"), crs=4326)

head(gwwa.df)

animateTrack( gwwa.df, combine.years = T, resolution.time = 0.25, resolution.unit = 'days', save.prefix = "GWWA", colour.scale = "gradient", colour.var = "departureDate", map_service = "maptiler", map_type = "satellite", bbox = bbox, show_scale = F, show_northarrow = F )
