
# Expected columns:
# tagDeployID, speciesEN, ts, recvDeployLat, recvDeployLon, recvDeployName
animateTrack <- function(data.input, 
                         resolution.time = 1, 
                         resolution.unit = 'days', 
                         combine.years = F, 
                         save.prefix, 
                         colour.scale = "discrete", 
                         colour.var = "tagDeployID", 
                         map_service = "osm", 
                         map_type = "topographic", 
                         r_base,
                         api_key,
                         api_key_file = "api_token",
                         bbox = NULL,
                         show_scale = T,
                         show_northarrow = T,
                         text_colour = "black") {
  
  ## Load Required Packages
  require(motus)
  require(tidyverse)
  require(lubridate)
  require(sf)
  require(moveVis)
  require(move2)
  
  if (missing(data.input)) 
    stop("Missing input data")
  
  if (!is(data.input, "SQLiteConnection") & !is.data.frame(data.input))
    stop("Input data is neither an SQLite database nor a data frame")
  
  if (is(data.input, "SQLiteConnection"))
    if (!DBI::dbExistsTable(data.input, "allruns"))
      stop("Require table 'allruns' does not exist within input database")
    else
      input.df = data.input %>% tbl("allruns") %>% 
        filter(motusFilter == 1) %>%
        collect() %>% as.data.frame() %>%
        pivot_longer(cols = c("tsBeginCorrected","tsEndCorrected"), names_to = "tsPart", values_to = "ts")
  else if (!"ts" %in% colnames( data.input ) )
    stop("Can't find any timestamp column in data frame (needs either 'ts' or 'tsCorrected')")
  else
    input.df = data.input

  
  if (missing(input.df) | nrow(input.df) < 2) {
    stop('No data found. Aborting.')
  }
  
  message("Input data appears valid")
  
  if (missing( save.prefix )) {
    save.prefix <- "unknown"
    if (is(data.input, "SQLiteConnection") & DBI::dbExistsTable(data.input, "meta")) {
      meta <- sql %>% tbl("meta") %>% collect() %>% as.data.frame()
      if (meta %>% filter(key == "tagProject") %>% nrow > 0)
        save.prefix <- meta %>% filter(key == "tagProject") %>% pull(val) %>% .[1] %>% paste0("proj", "_", .)
      else if (meta %>% filter(key == "recvSerno") %>% nrow > 0)
        save.prefix <- meta %>% filter(key == "recvSerno") %>% pull(val) %>% .[1] %>% paste0("recv", "_", .)
    }
  }
  
  
  frame.label.format <- paste0(
    case_when(
      combine.years ~ "%B %d",
      .default = "%F"
    ),
    case_when(
      resolution.unit %in% c("d","day","days") ~ "",
      resolution.unit %in% c("h","hour","hours") ~ " %H:00",
      resolution.unit %in% c("min","minute", "minutes") ~ " %H:%M",
      .default = "%T"
    )
  )
  
  filtered.df <- input.df %>%
    group_by(tagDeployID) %>%
    filter( difftime(max(ts), min(ts), units = resolution.unit) > resolution.time * 2,
            length(unique(recvDeployName)) > 1)

  if (nrow(filtered.df) < nrow(input.df)) {
    message("Not all tags will be plotted due to there being too few locations (n < 2) or the track duration is shorter than the animation frame resoloution (duration < ", resolution.time, " ", resolution.unit, ")")
    if (nrow(filtered.df) < 2) {
      message("Too few rows to plot. Aborting")
      return()
    }
  }
  
  message('Format data... ', appendLF = F)
  
  ## Select necessary columns and clean the data
  tracks.df <- filtered.df %>%
    mutate(lat = recvDeployLat,
           lon = recvDeployLon) %>%
    ## We can't use the NAs so I'm removing them
    filter(!is.na(lat) & !is.na(tagDeployID)) %>%
    ## MoveVis doesn't like duplicate rows so I'm removing them here. 
    ## I could use 'unique' or 'group_by', but this seems to be the fastest way
    #distinct(tagDeployID, speciesEN, ts, lat, lon, recvDeployName) %>%
    arrange(tagDeployID) %>%
    ## Format the date so it's readable (for moveVis)
    mutate(timestamp = as.POSIXct(ts, origin = '1970-01-01'),
           colour = .data[[colour.var]]) %>%
    ## I also have to get rid of tracks with only one site so here I group them by tag deployment
    ## then select those deployments with more than 1 recvDeployName
    group_by(tagDeployID) %>%
    filter(length(unique(recvDeployName)) > 1) %>%
    ungroup() %>%
    arrange(tagDeployID, timestamp)
  
  
  if (combine.years) {
    message("Combining all years together")
    tracks.df <- tracks.df %>%
      mutate(timestamp = as.POSIXct(paste(paste("2020", month(timestamp), day(timestamp),sep = "-"), paste(hour(timestamp), minute(timestamp), second(timestamp),sep = ":"), sep = " ")))
  }
  
  message('Done.')
  

  
  message('Make the animation... ', appendLF = F)
  
  attributes(tracks.df$ts)$tzone <- "UTC"
  # 
  # tracks.sf <- tracks.df %>%
  #   st_as_sf(coords = c("lon","lat"), crs = 4326) %>% #WGS84
  #   st_transform(crs = 3857)
  # 
  # Prepare the move dataframe 
  tracks.move2 <- mt_as_move2(tracks.df, time_column = "timestamp", track_id_column = "tagDeployID", track_attributes = c("colour","speciesEN"), coords = c("lon", "lat"), crs = 4326)
 # tracks.move2 <- mt_as_move2(tracks.sf, time_column = "timestamp", track_id_column = "tagDeployID", track_attributes = c("colour","speciesEN"))
  
  # align move_data to a uniform time scale
  m <- align_move(tracks.move2, res = units::set_units(resolution.time, resolution.unit, mode = "standard")) |> sf::st_transform(crs = 3857)
  
  # create spatial frames with a OpenStreetMap watercolour map
  if (colour.scale == "gradient") {
    # Normalize departureDate to a 0–1 scale
    scale.range <- range(tracks.df$colour, na.rm = TRUE)
    
    tracks.df <- tracks.df %>%
      mutate(scaleNorm = (colour - scale.range[1]) / (scale.range[2] - scale.range[1]))
    
    # Create gradient color ramp
    colfun <- colorRampPalette(c("violetred", "turquoise"))
    
    
    tracks.df <- tracks.df %>%
      mutate(colour = colfun(100)[floor(scaleNorm * 99) + 1])
    
    m$colour <- tracks.df$colour[match(m$tagDeployID, tracks.df$tagDeployID)]
    
  }
  
  if (missing(api_key)) {
    warning("Missing API key for mapping service. Searching for key in secret file '",api_key_file,"'.")
    if (file.exists(api_key_file))
      api_key <- read_lines(api_key_file)
    else
      stop("Missing API key. Can't find secret file: '",api_key_file,"'")
    if (nchar(api_key) > 10)
      message("Success! API Key found")
    else
      stop("Secret file '",api_key_file,"' contains invalid API key '",api_key,"'")
  }
  if (missing(r_base)) {
    frames <- frames_spatial(m,
                             map_service = map_service, 
                             map_type = map_type, 
                             map_token = api_key,
                             alpha = 1,
                             trace_show = TRUE,
                             path_legend = FALSE,
                             path_fade = TRUE,
                             ext = bbox,
                             crs = st_crs(3857),
                             equidistant = F) 
  } else {
    
    time(r_base) <- rep(tracks.df$timestamp %>% min, nlyr( r_base ))
    
    frames <- frames_spatial(m,
                             r = r_base,
                             alpha = 1,
                             trace_show = TRUE,
                             path_legend = FALSE,
                             path_fade = TRUE,
                             ext = bbox,
                             crs = st_crs(3857),
                             equidistant = F) 
  }
  
  frames <- frames %>% 
    add_labels(x = "Longitude", y = "Latitude") %>%# add some customizations, such as axis  labels
    add_timestamps(type = "label", format = frame.label.format) %>% 
    add_progress()
  
  if (show_northarrow)
    frames <- frames %>% add_northarrow(colour = text_colour)
  if (show_scale)
    frames <- frames %>% add_scalebar(colour = text_colour)
  
  
  # animate frames
  animate_frames(frames, out_file = paste0(save.prefix, "-", resolution.time, resolution.unit,"-",map_type,'.gif'), overwrite = T)
  
  message('Done.')
  
}
