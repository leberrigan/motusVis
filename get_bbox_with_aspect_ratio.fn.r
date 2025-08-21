get_bbox_with_aspect_ratio.fn <- function(
    input.data, 
    aspect_ratio = 1, 
    coords = c("lon", "lat"), 
    crs = 4326) {

  require(sf)
  require(dplyr)
  
  # Get original bounding box
  bbox <- input.data %>%
    st_as_sf(coords = coords, crs = crs) %>%
    st_bbox()
  
  # Extract coordinates
  xmin <- bbox[["xmin"]]
  xmax <- bbox[["xmax"]]
  ymin <- bbox[["ymin"]]
  ymax <- bbox[["ymax"]]
  
  # Compute center
  xmid <- (xmin + xmax) / 2
  ymid <- (ymin + ymax) / 2
  
  # Current width and height
  width <- xmax - xmin
  height <- ymax - ymin
  
  # Adjust dimensions
  if ((width / height) > aspect_ratio) {
    # Too wide — increase height
    new_height <- width / aspect_ratio
    ymin_new <- ymid - new_height / 2
    ymax_new <- ymid + new_height / 2
    xmin_new <- xmin
    xmax_new <- xmax
  } else {
    # Too tall — increase width
    new_width <- height * aspect_ratio
    xmin_new <- xmid - new_width / 2
    xmax_new <- xmid + new_width / 2
    ymin_new <- ymin
    ymax_new <- ymax
  }
  
  # Step 7: Create new bbox
  bbox_fixed <- st_bbox(c(xmin = xmin_new, xmax = xmax_new,
                          ymin = ymin_new, ymax = ymax_new),
                        crs = st_crs(crs))
  
  return(bbox_fixed)
}
