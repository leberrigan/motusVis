# motusVis
Visualise Motus Data using moveVis

## Introduction
These functions are wrappers to make it a little bit easier to make animated maps of Motus tracks.

See [moveVis](https://movevis.org/) to learn more about the R library

## Dependencies
These functions were created using R version 4.5.1 and were not tested on other versions.

These functions depend on the following libraries:
 - tidyverse
 - motus
 - moveVis
 - sf
 - lubridate
 
# How to use

Coming soon...


# Example

```
animateTrack( 
  gwwa.df, 
  combine.years = T, 
  resolution.time = 0.25, 
  resolution.unit = 'days', 
  save.prefix = "GWWA", 
  colour.scale = "gradient", 
  colour.var = "departureDate", 
  map_service = "maptiler", 
  map_type = "satellite", 
  bbox = bbox, 
  show_scale = F, 
  show_northarrow = F )

```

![Animated map of Motus tracks](GWWA-0.25days-satellite.gif)