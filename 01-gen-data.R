# Load libraries ---------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(tiff)
library(raster)
library(terra)
library(here)
library(sf)
library(ggiraph)
library(ggrepel)

# NOTEL: I ran this, but it takes a while. To spare you the time, I save it
# as a CSV and import it (I could've parallelized this with furrr I imagine,
# but that would require me brushing off the dust- juice not worth squeeze)
# Load data ---------------------------------------------------------------
#Emissions
emissions <- raster(here("data/raw/onroad_2017.tif"))

# Place boundaries
places <- get_acs(geography = "place", year = 2017, state = state.abb,
                  variables = "B01003_001", geometry = T)

# Just grab big places (exclude places where I have no raster)
top_places <- places %>%
  filter(estimate>100000) %>%
  filter(!str_detect(NAME, "Puerto Rico|Alaska|Hawaii")) %>%
  mutate(area = st_area(geometry),
         area_mi = units::set_units(area, mi^2)) %>%
  arrange(NAME) %>%
  mutate(row = row_number())

# Function that crops raster to just place boundary -----------------------
get_place <- function(place_choice, data, raster){
  tryCatch(expr = {
    print(place_choice)
    place_data <- data %>%
      filter(NAME == place_choice)
    
    place_data <-  st_transform(place_data, st_crs(emissions))
    
    cropped <- crop(raster, place_data)
    final <- mask(cropped, place_data)
    
    # Will need to make sure this is *actually* how to calculate emissions for places, just summing
    # the raster values...
    final %>%
      as.data.frame(xy = T) %>%
      as_tibble() %>%
      filter(!is.na(onroad_2017)) %>%
      summarise(sum = sum(onroad_2017)) %>%
      mutate(place = place_choice)
  },
  error = function(e){
    message(glue::glue("An error occurred: {place_choice}\n"), e)
  })
}

# Apply function and save -------------------------------------------------
place_emissions <- map_df(top_places$NAME, get_place, places, emissions)
write_csv(place_emissions, here("data/created/place_emissions-2017.csv"))
