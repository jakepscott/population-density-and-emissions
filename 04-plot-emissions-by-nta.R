# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(tigris)
library(patchwork)
library(ggtext)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load data  -------------------------------------------
# NYC tract emissions data
source(here("helper-scripts/load-nyc-tract-data.R"))

# Shapefile for NTAs
nta_sf <- sf::read_sf(here("data/raw/2010 Neighborhood Tabulation Areas (NTAs)/"))

# Clean Data --------------------------------------------------------------
# Obtain emissions data at the NTA level
nta_pol <- tract_pol_nyc %>% 
  select(tract_fips, pollution, pop) %>% 
  left_join(tracts_to_nta %>% 
              rename(ntacode  = neighborhood_tabulation_area_nta, 
                     nta_name = x7)) %>% 
  group_by(ntacode, nta_name) %>% 
  summarise(pollution= sum(pollution, na.rm = T),
            pop = sum(pop, na.rm = T))

# Generate categories for pollution and population density. 
# I do so by lining each NTA up by the measure of interest and
# creating 5 groups with the same number of NTAs in each. A quintile 
# analysis
nta_full <- nta_pol %>% 
  left_join(nta_sf) %>% 
  mutate(area = st_area(geometry),
         area_mi = units::set_units(area, mi^2), 
         pop_den = pop/area_mi,
         pol_per_cap = pollution/pop) %>% 
  ungroup() %>% 
  filter(pop > 1000) %>%  
  # So says make 5 equally sized groups of
  # NTAs based on their population density and pollution
  mutate(pop_den_cat = ntile(pop_den, 5), 
         pop_den_cat = factor(pop_den_cat, 
                              levels = c(1:5),
                              labels = c("Low",
                                         "Medium Low",
                                         "Medium",
                                         "Medium High", 
                                         "High")),
         pol_cat =ntile(pol_per_cap, 5), 
         pol_cat = factor(pol_cat, 
                          levels = c(1:5),
                          labels = c("Low",
                                     "Medium Low",
                                     "Medium",
                                     "Medium High", 
                                     "High")))

# Plot -------------------------------------------------------------
# Density
(pop_den_plot <- nta_full %>% 
   st_as_sf() %>%
   ggplot() +
   geom_sf(data = st_as_sf(nta_sf %>% 
                             filter(boro_name != "Staten Island")), fill = "grey90") +
   geom_sf(aes(fill = pop_den_cat)) +
   scale_fill_brewer(palette = "YlOrBr") +
   guides(fill = guide_legend(title.position="top", title.hjust = 0.5, 
                              nrow=2,byrow=TRUE)) +
   labs(fill = "People per square mile (Quintiles)") +
   theme_void(base_size = 12,
              base_family = "Roboto Condensed") +
   theme(legend.position = "bottom"))

# Pollution
(pol_plot <- nta_full %>%
    filter(!str_detect(nta_name, "park-cemetery")) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = st_as_sf(nta_sf %>% 
                              filter(boro_name != "Staten Island")), fill = "grey90") +
    geom_sf(aes(fill = pol_cat)) +
    scale_fill_brewer(palette = "YlOrRd") +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5,
                               nrow=2,byrow=TRUE)) +
    labs(fill = "Tons of carbon per capita (Quintiles)") +
    theme_void(base_size = 12,
               base_family = "Roboto Condensed") +
    theme(legend.position = "bottom"))

pop_den_plot + pol_plot

ggsave(here("figures/04-map.png"),
       dpi = 600, height = 8, width = 8, units = "in")  
