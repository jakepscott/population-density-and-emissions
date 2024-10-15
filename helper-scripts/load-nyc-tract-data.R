# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(tigris)

# Load data ---------------------------------------------------------------
# Load population by tract 
tract_pops_raw <- get_acs(geography = "tract", year = 2017,
                          variables = "B01003_001", geometry = T, 
                          state = c("New York", "Texas"))

# Load a conversion from Census Block Groups to tracts 
blocks_to_tracts <- read_csv(here("data/raw/geocorr-2018-census-block-to-tract-ny-tx.csv"), skip = 1) %>% 
  janitor::clean_names()

# Load place to tract crosswalk so I can look at NYC
place_to_tract <- read_csv(here("data/raw/geocorr-2018-place-to-tract-ny-tx.csv"), skip = 1)
place_to_tract <- place_to_tract %>% 
  janitor::clean_names()

# Load NTAs so I can remove tracts within parks
tracts_to_nta <- readxl::read_xlsx(here("data/raw/nyc2010census_tabulation_equiv.xlsx"), skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(row_number() != 1) %>% 
  mutate(tract_fips = paste0(36, 
                             x2010_census_bureau_fips_county_code, 
                             x2010_census_tract))


# Load emissions data
emission_blocks <- st_read(here("data/raw/DARTE_v2.gdb/DARTE_v2.gdb/"))

# Clean Data --------------------------------------------------------------
# Only take emissions data from NYS and TX
emission_blocks_tibble <- emission_blocks %>% 
  select(GEOID, bg_area_m2, kgco2_2017) %>% 
  filter(substr(GEOID,1,2) %in% c(36, 48)) 

# Add emission data to Census Blocks
blocks_pol <- blocks_to_tracts %>% 
  mutate(tract = str_remove(tract, "\\."),
         block_fips = paste0(county_code, tract, block_group),
         tract_fips = paste0(county_code, tract)) %>% 
  select(block_fips, tract_fips) %>% 
  left_join(emission_blocks_tibble %>% 
              rename(block_fips = GEOID))

# Aggregate Census Blocks to Census Tracts 
tracts_pol <- blocks_pol %>% 
  group_by(tract_fips) %>% 
  summarise(pollution = sum(kgco2_2017),
            area = sum(bg_area_m2, na.rm = T))

# Add in population data, calc pollution per capita and pop density
tract_full <- tracts_pol %>% 
  left_join(tract_pops_raw %>% 
              select(tract_fips = GEOID, pop = estimate)) %>% 
  filter(pop > 100) %>% 
  mutate(area = st_area(geometry),
         area_mi = as.numeric(units::set_units(area, mi^2)), 
         emissions_per_cap = pollution/pop,
         pop_den = pop/area_mi) 

# Filter for just NYC
nyc_tracts <- place_to_tract %>% 
  filter(place_name == "New York city, NY") %>% 
  mutate(tract = str_remove(tract, "\\."),
         tract_fips = paste0(county_code, tract))

tract_pol_nyc <- tract_full %>% 
  filter(tract_fips %in% nyc_tracts$tract_fips) %>% 
  left_join(tracts_to_nta %>% 
              distinct(borough, tract_fips, nta_name = x7)) %>% 
  filter(!str_detect(nta_name, "park-cemetery")) 