# Load libraries ---------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(here)
library(ggiraph)
library(ggrepel)
library(sf)
library(ggtext)
library(extrafont)
loadfonts()
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Load data -------------------------------------------------
# Each place and its emissions
place_emissions_raw <- read_csv(here("data/created/place_emissions-2017.csv"))

# Place boundaries
places_raw <- get_acs(geography = "place", year = 2017, state = state.abb,
                  variables = "B01003_001", geometry = T)

# Clean data --------------------------------------------------------------
# Get the area for each place 
places <- places_raw %>%
  mutate(area = st_area(geometry),
         area_mi = units::set_units(area, mi^2))

# Take the place-emissions data and join in place population and area. 
# Select the top 20 places by population. Put emissions in per capita
# terms and calculate population density
place_emissions <- place_emissions_raw %>% 
  left_join(places %>% 
              as_tibble() %>% 
              dplyr::select(place = NAME, pop = estimate, area_mi)) %>% 
  top_n(n = 20, wt = pop) %>% 
  mutate(emission_per_cap = sum/pop, 
         pop_den = as.double(pop/area_mi),
         place = str_remove(place, " city"),
         place = str_remove(place, " metropolitan government \\(balance\\)"),
         place = str_remove(place, " \\(balance\\)")) %>% 
  mutate(place = str_replace(place, pattern = "Texas", replacement =  "TX"),
         place = str_replace(place, pattern = " Indiana", replacement =  " IN"),
         place = str_replace(place, pattern = "Florida", replacement =  "FL"),
         place = str_replace(place, pattern = "Arizona", replacement =  "AZ"),
         place = str_replace(place, pattern = "North Carolina", replacement =  "NC"),
         place = str_replace(place, pattern = "California", replacement =  "CA"),
         place = str_replace(place, pattern = "Pennsylvania", replacement =  "PA"),
         place = str_replace(place, pattern = "Illinois", replacement =  "IL"),
         place = str_replace(place, pattern = "Washington", replacement =  "WA"),
         place = str_replace(place, pattern = "New York", replacement =  "NY"),
         place = str_replace(place, pattern = "Michigan", replacement =  "MI"))

# Plot top places --------------------------------------------------------------
(plot <- place_emissions %>% 
   ggplot(aes(pop_den, emission_per_cap)) +
   geom_point_interactive(aes(tooltip = glue::glue("City: {place}
                                                    Emissions per capita: {prettyNum(round(emission_per_cap), big.mark = ',')}
                                                    Pop Density: {prettyNum(round(pop_den), big.mark = ',')}"))) +
   #geom_smooth(se = F) +
   geom_label_repel(aes(label = str_wrap(place,20)),
                    size = 2, min.segment.length = 0) +
   scale_x_continuous(labels = function(x){glue::glue("{prettyNum(x, big.mark = ',')}")}) +
   scale_y_continuous(labels = function(x){glue::glue("{prettyNum(x, big.mark = ',')}")}) +
   labs(title = "Emissions decline most dramatically when going from\ndensities like that of Houston to Seattle",
        subtitle = "Tons of CO<sub>2</sub> emitted per capita annually versus population per mi<sup>2</sup> by census place",
        caption = "DARTE Annual On-road CO2 Emissions (2017). American Community Survey (2013-2017 5-year)",
        x = "People per mi<sup>2</sup>",
        y = "Tons of CO<sub>2</sub> per capita") +
   theme_bw(base_size = 12,
            base_family = "Roboto Condensed") +
   theme(plot.title.position = "plot",
         plot.title = element_text(size = rel(1.5), 
                                   face = "bold"),
         plot.subtitle = element_markdown(size = rel(1),
                                           face = "italic",
                                           color = "grey30"),
         plot.caption = element_text(size = rel(.8),
                                     face = "italic",
                                     color = "grey70"),
         axis.title = element_markdown()))

ggsave(here("figures/02-emissions-by-place.png"),
       dpi = 600, height = 6, width = 6, units = "in")

# girafe(ggobj = plot,
#        options = list(
#          opts_sizing(rescale = FALSE),
#          opts_zoom(max = 2)  # Enable zoom with a maximum zoom factor
#        ))


