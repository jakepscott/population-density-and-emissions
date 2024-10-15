# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(viridis)
library(tigris)
library(ggtext)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

options(scipen = 999)
# Load Data ---------------------------------------------------------------
source(here("helper-scripts/load-nyc-tract-data.R"))

# Faceted -----------------------------------------------------------------
(plot <- tract_pol_nyc %>% 
   filter(!is.na(pollution)) %>% 
   ggplot(aes(pop_den, (emissions_per_cap))) +
   geom_point(alpha = .5) +
   geom_smooth(se = F) +
   #scale_x_continuous(labels = function(x){glue::glue("{prettyNum(x, big.mark = ',')}")}) +
   scale_x_continuous(labels = function(x){glue::glue("{prettyNum(x/1000, big.mark = ',')}k")}) +
   scale_y_log10(labels = function(x){glue::glue("{prettyNum(x, big.mark = ',')}")}) +
   labs(title = "Even at the tract level, emissions decline most rapidly at\nthe early stages of densification",
        subtitle = "Tons of CO<sub>2</sub> per capita versus population per mi<sup>2</sup> by census tract in New York City",
        caption = "DARTE Annual On-road CO2 Emissions (2017). American Community Survey (2013-2017 5-year)",
        x = "People per square mile",
        y = "Tons of CO<sub>2</sub> per capita") +
   facet_wrap(~borough, scales = "free") +
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

ggsave(here("figures/03-emissions-by-tract-nyc-faceted.png"),
       dpi = 600, height = 6, width = 6, units = "in")
