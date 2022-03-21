# Plot map
# Plot leaflet
library(tidyverse)
library(leaflet)
library(here)
library(tigris)
library(sf)
library(htmltools) # Escaping HTML for security

# source scripts
source(here("r/get_diff.R"))

# Using 2018 Cartographic Boundary (CB) shape that uses 2010 districting 
# (older files have compatibility issues)
DE_shape_2010 <-  read_sf(here("data/raw/cb_2018_10_tract_500k/"))
# Using 2020 CB shape that uses 2020 districting
DE_shape_2020 <- read_sf(here("data/raw/cb_2020_10_tract_500k/"))

# Default anchor for the leaflet view
default_lat <- 39.1824
default_lng <- -75.4

# Function to create leaflet labels
render_label <- function(year, census_tract, GEOID){
    return(paste0("Year: ", year, " | ",
                  "Census Tract: ", census_tract, " | ",
                  "GEOID: ", GEOID))
}

# Create leaflet labels
DE_shape_2010 <- DE_shape_2010 %>%
    mutate(leaflet_label = render_label(2010, NAME, GEOID))

DE_shape_2020 <- DE_shape_2020 %>%
    mutate(leaflet_label = render_label(2020, NAME, GEOID))


# Get the difference between two shapefiles
DE_shape_diff <- get_diff(DE_shape_2010, DE_shape_2020)

plot_map <- function(){
    
    DE_2010_grp_name <- "<span style='color: #1b9e77'/>2010</span>"
    DE_2020_grp_name <- "<span style='color: #d95f02'/>2020</span>"
    
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = default_lng,
                lat = default_lat,
                zoom = 9) %>%
        addPolygons(data = DE_shape_diff,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~leaflet_label,
                    labelOptions = labelOptions(noHide = FALSE),
                    group = "Symmetric Difference") %>%
        addPolygons(data = DE_shape_2010, 
                    color = "#1b9e77",
                    weight = 6,
                    group = DE_2010_grp_name,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~htmlEscape(leaflet_label)) %>%
        addPolygons(data = DE_shape_2020,
                    color = "#d95f02",
                    weight = 3,
                    group = DE_2020_grp_name,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~htmlEscape(leaflet_label)) %>%
        addLayersControl(overlayGroups = c(DE_2010_grp_name, DE_2020_grp_name,
                                           "Symmetric Difference"),
                         options = layersControlOptions(collapsed = FALSE))
}
