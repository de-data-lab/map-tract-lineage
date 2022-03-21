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
    label_string <- paste0(tags$b(year), "<br>",
                           "Census Tract: ", census_tract, "<br>",
                           "GEOID: ", GEOID)
    return(label_string)
}

# Create leaflet labels
DE_shape_2010 <- DE_shape_2010 %>%
    rowwise() %>%
    mutate(leaflet_label = HTML(render_label(2010, NAME, GEOID))) %>%
    ungroup()

DE_shape_2020 <- DE_shape_2020 %>%
    rowwise() %>%
    mutate(leaflet_label = HTML(render_label(2020, NAME, GEOID))) %>%
    ungroup()

# Get the difference between two shapefiles
DE_shape_diff <- get_diff(DE_shape_2010, DE_shape_2020)

plot_map <- function(){
    
    DE_2010_grp_name <- "<span style='color: #1b9e77'/>2010</span>"
    DE_2020_grp_name <- "<span style='color: #d95f02'/>2020</span>"
    diff_layer_name <- "<span style='color: #3477eb'/>Difference</span>"
    
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = default_lng,
                lat = default_lat,
                zoom = 9) %>%
        addMapPane("diff", zIndex = 400) %>%
        addMapPane("2010", zIndex = 450) %>%
        addMapPane("2020", zIndex = 500) %>%
        addPolygons(data = DE_shape_diff,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~leaflet_label,
                    labelOptions = labelOptions(noHide = FALSE),
                    group = diff_layer_name,
                    options = pathOptions(pane = "diff")) %>%
        addPolygons(data = DE_shape_2010, 
                    color = "#1b9e77",
                    weight = 6,
                    group = DE_2010_grp_name,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~leaflet_label,
                    options = pathOptions(pane = "2010")) %>%
        addPolygons(data = DE_shape_2020,
                    color = "#d95f02",
                    weight = 3,
                    group = DE_2020_grp_name,
                    highlight = highlightOptions(fillOpacity = 0.8,
                                                 weight = 2),
                    label = ~leaflet_label,
                    options = pathOptions(pane = "2020")) %>%
        addLayersControl(overlayGroups = c(diff_layer_name,
                                           DE_2010_grp_name, 
                                           DE_2020_grp_name),
                         options = layersControlOptions(collapsed = FALSE))
}
