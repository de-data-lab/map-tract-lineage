# Get difference in the layers

get_diff <- function(sfc1, sfc2, suffix = c("_2010", "_2020")){
    
    # Get the geometry in a different column 
    # and arrange
    sfc1 <- sfc1 %>% 
        mutate(geometry_col = .data$geometry) %>%
        arrange(TRACTCE)
    
    sfc2 <- sfc2 %>% 
        mutate(geometry_col = .data$geometry) %>%
        arrange(TRACTCE)
    
    # Drop geometry for join operations 
    sfc1_nogeo <- sfc1 %>%
        st_drop_geometry()
    
    sfc2_nogeo <- sfc2 %>%
        st_drop_geometry()
    
    # Combine 2010 and 2020 data
    nogeo_joined <- sfc1_nogeo %>%
        full_join(sfc2_nogeo,
                  by = "GEOID",
                  suffix = suffix)
    
    # Map through to calculate symmetric differences
    col1_name <- sym(paste0("geometry_col", suffix[[1]]))
    col2_name <- sym(paste0("geometry_col", suffix[[2]]))
    shape_diff <- nogeo_joined %>%
        mutate(geometry_sym_diff = map2(!!col1_name, 
                                        !!col2_name,
                                        st_sym_difference))
    
    # Convert the shape for the symmetric difference
    shape_diff <-  shape_diff %>% 
        mutate(geometry = st_as_sfc(geometry_sym_diff))
    
    # Assign back the geometry to the data frame
    st_geometry(shape_diff) <- shape_diff$geometry
    
    # Label the joined df
    shape_diff <- shape_diff %>%
        rowwise() %>%
        mutate(leaflet_label = HTML(
            paste0("2010: ", NAME_2010, "<br>",
                   "2020: ", NAME_2020))
            ) %>% 
        ungroup()
    
    # rmapshaper::ms_simplify could be used to simplify slivers
    
    return(shape_diff)
}
