# Problem 2
transform_metadata_to_df <- 
  function(metadata){
    stations_metadata[[1]] %>% 
      map(as_tibble) %>%  
      list_rbind() %>%  
      mutate(latestData = map_chr(latestData, 1, .default = ""))  %>%  
      mutate(latestData = as_datetime(latestData))  %>%  
      mutate(location = map(location, unlist)) |>  
      mutate(
        lat = map_dbl(location, "latLon.lat"),
        lon = map_dbl(location, "latLon.lon")
      ) %>% 
      select(-location)
  }

# Problem 4a
to_iso8601 <- 
  function(date, offset){
    iso8601(date+days(offset)) %>% 
      paste0("Z")
  }
# Test
to_iso8601(as_datetime("2016-09-01 10:11:12"),0)

# Problem 5
