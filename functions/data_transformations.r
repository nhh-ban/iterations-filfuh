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

# Transform volume function
transform_volumes <- function(json_data) {
  # Extract the 'edges' list containing data
  edges <- json_data$data$trafficData$volume$byHour$edges
  
  # Create a data frame with 'from', 'to', and 'volume' columns
  df <- data.frame(
    from = sapply(edges, function(edge) edge$node$from),
    to = sapply(edges, function(edge) edge$node$to),
    volume = sapply(edges, function(edge) edge$node$total$volumeNumbers$volume),
    stringsAsFactors = FALSE
  )
  
  # Convert the 'from' column to date-time objects
  df$from <- as.POSIXct(df$from, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
  
  # Return the transformed data frame
  return(df)
}

