library(maps)
library(mapdata)
library(ggplot2)

# Hier habe ich vorher auf eine andere Art und Weise versucht eine Karte zu erstellen
get_USA_map <- function() {
  map_usa <- map_data("usa")
  state_borders <- map_data("state")
  
  return(
    ggplot() + 
      geom_polygon(data = map_usa, aes(x = long, y = lat, group = group), fill = "#A6CAE0", color = "black") + 
      geom_polygon(data = state_borders, aes(x = long, y = lat, group = group), fill = "#BEBEBE", color = "black", size = 0.5) +  
      theme_void()
  )
}

# wird nicht mehr benutzt
input_ufo_sightings_in_map <- function(map, sightings_data, year) {

  sightings_count <- sightings_data %>% 
    group_by(longitude, latitude) %>% 
    summarise(count = n())
  
  
  ufo_plot <- map + 
    geom_point(data = sightings_count, aes(x = longitude, y = latitude, size = count), color = "red") +  
    scale_size_continuous(range = c(1, 5)) + 
    labs(title = paste("UFO Sightings", year))  
  
  return(ufo_plot)
}

# Hiermit habe ich ein paar erste nützliche Informationen über alle UFO-Formen bekommen
extract_useful_information_shapes <- function(dataframe) {
  
  shape_counts <- table(dataframe$shape)
  
  
  shape_counts_filtered <- shape_counts[shape_counts >= 100]
  

  shape_counts_ordered <- sort(shape_counts_filtered, decreasing = TRUE)
  
  
  shape_colors <- rainbow(length(shape_counts_ordered))
  
  
  shape_info <- data.frame(
    shape = names(shape_counts_ordered),
    count = shape_counts_ordered,
    color = shape_colors
  )
  
  
  return(shape_info)
}