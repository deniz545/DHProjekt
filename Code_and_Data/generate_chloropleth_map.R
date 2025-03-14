library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(usmap)
# Erstellung der Karte
plot_choropleth <- function(year_group_wanted, ufo_summary) {
  # Filterung nach Jahr
  data_to_plot <- ufo_summary %>% filter(year_group == year_group_wanted)
  
  # Wie viele Sichtungen gab es insgesamt?
  total_sightings <- sum(data_to_plot$sightings, na.rm = TRUE)
  
  # Übersetzen des Bundesstaates auf den vollständig Namen --> CA = California --> damit sich das dann auf einer Karte der USA einträgen lässt
  data_to_plot$state_full <- state.name[match(data_to_plot$state, state.abb)]
  
  # Erstellen der Karte
  plot <- plot_usmap(data = data_to_plot, values = "sightings", regions = "states") +
    scale_fill_continuous(low = "lightblue", high = "darkblue", name = "Sichtungen") +
    theme_void() +
    labs(
      title = paste("UFO-Sichtungen von", year_group_wanted, "bis", year_group_wanted + 4),
      subtitle = paste("Gesamtzahl der Sichtungen:", total_sightings)  
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),  
      legend.background = element_rect(fill = "white", color = NA)  
    )
  
  return(plot)
}


