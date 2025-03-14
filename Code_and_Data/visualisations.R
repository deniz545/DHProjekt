generate_chart_durations <- function(year, dataframe) {
  # Daten werden nach dem gewünschtem Jahr gefiltert
  data_to_plot <- dataframe %>% filter(year_group == year)
  
  # Zählen der Häufigkeiten
  duration_counts <- data_to_plot %>%
    count(duration) %>%
    arrange(desc(n)) %>%  # Nach Häufigkeit absteigend sortieren
    slice_head(n = 10)    # Nur die Top 10 Dauerwerte
  
  # Erstellung Balkendiagramm auf Basis der 10 häufigsten Sichtungsdauern
  plot <- ggplot(duration_counts, aes(x = reorder(duration, n), y = n, fill = duration)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = -0.3, size = 5) +  # Werte oberhalb der Balken anzeigen
    scale_fill_manual(values = rainbow(nrow(duration_counts))) +  # Unterschiedliche Farben für Balken
    theme_minimal() +
    labs(
      title = paste("Top 10 Sichtungsdauern für", year, "-", year + 4),
      x = "Dauer der Sichtung",
      y = "Anzahl"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),  
      panel.background = element_rect(fill = "white", color = NA), 
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty())  
  
  return(plot)
}
# generiert das Kreisdiagramm für die UFO-Formen
generate_chart_shape <- function(year, dataframe) {
  # Filterung nach Jahren und entfernen von Einträgen, welche einen NULL-Wert als Shape haben
  data_to_plot <- dataframe %>%
    filter(year_group == year, !is.na(shape))  
  
  # Zählen der Häufigkeiten der Formen
  shape_counts <- data_to_plot %>%
    count(shape) %>%
    arrange(desc(n))  
  
  # Summe aller Sichtungen
  total_sightings <- sum(shape_counts$n)
  
  # Alle Formen mit einer relativen Häufigkeit unter 5 % werden als Sonstige zusammengefasst
  threshold <- total_sightings * 0.05
  
  # Gruppieren aller Formen unter 5%
  shape_counts <- shape_counts %>%
    mutate(shape = ifelse(n < threshold, "Sonstige", shape)) %>%
    group_by(shape) %>%
    summarise(n = sum(n), .groups = "drop") 
  
  # Kurzes Sortieren, zuerst normale Werte, dann am Ende vom Kreisdiagramm kommt das Sonstige --> ist ja glaube normerlweise so
  shape_counts <- shape_counts %>%
    arrange(desc(n)) %>%
    mutate(shape = factor(shape, levels = c(setdiff(shape, "Sonstige"), "Sonstige")))  
  
  # Leere Grafik falls keine Formen gezählt wurden --> ist deshalb hier, sodass ich das alles in einer Schleife laufen lassen kann ohne  Fehler zu bekommen
  if (nrow(shape_counts) == 0) {
    return(ggplot() + ggtitle(paste("Keine UFO-Formen für", year, "-", year + 4)))
  }
  
  # Habe Online das Viridis Package gefunden --> gibt mir bessere und kontrasreichere Farben --> Es waren ziemlich viele Formen und der Kreisdiagramm wurde sonst etwas unübersichtlich
  colors <- setNames(viridis::viridis(nrow(shape_counts), option = "C"), shape_counts$shape)
  
  # Kleine Spielereien mit der Legende
  shape_labels <- setNames(paste0(shape_counts$shape, " (", shape_counts$n, ")"), shape_counts$shape)
  
  # JETZT kann endlich das Kreisdiagramm erstellt werden
  plot <- ggplot(shape_counts, aes(x = "", y = n, fill = shape)) +
    geom_bar(stat = "identity", width = 1) +  
    coord_polar(theta = "y", start = 0) + 
    scale_fill_manual(values = colors, labels = shape_labels) + 
    theme_minimal() +
    labs(
      title = paste("Verteilung der UFO-Formen für", year, "-", year + 4),
      fill = "UFO-Form"
    ) +
    theme(
      axis.text = element_blank(),  # Entfernt Achsen-Beschriftung
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA), 
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    guides(fill = guide_legend(override.aes = list(size = 5)))  # Größere Farbpunkte in der Legende
  
  return(plot)
}



