library(ggplot2)

# File path to the folder which should contain the data
# which can be found here: https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/master/01-Introduction/data/ufo/ufo_awesome.tsv
UFO_DATASET_PATH <- "data/UFO_Sightings/ufo_awesome.tsv"
UFO_SIGHTINGS_MAPS_PATH <- "data/maps/"

source("read_UFO_Sightings.R")
source("prepare_ufo_data.R")
source("generate_chloropleth_map.R")
source("fileHandling.R")
source("visualisations.R")

# read the data
ufo_data <- read_UFO_data(UFO_DATASET_PATH)

# clean the reported_at and sighted_at dates, i think i will just need the years and not the months and days
ufo_data <- cut_dates_into_years(ufo_data)

# now split the dataframe into 2 dataframes, one contains all sightings which happend in the USA and the other contains all that happended somewhere else on the world
filtered_sightings <- filter_sightings_inUSA_or_notInUSA(ufo_data)
# in filtered_sightings[1] are all sightings which were in the USA
sightings_in_usa <- filtered_sightings[[1]]
# in filtered_sightings[2] are all sightings which were not the USA
sightings_outside_usa <- filtered_sightings[[2]]

# extract the state
# sightings_in_usa$state <- substr(sightings_in_usa$location, nchar(sightings_in_usa$location)-1, nchar(sightings_in_usa$location))
sightings_in_usa <- extract_state(sightings_in_usa)
# Überprüfe ob die Location auch tatsächlich ein US-Bundesstaat ist
sightings_in_usa <- check_if_valid_state(sightings_in_usa)

# Lösche Einträge, welche bei sighted_at einen leeren Eintrag haben oder sowas wie '0000' --> möglicherweise fehlerhafte und unbrauchbare Daten
sightings_in_usa <- sightings_in_usa[sightings_in_usa$sighted_at != "0000" & !is.na(sightings_in_usa$sighted_at), ]
# füge eine Jahresgruppierung hinzu --> dient später zur einfacheren Analyse und Filterubng
sightings_in_usa <- add_year_group(sightings_in_usa)

# ufo_summary gibt die Anzahl der Sichtungen für jeden Bundesstaat in einem spezifisches 5 Jahres Abschnitt an, year_group 1990 bedeutet, das alle Sichtungen für die Jahre 1990-1994 in den Bundesstaaten gezählt werden
ufo_summary <- sightings_in_usa %>%
  group_by(year_group, state) %>%
  summarise(sightings = n(), .groups = "drop")

# generieren der Karten und Diagramme für die Zeiträume des Zeitraums 1900  bis 2010
for (year in seq(1900, 2010, by = 5)) {
  # erstelle die Karte für diesen Zeitraum und speichere diese unter dem Pfad /data/maps ab --> de Pfad sollte für Windwos angepasst werden, habe irgendwie darauf gar nicht geachtet 
  plot <- plot_choropleth(year, ufo_summary)
  file_name <- paste0(UFO_SIGHTINGS_MAPS_PATH, year, "_UFO_Sichtungen.png")
  ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 300)
  
  # erstelle ein Balkendiagramm für die Sichtungedauer und speichere dieses ab
  chart_durations <- generate_chart_durations(year,sightings_in_usa)
  ggsave(
    filename = paste0(UFO_SIGHTINGS_MAPS_PATH, year, "_durations.png"),  # Dateipfad & Name
    plot = chart_durations,  
    width = 10, height = 6, dpi = 300  # Größe und Qualität anpassen
  )
  # erstellen ein Kreisdiagramm für die UFO-Formen und speichere dieses ab
  chart_shapes <- generate_chart_shape(year, sightings_in_usa)
  
  ggsave(
    filename = paste0(UFO_SIGHTINGS_MAPS_PATH, year, "_shapes.png"),  # Dateipfad & Name
    plot = chart_shapes,  
    width = 10, height = 6, dpi = 300  # Größe und Qualität anpassen
  )
}