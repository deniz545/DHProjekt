# Die Funktionen dieses Files werden für diverse Vorverarbeitungsschritte verwendet

source("get_coordinates.R")
library(tidyr)
library(dplyr)

# first, replace the sighted_at and reported_at dates with just the years
# i want to analyze the UFO sightings on a yearly and not monthly basis
cut_dates_into_years <- function(dataframe) {
  # change the dates to just the years
  # i want to analyze the UFO Sightings on a yearly basis, the months and days are not that relevant
  dataframe$sighted_at <- (substr(dataframe$sighted_at, 1, 4))
  dataframe$reported_at <- (substr(dataframe$reported_at, 1, 4))
  
  return(dataframe)
}

# hier habe ich versucht die Location in Längen- und Breitegrade zu übersetzen --> könnte für eine detaillierte Karte verwendet werden
replace_location_by_coordinates <- function(dataframe) {
  return(translate_locations_into_coodinates(dataframe))
}

# filter all sightings which were in the USA
# with the location being either "City, State" or "City (further information), State"
filter_sightings_inUSA_or_notInUSA <- function(dataframe) {
  # Filterung der Sichtungsorten der USA --> sollten in solch einem Format vorliegen "Stadt,XY"
  sightings_in_usa <-
    dataframe[grepl("^[^,]+(?: \\([^)]+\\))?, [A-Za-z]{2}$", dataframe$location) |
                grepl("^[^,]+, [A-Za-z\\.]+, [A-Za-z]{2}$", dataframe$location),]
  
  # Es gab hier einige unnötige Inhalte in der Location, wie zum beispiel Klammer, diese habe ich ebenfalls entfernt
  
  sightings_in_usa$location <- remove_unnecessary_stuff_from_location(sightings_in_usa$location)
  
  # Filterung aller Einträge welche nicht in der USA sind --> Sind alle Einträge aus der originalen Liste, welche nich schon sightings_in_usa sind
  sightings_not_in_usa <-
    dataframe[!rownames(dataframe) %in% rownames(sightings_in_usa),]
  
  return(list(sightings_in_usa, sightings_not_in_usa))
}

# Diese Funktion dient der Konvertierung der Locations in Längen- und Breitengrad über geocode --> dauert lange und wird an sich nicht mehr verwendet
convert_location_into_coordinates_usa <- function(dataframe) {
  
  lat_longs <- dataframe %>% geocode(location, method="osm", lat= latitude, long=longitude)
  
  return(lat_longs)
}

# Entfernen unnötiger Inhalte aus der Location für einfachere Verarbeitung
remove_unnecessary_stuff_from_location <- function(text) {
  cleaned_location <- gsub("\\s*\\(.*?\\)", "", text)
  return(cleaned_location)
}

# Aufsplitten der Location in Stadt und Staat --> ist für spätere Filterung angenehmer 
split_location <- function(dataframe) {
  return(separate(dataframe, location, into = c("city", "state"), sep = ", ", remove = FALSE))
}

extract_state <- function(dataframe) {
  dataframe$state <- substr(dataframe$location, nchar(dataframe$location)-1, nchar(dataframe$location))
  
  return(dataframe)
}
# Überprüfen ob es sich um einen validen US-Bundesstaat handelt --> Abgleichen des extrahierten Staates mit einer vordefinierten Liste von US-Bundestaatn
check_if_valid_state <- function(dataframe) {
  valid_states = state.abb
  
   return(dataframe %>% filter(state %in% valid_states))
}

# Gruppieren der Sichtungen in 5 Jahres Abschnitte zur bessere Überprüfung, damit sollen dann alle Daten von beispielsweise 1990-1995 in einer Karte angezeigt und analysiert werden
group_years <- function(year) {
  return(5 * (year %/% 5))
}

# fügt die entsprechende Jahregruppe jedem Eintrag zu
add_year_group <- function(dataframe) {
  dataframe$sighted_at <- as.numeric(dataframe$sighted_at)
  dataframe$year_group <- sapply(dataframe$sighted_at, group_years)
  
  return(dataframe)
}




