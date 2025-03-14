library(readr)

# Diese Funktion liest die originalen Daten in einen Dateframe
read_UFO_data <- function(path) {
  tryCatch({

    ufo_data <- read_tsv(path, col_names = FALSE)
    
    # Vergeben von Spaltennamen
    colnames(ufo_data) <-
      c("sighted_at",
        "reported_at",
        "location",
        "shape",
        "duration",
        "description")
    
    # hier gab es es warum auch immer unnötige Whitespaces im reported_at datum
    ufo_data$reported_at <- trimws(ufo_data$reported_at, "left")
    
    
    return(as.data.frame(ufo_data))
  }, error =  function(e) {
    print(
      paste(
        "Error (Seems like there was problem with the file, check if it exists in the data folder):",
        e
      )
    )
    stop("Make sure the UFO Data is the right data folder")
  })
}
