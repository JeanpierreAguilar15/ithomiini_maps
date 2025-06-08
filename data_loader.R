# ===============================================
# DATA LOADER - SOLO DATOS DE JOANA
# ===============================================

# Load required libraries
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Set locale for date parsing
Sys.setlocale(locale = "en_US.UTF-8")

# Define constants for Joana's Google Sheets
gsheet_id <- "1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4"
sheets <- c("Collection_data", "Location_data", "Photo_links")
rds_paths <- setNames(paste0(sheets, ".rds"), sheets)

# Function to download and save data from Google Sheets
download_and_save_data <- function() {
  cat("Descargando datos de Google Sheets...\n")
  
  # Download data from Google Sheets
  data_list <- lapply(sheets, function(sheet_name) {
    cat("Descargando hoja:", sheet_name, "\n")
    url <- paste0("https://docs.google.com/spreadsheets/d/", gsheet_id, "/gviz/tq?tqx=out:csv&sheet=", URLencode(sheet_name))
    df <- read_csv(url, col_types = cols(.default = "c"))
    return(df)
  })
  
  # Name the data frames
  names(data_list) <- sheets
  
  # Save as RDS files
  mapply(saveRDS, data_list, rds_paths)
  
  cat("Datos descargados y guardados exitosamente!\n")
  return(data_list)
}

# Function to process date columns
process_date_columns <- function(df) {
  date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
  df %>%
    mutate(across(all_of(date_cols), ~ as.Date(., format = "%d-%b-%y")))
}

# Function to process data
process_data <- function(data) {
  # Process photo links
  data$Photo_links <- data$Photo_links %>%
    mutate(URL_to_view = gsub("https://drive.google.com/file/d/(.*)/view\\?usp=drivesdk",
                              "https://drive.google.com/thumbnail?id=\\1&sz=w2000", URL),
           CAM_ID = str_extract(Name, ".*(?=[dv]\\.JPG)"))
  
  # Create Dorsal and Ventral links
  create_links <- function(side) {
    data$Photo_links %>%
      filter(str_detect(Name, paste0(side, "\\.JPG"))) %>%
      select(CAM_ID, URL = URL_to_view) %>%
      rename_with(~ paste0("URL", side), "URL")
  }
  
  Dorsal_links <- create_links("d")
  Ventral_links <- create_links("v")
  
  # Process coordinates
  coord <- data$Location_data %>% 
    select(COLLECTION_LOCATION, lat=DECIMAL_LATITUDE, long=DECIMAL_LONGITUDE)
  
  # Process main collection data
  data$Collection_data <- data$Collection_data %>%
    process_date_columns() %>%
    mutate(CAM_ID = if_else(!is.na(CAM_ID_insectary) & CAM_ID_insectary != "NA", CAM_ID_insectary, CAM_ID)) %>%
    left_join(Dorsal_links, by = "CAM_ID") %>%
    left_join(Ventral_links, by = "CAM_ID") %>%
    mutate(Preservation_date_formatted = format(as.Date(Preservation_date), "%d/%b/%Y")) %>% 
    rename(Species = SPECIES) %>%
    mutate(ID_status = if_else(is.na(ID_status), "NA", ID_status)) %>% 
    left_join(coord, by = "COLLECTION_LOCATION")
  
  return(data)
}

# Function to convert processed data to the format expected by the app
convert_to_app_format <- function(processed_data) {
  cat("Convirtiendo datos al formato de la aplicación...\n")
  
  collection_data <- processed_data$Collection_data
  
  # Convert to the format expected by the Shiny app
  species_data <- collection_data %>%
    select(
      Genus,
      Species,
      `Sub.species` = Subspecies_Form,
      Latitude = lat,
      Longitude = long,
      Country = Country,
      Subfamily = Subfamily,
      Tribe = Tribe,
      Collection_date
    ) %>%
    mutate(
      Source = "Joana",  # All data is from Joana
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      # Create collection_year from Collection_date
      collection_year = as.numeric(format(as.Date(Collection_date), "%Y"))
    ) %>%
    # Remove rows with missing coordinates
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    # Remove duplicate rows
    distinct(Genus, Species, `Sub.species`, Latitude, Longitude, .keep_all = TRUE)
  
  cat("Total de registros procesados:", nrow(species_data), "\n")
  cat("Registros con coordenadas válidas:", sum(!is.na(species_data$Latitude) & !is.na(species_data$Longitude)), "\n")
  cat("Rango de años:", min(species_data$collection_year, na.rm = TRUE), "-", max(species_data$collection_year, na.rm = TRUE), "\n")
  
  return(species_data)
}

# Main function to load complete dataset
load_complete_dataset <- function() {
  cat("=== CARGANDO DATOS DE JOANA ===\n")
  
  # Load or download raw data
  if (!all(file.exists(unlist(rds_paths)))) {
    cat("Archivos RDS no encontrados. Descargando desde Google Sheets...\n")
    rawData <- download_and_save_data()
  } else {
    cat("Cargando datos desde archivos RDS locales...\n")
    rawData <- lapply(rds_paths, readRDS)
    names(rawData) <- names(rds_paths)
  }
  
  # Process the data
  cat("Procesando datos...\n")
  processed_data <- process_data(rawData)
  
  # Convert to app format
  final_data <- convert_to_app_format(processed_data)
  
  cat("=== CARGA COMPLETADA ===\n")
  cat("Dataset final: ", nrow(final_data), "registros\n")
  
  return(final_data)
}

# Alternative function to force refresh from Google Sheets
refresh_from_google_sheets <- function() {
  cat("=== FORZANDO ACTUALIZACIÓN DESDE GOOGLE SHEETS ===\n")
  
  # Delete existing RDS files to force re-download
  existing_files <- file.exists(unlist(rds_paths))
  if(any(existing_files)) {
    file.remove(unlist(rds_paths)[existing_files])
    cat("Archivos RDS anteriores eliminados.\n")
  }
  
  # Download fresh data
  return(load_complete_dataset())
} 