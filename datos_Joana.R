# Load required libraries
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Set locale for date parsing
Sys.setlocale(locale = "en_US.UTF-8")

# Define constants
gsheet_id <- "1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4"
sheets <- c("Collection_data", "Location_data", "Photo_links")
rds_paths <- setNames(paste0(sheets, ".rds"), sheets)

# Function to download and save data
download_and_save_data <- function() {
  # Download data from Google Sheets
  data_list <- lapply(sheets, function(sheet_name) {
    
    url <- paste0("https://docs.google.com/spreadsheets/d/", gsheet_id, "/gviz/tq?tqx=out:csv&sheet=", URLencode(sheet_name))
    df <- read_csv(url, col_types = cols(.default = "c"))
    
    return(df)
  })
  
  # Name the data frames
  names(data_list) <- sheets
  
  # Save as RDS files
  mapply(saveRDS, data_list, rds_paths)
  
  print("Data downloaded and saved successfully!")
  
  return(data_list)
}

# Function to process date columns
process_date_columns <- function(df) {
  date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
  df %>%
    mutate(across(all_of(date_cols), ~ as.Date(., format = "%d-%b-%y")))
}

# Function to process date columns
process_date_columns <- function(df) {
  date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
  df %>%
    mutate(across(all_of(date_cols), ~ as.Date(., format = "%d-%b-%y")))
}

# Function to process data
process_data <- function(data) {
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
  
  coord = data$Location_data %>% select(COLLECTION_LOCATION, lat=DECIMAL_LATITUDE, long=DECIMAL_LONGITUDE)
  
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

# Load or download raw data
if (!all(file.exists(unlist(rds_paths)))) {
  rawData <- download_and_save_data()
} else {
  rawData <- lapply(rds_paths, readRDS)
  names(rawData) <- names(rds_paths)
}


# Process the data
data <- process_data(rawData)

#Datos Joana
Joana_Datos = data$Collection_data %>% 
  select(Genus, Species, Sub.species = Subspecies_Form,  Latitude = lat, Longitude = long, Country) %>% #Filtrar por los campos del Archivo DORE
  filter(if_any(everything(), ~!is.na(.))) # Eliminar solo filas donde todos los valores son NA

# Guardar Joana_Datos en CSV con separador ;
write.csv2(Joana_Datos, "Joana_Datos.csv", row.names = FALSE, fileEncoding = "UTF-8")


