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

# Load required libraries
library(dplyr)
library(readr)
library(readxl)

# Read the Collection_data
data_rds <- data$Collection_data

# Read the Excel data
data_excel <- read_excel("Dore_Ithomiini_Data.xlsx")

# Process RDS data
species_data_rds <- data_rds %>%
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
    Source = "Joana",  # Add source column for RDS data
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Process Excel data
species_data_excel <- data_excel %>%
  select(
    Genus,
    Species,
    `Sub.species`,
    Latitude,
    Longitude,
    Country
  ) %>%
  # Add missing columns with NA
  mutate(
    Subfamily = NA,
    Tribe = NA,
    Source = "Dore",  # Add source column for Excel data
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Collection_date = NA  # Add Collection_date column with NA values
  )

# Combine both datasets
species_data_combined <- bind_rows(species_data_rds, species_data_excel)

# Remove any duplicate rows based on Genus, Species, Sub.species, Latitude, and Longitude
species_data_combined <- species_data_combined %>%
  distinct(Genus, Species, `Sub.species`, Latitude, Longitude, .keep_all = TRUE)

# Write to CSV
write_csv(species_data_combined, "species_data_combined.csv")

print("Combined CSV file has been created successfully!")
print(paste("Total rows:", nrow(species_data_combined)))
print(paste("Rows from Joana:", nrow(species_data_rds)))
print(paste("Rows from Dore:", nrow(species_data_excel))) 


