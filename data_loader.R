# data_loader.R - Sistema unificado de carga de datos
library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(stringr)
library(tidyr)

# Función auxiliar para limpiar y validar coordenadas
clean_coordinates <- function(df) {
  # Función auxiliar para convertir a numérico de forma segura
  safe_numeric <- function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }
  
  # Buscar columnas de latitud
  lat_cols <- grep("lat|latitude|decimal_latitude", names(df), ignore.case = TRUE, value = TRUE)
  if(length(lat_cols) > 0) {
    # Usar la primera columna encontrada
    df$latitude <- safe_numeric(df[[lat_cols[1]]])
  }
  
  # Buscar columnas de longitud
  lon_cols <- grep("lon|longitude|decimal_longitude", names(df), ignore.case = TRUE, value = TRUE)
  if(length(lon_cols) > 0) {
    # Usar la primera columna encontrada
    df$longitude <- safe_numeric(df[[lon_cols[1]]])
  }
  
  # Verificar que las columnas existan
  if(!"latitude" %in% names(df) || !"longitude" %in% names(df)) {
    warning("No se encontraron columnas de coordenadas válidas")
    df$latitude <- NA
    df$longitude <- NA
  }
  
  # IMPORTANTE: Ya NO filtramos registros sin coordenadas
  # Solo marcamos coordenadas inválidas como NA
  df <- df %>%
    mutate(
      latitude = ifelse(is.na(latitude) | latitude < -90 | latitude > 90, NA, latitude),
      longitude = ifelse(is.na(longitude) | longitude < -180 | longitude > 180, NA, longitude)
    )
  
  return(df)
}

# Función para estandarizar nombres de columnas
standardize_columns <- function(df) {
  # Convertir todos los nombres a minúsculas
  names(df) <- tolower(names(df))
  
  # Reemplazar puntos por guiones bajos
  names(df) <- gsub("\\.", "_", names(df))
  
  # Renombrar columnas específicas si existen
  if("subspecies" %in% names(df)) names(df)[names(df) == "subspecies"] <- "sub_species"
  if("sub_sp" %in% names(df)) names(df)[names(df) == "sub_sp"] <- "sub_species"
  if("subspecies_form" %in% names(df)) names(df)[names(df) == "subspecies_form"] <- "sub_species"
  
  if("specie" %in% names(df)) names(df)[names(df) == "specie"] <- "species"
  if("sp" %in% names(df)) names(df)[names(df) == "sp"] <- "species"
  
  if("gen" %in% names(df)) names(df)[names(df) == "gen"] <- "genus"
  
  if("pais" %in% names(df)) names(df)[names(df) == "pais"] <- "country"
  if("país" %in% names(df)) names(df)[names(df) == "país"] <- "country"
  
  if("collection_date" %in% names(df)) names(df)[names(df) == "collection_date"] <- "collection_date"
  if("fecha_colecta" %in% names(df)) names(df)[names(df) == "fecha_colecta"] <- "collection_date"
  
  if("collector" %in% names(df)) names(df)[names(df) == "collector"] <- "collector"
  if("colector" %in% names(df)) names(df)[names(df) == "colector"] <- "collector"
  
  if("altitude" %in% names(df)) names(df)[names(df) == "altitude"] <- "altitude"
  if("elevation" %in% names(df)) names(df)[names(df) == "elevation"] <- "altitude"
  if("altitud" %in% names(df)) names(df)[names(df) == "altitud"] <- "altitude"
  
  if("habitat" %in% names(df)) names(df)[names(df) == "habitat"] <- "habitat"
  
  if("host_plant" %in% names(df)) names(df)[names(df) == "host_plant"] <- "host_plant"
  if("planta_hospedera" %in% names(df)) names(df)[names(df) == "planta_hospedera"] <- "host_plant"
  
  return(df)
}

# Función para cargar datos desde Excel (DORE)
load_dore_data <- function() {
  tryCatch({
    if (file.exists("Dore_Ithomiini_records_with_ssp.xlsx")) {
      message("Cargando datos DORE desde Excel...")
      
      df <- read_excel("Dore_Ithomiini_records_with_ssp.xlsx")
      n_original <- nrow(df)
      
      df <- clean_names(df)
      df <- standardize_columns(df)
      df <- clean_coordinates(df)
      
      message(paste("  - Total registros DORE:", n_original))
      message(paste("  - Registros con coordenadas válidas:", sum(!is.na(df$latitude) & !is.na(df$longitude))))
      return(df)
    } else {
      warning("Archivo DORE Excel no encontrado")
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Error cargando datos DORE:", e$message))
    return(NULL)
  })
}

# Función para cargar y procesar datos de Joana
load_joana_data <- function() {
  tryCatch({
    if (file.exists("Joana_Datos.csv")) {
      message("Cargando datos de Joana desde CSV local...")
      
      # Intentar con diferentes separadores
      df <- NULL
      
      # Intentar con punto y coma
      tryCatch({
        df <- read.csv2("Joana_Datos.csv", stringsAsFactors = FALSE)
        if(ncol(df) > 1) {
          message("  - Archivo leído con separador ';'")
        } else {
          df <- NULL
        }
      }, error = function(e) { df <- NULL })
      
      # Si falla, intentar con coma
      if(is.null(df)) {
        tryCatch({
          df <- read.csv("Joana_Datos.csv", stringsAsFactors = FALSE)
          if(ncol(df) > 1) {
            message("  - Archivo leído con separador ','")
          } else {
            df <- NULL
          }
        }, error = function(e) { df <- NULL })
      }
      
      if(!is.null(df)) {
        n_original <- nrow(df)
        
        df <- clean_names(df)
        df <- standardize_columns(df)
        df <- clean_coordinates(df)
        
        message(paste("  - Total registros Joana:", n_original))
        message(paste("  - Registros con coordenadas válidas:", sum(!is.na(df$latitude) & !is.na(df$longitude))))
        return(df)
      } else {
        warning("No se pudo leer el archivo Joana_Datos.csv")
        return(NULL)
      }
    } else {
      warning("Archivo Joana_Datos.csv no encontrado")
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Error procesando datos de Joana:", e$message))
    return(NULL)
  })
}

# Función para cargar archivos CSV genéricos
load_generic_csv <- function(file_path) {
  tryCatch({
    df <- NULL
    
    # Intentar con coma
    tryCatch({
      df <- read_csv(file_path, show_col_types = FALSE)
      if (ncol(df) <= 1) df <- NULL
    }, error = function(e) { df <- NULL })
    
    # Intentar con punto y coma
    if(is.null(df)) {
      tryCatch({
        df <- read_csv2(file_path, show_col_types = FALSE)
        if (ncol(df) <= 1) df <- NULL
      }, error = function(e) { df <- NULL })
    }
    
    # Intentar con tab
    if(is.null(df)) {
      tryCatch({
        df <- read_tsv(file_path, show_col_types = FALSE)
        if (ncol(df) <= 1) df <- NULL
      }, error = function(e) { df <- NULL })
    }
    
    if (!is.null(df)) {
      df <- clean_names(df)
      df <- standardize_columns(df)
      df <- clean_coordinates(df)
      return(df)
    } else {
      warning(paste("No se pudo leer", file_path))
      return(NULL)
    }
    
  }, error = function(e) {
    warning(paste("Error leyendo", file_path, ":", e$message))
    return(NULL)
  })
}

# ============================================
# PROCESO PRINCIPAL DE CARGA DE DATOS
# ============================================

message("=== Iniciando carga de datos de mariposas Ithomiini ===")

# Lista para almacenar todos los dataframes
all_data <- list()

# 1. Cargar datos DORE
dore_data <- load_dore_data()
if (!is.null(dore_data) && nrow(dore_data) > 0) {
  all_data$dore <- dore_data
}

# 2. Cargar datos de Joana
joana_data <- load_joana_data()
if (!is.null(joana_data) && nrow(joana_data) > 0) {
  all_data$joana <- joana_data
}

# 3. Cargar otros archivos CSV en el directorio
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
csv_files <- csv_files[!grepl("Joana_Datos\\.csv", csv_files)]

if (length(csv_files) > 0) {
  message(paste("\nBuscando otros archivos CSV:", length(csv_files), "encontrados"))
  
  for (file in csv_files) {
    message(paste("  - Procesando:", basename(file)))
    df <- load_generic_csv(file)
    if (!is.null(df) && nrow(df) > 0) {
      all_data[[basename(file)]] <- df
    }
  }
}

# Combinar todos los datos
if (length(all_data) > 0) {
  message("\nCombinando todos los conjuntos de datos...")
  
  # Función para asegurar que todas las columnas existan
  ensure_columns <- function(df) {
    required_cols <- c("genus", "species", "sub_species", "country", 
                      "latitude", "longitude", "collection_date", 
                      "collector", "altitude", "habitat", "host_plant")
    
    for (col in required_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA
      }
    }
    
    return(df)
  }
  
  # Asegurar columnas en todos los dataframes
  all_data <- lapply(all_data, ensure_columns)
  
  # Combinar todos los dataframes
  datos_mariposas <- bind_rows(all_data, .id = "source_file")
  
  # Limpiar y procesar los datos combinados
  datos_mariposas <- datos_mariposas %>%
    mutate(
      # Limpiar texto
      genus = trimws(genus),
      species = trimws(species),
      sub_species = trimws(sub_species),
      country = trimws(country),
      
      # Capitalizar género correctamente
      genus = ifelse(is.na(genus) | genus == "", "Unknown",
                    paste0(toupper(substr(genus, 1, 1)), 
                          tolower(substr(genus, 2, nchar(genus))))),
      
      # Species en minúsculas
      species = ifelse(is.na(species) | species == "", "sp.", tolower(species)),
      
      # Manejar subespecies vacías
      sub_species = ifelse(is.na(sub_species) | sub_species == "" | sub_species == "NA", 
                          "No registrada", sub_species),
      
      # País
      country = ifelse(is.na(country) | country == "", "No especificado", country),
      
      # Crear nombre científico
      scientific_name = paste(genus, species),
      
      # ID único
      record_id = row_number()
    )
  
  # Agregar campos calculados para análisis SIG
  datos_mariposas <- datos_mariposas %>%
    mutate(
      # Zona latitudinal (solo para registros con coordenadas)
      lat_zone = case_when(
        is.na(latitude) ~ "Sin coordenadas",
        latitude < -23.5 ~ "Zona templada sur",
        latitude < 0 ~ "Zona tropical sur",
        latitude < 23.5 ~ "Zona tropical norte",
        TRUE ~ "Zona templada norte"
      ),
      
      # Categoría de elevación
      elevation_category = case_when(
        is.na(altitude) ~ "Sin datos",
        altitude < 500 ~ "Tierras bajas",
        altitude < 1000 ~ "Premontano bajo",
        altitude < 2000 ~ "Premontano",
        altitude < 3000 ~ "Montano bajo",
        TRUE ~ "Montano alto"
      ),
      
      # Indicador de coordenadas válidas
      has_coords = !is.na(latitude) & !is.na(longitude)
    )
  
  # NO eliminar duplicados - mantener todos los registros
  datos_mariposas <- datos_mariposas %>%
    arrange(genus, species, sub_species)
  
  # Estadísticas finales
  message("\n=== Resumen de datos cargados ===")
  message(paste("Total de registros:", format(nrow(datos_mariposas), big.mark = ",")))
  message(paste("Registros con coordenadas válidas:", format(sum(datos_mariposas$has_coords), big.mark = ",")))
  message(paste("Registros sin coordenadas:", format(sum(!datos_mariposas$has_coords), big.mark = ",")))
  message(paste("Géneros únicos:", n_distinct(datos_mariposas$genus)))
  message(paste("Especies únicas:", n_distinct(datos_mariposas$scientific_name)))
  
  n_subspecies <- sum(datos_mariposas$sub_species != "No registrada")
  message(paste("Registros con subespecie:", format(n_subspecies, big.mark = ",")))
  
  message(paste("Países:", n_distinct(datos_mariposas$country)))
  
  if(any(!is.na(datos_mariposas$altitude))) {
    message(paste("Rango altitudinal:", 
                  round(min(datos_mariposas$altitude, na.rm = TRUE)), "-", 
                  round(max(datos_mariposas$altitude, na.rm = TRUE)), "m"))
  }
  
  # Mostrar fuentes de datos
  message("\nDistribución por fuente de datos:")
  print(table(datos_mariposas$source_file))
  
} else {
  # Crear dataframe vacío si no hay datos
  warning("No se pudieron cargar datos. Creando estructura vacía...")
  
  datos_mariposas <- data.frame(
    record_id = integer(),
    source_file = character(),
    genus = character(),
    species = character(),
    sub_species = character(),
    scientific_name = character(),
    country = character(),
    latitude = numeric(),
    longitude = numeric(),
    collection_date = as.Date(character()),
    collector = character(),
    altitude = numeric(),
    habitat = character(),
    host_plant = character(),
    lat_zone = character(),
    elevation_category = character(),
    has_coords = logical(),
    stringsAsFactors = FALSE
  )
}

message("\n=== Carga de datos completada ===\n")