# Script para instalar todos los paquetes necesarios para el Mapa Interactivo de Mariposas Ithomiini

# Lista de paquetes necesarios
packages <- c(
  # Paquetes principales
  "shiny",
  "leaflet",
  "dplyr",
  "plotly",
  "DT",
  "bslib",
  "shinyWidgets",
  "htmltools",
  
  # Paquetes para datos
  "readxl",
  "janitor",
  "tidyr",
  "stringr",
  "readr",
  
  # Paquetes para visualización
  "RColorBrewer",
  "viridis",
  "leaflet.extras"
)

# Función para instalar paquetes si no están instalados
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Instalando paquete: ", pkg, "\n"))
    install.packages(pkg)
  } else {
    cat(paste0("El paquete ", pkg, " ya está instalado.\n"))
  }
}

# Instalar paquetes faltantes
cat("=== Comprobando e instalando paquetes necesarios ===\n")
for (pkg in packages) {
  install_if_missing(pkg)
}
cat("=== Instalación completada ===\n")

# Cargar los paquetes principales
cat("=== Cargando paquetes principales ===\n")
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
cat("=== Paquetes cargados correctamente ===\n")

cat("\n¡Listo! Todos los paquetes necesarios están instalados.\n")
cat("Para ejecutar la aplicación, use: shiny::runApp('app.R')\n") 