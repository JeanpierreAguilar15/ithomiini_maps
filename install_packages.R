# Lista de paquetes necesarios
packages <- c("stringr", "shiny", "dplyr", "DT", "leaflet", "grDevices", "htmlwidgets", "readxl")

# Instalar paquetes que no estén instalados
for(package in packages) {
  if(!require(package, character.only = TRUE)) {
    install.packages(package)
  }
} 