# Mapa Interactivo de Mariposas Ithomiini

Una aplicación interactiva desarrollada en R Shiny para visualizar y explorar datos de distribución geográfica de mariposas de la tribu Ithomiini.

## Características

- **Visualización geoespacial**: Mapa interactivo con agrupación de puntos (clustering)
- **Filtros taxonómicos**: Búsqueda y filtrado por género, especie y subespecie
- **Filtros geográficos**: Selección por país y región
- **Interfaz moderna**: Diseñada con bslib y shinyWidgets para una experiencia de usuario mejorada
- **Análisis de datos**: Gráficos y resúmenes estadísticos de la distribución de especies

## Instalación

1. Clona este repositorio:
   ```
   git clone https://github.com/JeanpierreAguilar15/ithomiini_maps.git
   ```

2. Instala las dependencias necesarias:
   ```R
   # Instalar desde la consola de R
   install.packages(c("shiny", "leaflet", "dplyr", "plotly", "DT", "bslib", "shinyWidgets", "htmltools", "readxl", "janitor"))
   ```

3. Ejecuta la aplicación:
   ```R
   # Desde R Studio o la consola de R
   shiny::runApp("app.R")
   ```

## Estructura del proyecto

- `app.R`: Código principal de la aplicación Shiny
- `data_loader.R`: Script para cargar y procesar los datos de mariposas
- `install_packages.R`: Script para instalar todas las dependencias necesarias
- `rename_files.R`: Utilidad para renombrar archivos de datos

## Archivos de datos requeridos

La aplicación necesita los siguientes archivos de datos:
- `Dore_Ithomiini_data.xlsx`: Conjunto de datos principal de mariposas Ithomiini
- `Dore_Ithomiini_dataJ.csv`: Datos adicionales (anteriormente "Joana_Datos.csv")

## Fuentes de datos

La aplicación combina datos de dos fuentes principales:
- Registros DORE de mariposas Ithomiini
- Datos de Joana con registros adicionales

## Capturas de pantalla

[Incluir capturas de pantalla de la aplicación]

## Autor

- **Jeanpierre Aguilar** - [JeanpierreAguilar15](https://github.com/JeanpierreAguilar15)

## Licencia

Este proyecto está licenciado bajo la Licencia MIT - ver el archivo LICENSE para más detalles.

## Agradecimientos

- Datos proporcionados por el proyecto de investigación de mariposas Ithomiini
- Desarrollado con el framework Shiny para R 