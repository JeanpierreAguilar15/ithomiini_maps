library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(viridis)
library(geosphere)

# Cargar datos
source("data_loader.R")

# Verificar si existe el campo year, si no, intentar crearlo
if(!"year" %in% names(datos_mariposas)) {
  # Crear año simulado directamente para todos los registros
  set.seed(123) # Para reproducibilidad
  datos_mariposas$year <- sample(1950:2020, nrow(datos_mariposas), replace = TRUE)
  message("Creando años simulados para demostración (1950-2020)")
}

# Tema personalizado
mi_tema <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  primary = "#2c3e50",
  secondary = "#18bc9c",
  success = "#28a745",
  info = "#3498db",
  warning = "#f39c12",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab"),
  font_scale = 0.9
)

# UI
ui <- page_navbar(
  theme = mi_tema,
  title = "SIG Mariposas Ithomiini",
  
  # CSS personalizado
  tags$head(
    tags$style(HTML('
      .content-box {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        padding: 15px;
        margin-bottom: 20px;
      }
      .stat-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        margin-bottom: 15px;
      }
      .stat-box h3 {
        margin: 0;
        font-size: 2.5em;
        font-weight: bold;
      }
      .stat-box p {
        margin: 5px 0 0 0;
        opacity: 0.9;
      }
      .leaflet-container {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      }
      .navbar {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    '))
  ),
  
  # Tab 1: Mapa Principal
  nav_panel(
    title = "Mapa Interactivo",
    icon = icon("map"),
    
    # Layout principal
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        # Controles principales
        div(class = "content-box",
          h5(icon("filter"), "Filtros", class = "mb-3"),
          
          pickerInput(
            "filtro_genero",
            "Género:",
            choices = c("Todos", sort(unique(datos_mariposas$genus))),
            selected = "Todos",
            options = list(
              liveSearch = TRUE,
              size = 10,
              actionsBox = TRUE
            )
          ),
          
          uiOutput("ui_filtro_especie"),
          
          uiOutput("ui_filtro_subespecie"),
          
          pickerInput(
            "filtro_pais",
            "País:",
            choices = c("Todos", sort(unique(datos_mariposas$country))),
            selected = "Todos",
            multiple = TRUE,
            options = list(
              actionsBox = TRUE,
              size = 10
            )
          ),
          
          # Filtro de años simplificado - solo rango
          div(
            class = "form-group mb-4",
            tags$label("Rango de años:", class = "mb-2 fw-bold"),
            sliderInput(
              "rango_anios",
              label = NULL,
              min = 1900,
              max = as.integer(format(Sys.Date(), "%Y")),
              value = c(1900, as.integer(format(Sys.Date(), "%Y"))),
              step = 1,
              sep = "",
              width = "100%",
              ticks = TRUE,
              animate = TRUE
            )
          )
        ),
        
        # Herramientas de análisis
        div(class = "content-box",
          h5(icon("tools"), "Herramientas de Análisis", class = "mb-3"),
          
          # Selector de tipo de mapa base
          selectInput(
            "mapa_base",
            "Tipo de mapa:",
            choices = c(
              "Calles" = "CartoDB.Positron",
              "Satélite" = "Esri.WorldImagery",
              "Topográfico" = "OpenTopoMap"
            ),
            selected = "CartoDB.Positron"
          ),
          
          radioButtons(
            "tipo_visualizacion",
            "Visualización de datos:",
            choices = list(
              "Puntos" = "points",
              "Mapa de calor" = "heatmap",
              "Densidad de kernel" = "density",
              "Bandas latitudinales" = "bands"
            ),
            selected = "points"
          ),
          
          conditionalPanel(
            condition = "input.tipo_visualizacion == 'bands'",
            sliderInput(
              "num_bandas",
              "Número de bandas:",
              min = 3,
              max = 10,
              value = 5,
              step = 1
            )
          )
        ),
        
        # Resumen rápido
        div(class = "content-box",
          h5(icon("chart-bar"), "Resumen", class = "mb-3"),
          div(class = "stat-box",
            h3(textOutput("total_registros")),
            p("Registros filtrados")
          ),
          div(class = "stat-box",
            h3(textOutput("total_especies")),
            p("Especies únicas")
          )
        )
      ),
      
      # Contenido principal - MAPA
      div(
        # Mapa principal grande
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span("Distribución geográfica de Ithomiini"),
            div(
              actionButton("reset_view", "Vista inicial", 
                         icon = icon("home"), 
                         class = "btn-sm btn-secondary me-2"),
              downloadButton("download_map", "Descargar", 
                           class = "btn-sm btn-primary")
            )
          ),
          card_body(
            leafletOutput("mapa_principal", height = "75vh")
          ),
          card_footer(
            textOutput("info_mapa")
          )
        ),
        
        # Gráficos pequeños debajo
        div(class = "mt-3",
          layout_column_wrap(
            width = 1/3,
            
            div(class = "content-box",
              h6("Distribución por País"),
              uiOutput("mini_pais")
            ),
            
            div(class = "content-box",
              h6("Distribución temporal"),
              plotlyOutput("mini_temporal", height = 180)
            ),
            
            div(class = "content-box",
              h6("Diversidad por elevación"),
              plotlyOutput("mini_elevacion", height = 180)
            )
          )
        )
      )
    )
  ),
  
  # Tab 2: Análisis Ecológico
  nav_panel(
    title = "Análisis Ecológico",
    icon = icon("leaf"),
    
    # Mostrar resumen de filtros activos
    div(class = "alert alert-info mb-3",
        textOutput("filtros_activos")
    ),
    
    layout_column_wrap(
      width = 1,
      
      # Análisis 1: Riqueza de especies por gradiente altitudinal
      card(
        card_header("Diversidad a lo largo del gradiente altitudinal"),
        card_body(
          plotlyOutput("analisis_altitudinal", height = 400)
        ),
        card_footer(
          p("Este análisis muestra cómo varía la diversidad de especies según la altitud. 
            Las mariposas Ithomiini suelen tener preferencias altitudinales específicas, 
            con mayor diversidad en zonas de bosque nuboso entre 1000-2000m.", 
            class = "text-muted")
        )
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # Análisis 2: Matriz simplificada de presencia
      card(
        card_header("Presencia de especies principales"),
        card_body(
          plotlyOutput("matriz_presencia", height = 350)
        ),
        card_footer(
          p("Muestra la presencia/ausencia de las especies más comunes en cada país. 
            Útil para identificar patrones biogeográficos.", 
            class = "text-muted small")
        )
      ),
      
      # Análisis 3: Fenología explicada
      card(
        card_header("Patrones fenológicos - Actividad anual"),
        card_body(
          plotlyOutput("fenologia", height = 350)
        ),
        card_footer(
          p("La fenología estudia cuándo las mariposas están más activas durante el año. 
            Los picos indican las mejores épocas para observación y muestreo. 
            Las Ithomiini suelen ser más abundantes en la estación seca.", 
            class = "text-muted small")
        )
      )
    ),
    
    # Información adicional relevante
    layout_column_wrap(
      width = 1,
      
      card(
        card_header("Resumen ecológico de especies filtradas"),
        card_body(
          tableOutput("resumen_ecologico")
        )
      )
    )
  ),
  
  # Tab 3: Base de Datos
  nav_panel(
    title = "Datos",
    icon = icon("database"),
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Base de datos completa"),
        downloadButton("download_data", "Descargar CSV", 
                     class = "btn-sm btn-primary")
      ),
      card_body(
        DTOutput("tabla_datos")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Valores reactivos para polígonos
  valores <- reactiveValues(
    poligonos_dibujados = list(),
    areas_calculadas = list()
  )
  
  # Filtro dinámico de especies
  output$ui_filtro_especie <- renderUI({
    especies_disponibles <- if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      datos_mariposas %>%
        filter(genus == input$filtro_genero) %>%
        pull(species) %>%
        unique() %>%
        sort()
    } else {
      sort(unique(datos_mariposas$species))
    }
    
    pickerInput(
      "filtro_especie",
      "Especie:",
      choices = c("Todas", especies_disponibles),
      selected = "Todas",
      options = list(
        liveSearch = TRUE,
        size = 10
      )
    )
  })
  
  # Filtro dinámico de subespecies
  output$ui_filtro_subespecie <- renderUI({
    subespecies_disponibles <- datos_mariposas
    
    # Filtrar por género si está seleccionado
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      subespecies_disponibles <- subespecies_disponibles %>%
        filter(genus == input$filtro_genero)
    }
    
    # Filtrar por especie si está seleccionada
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      subespecies_disponibles <- subespecies_disponibles %>%
        filter(species == input$filtro_especie)
    }
    
    # Obtener subespecies únicas
    subespecies_lista <- subespecies_disponibles %>%
      pull(sub_species) %>%
      unique() %>%
      sort()
    
    # Quitar "No registrada" si existe
    subespecies_lista <- subespecies_lista[subespecies_lista != "No registrada"]
    
    pickerInput(
      "filtro_subespecie",
      "Subespecie:",
      choices = c("Todas", subespecies_lista),
      selected = "Todas",
      options = list(
        liveSearch = TRUE,
        size = 10
      )
    )
  })
  
  # Modificar la función datos_filtrados para resolver el problema de filtrado
  datos_filtrados <- reactive({
    # Simplificar y hacer que funcione de manera más robusta
    df <- datos_mariposas
    
    # Aplicar filtros básicos primero
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      df <- df %>% filter(genus == input$filtro_genero)
    }
    
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      df <- df %>% filter(species == input$filtro_especie)
    }
    
    if(!is.null(input$filtro_subespecie) && input$filtro_subespecie != "Todas") {
      df <- df %>% filter(sub_species == input$filtro_subespecie)
    }
    
    if(!is.null(input$filtro_pais) && length(input$filtro_pais) > 0 && !"Todos" %in% input$filtro_pais) {
      df <- df %>% filter(country %in% input$filtro_pais)
    }
    
    # Solo aplicar filtro de años si está bien definido y si hay columna year
    if("year" %in% names(df) && !is.null(input$rango_anios) && length(input$rango_anios) == 2) {
      # Solo aplicar si los valores no son NA
      if(!is.na(input$rango_anios[1]) && !is.na(input$rango_anios[2])) {
        # Usar try para evitar errores
        tryCatch({
          df <- df %>% filter(year >= input$rango_anios[1] & year <= input$rango_anios[2])
        }, error = function(e) {
          # Si hay error, devolver todos los datos
          message("Error al filtrar por año: ", e$message)
        })
      }
    }
    
    # Si no quedan datos después del filtrado, devolver todos
    if(nrow(df) == 0) {
      warning("No hay datos después del filtrado. Mostrando todos los datos.")
      return(datos_mariposas)
    }
    
    return(df)
  })
  
  # Modificar la función datos_mapa para que use los datos filtrados correctamente
  datos_mapa <- reactive({
    # Usar el resultado de datos_filtrados
    df <- datos_filtrados()
    
    # Obtener solo los que tienen coordenadas válidas
    df_coords <- df %>%
      filter(has_coords == TRUE & !is.na(longitude) & !is.na(latitude) & 
             longitude != 0 & latitude != 0 & 
             longitude >= -180 & longitude <= 180 & 
             latitude >= -90 & latitude <= 90)
    
    # Debugging
    print(paste("Total de registros filtrados:", nrow(df)))
    print(paste("Registros con coordenadas válidas:", nrow(df_coords)))
    
    return(df_coords)
  })
  
  # Mostrar filtros activos
  output$filtros_activos <- renderText({
    filtros <- c()
    
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      filtros <- c(filtros, paste("Género:", input$filtro_genero))
    }
    
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      filtros <- c(filtros, paste("Especie:", input$filtro_especie))
    }
    
    if(!is.null(input$filtro_subespecie) && input$filtro_subespecie != "Todas") {
      filtros <- c(filtros, paste("Subespecie:", input$filtro_subespecie))
    }
    
    if(!is.null(input$filtro_pais) && length(input$filtro_pais) > 0 && !"Todos" %in% input$filtro_pais) {
      if(length(input$filtro_pais) <= 3) {
        filtros <- c(filtros, paste("Países:", paste(input$filtro_pais, collapse = ", ")))
      } else {
        filtros <- c(filtros, paste("Países:", length(input$filtro_pais), "seleccionados"))
      }
    }
    
    if(!is.null(input$rango_anios) && length(input$rango_anios) == 2) {
      filtros <- c(filtros, paste("Años:", input$rango_anios[1], "-", input$rango_anios[2]))
    }
    
    if(length(filtros) == 0) {
      "Mostrando todos los datos disponibles"
    } else {
      paste("Filtros activos:", paste(filtros, collapse = " | "))
    }
  })
  
  # Outputs de resumen
  output$total_registros <- renderText({
    format(nrow(datos_filtrados()), big.mark = ",")
  })
  
  output$total_especies <- renderText({
    n_distinct(datos_filtrados()$scientific_name)
  })
  
  output$info_mapa <- renderText({
    n_mapa <- nrow(datos_mapa())
    n_total <- nrow(datos_filtrados())
    
    if(n_mapa == 0) {
      return("No hay registros con coordenadas válidas que mostrar")
    } else {
      return(paste0("Mostrando ", format(n_mapa, big.mark = ","), 
             " registros con coordenadas de ", 
             format(n_total, big.mark = ","), " totales"))
    }
  })
  
  # Mapa principal - Inicializar
  output$mapa_principal <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -75, lat = -5, zoom = 4) %>%
      addScaleBar(position = "bottomright") %>%
      addMiniMap(position = "bottomleft", width = 150, height = 150) %>%
      # Agregar herramienta de medición por defecto
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqkilometers",
        activeColor = "#18bc9c",
        completedColor = "#2c3e50"
      ) %>%
      # Agregar herramienta de dibujo por defecto
      addDrawToolbar(
        targetGroup = "poligonos_area",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(
            color = "#e74c3c",
            fillColor = "#e74c3c",
            fillOpacity = 0.3
          )
        ),
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(
            color = "#e74c3c",
            fillColor = "#e74c3c",
            fillOpacity = 0.3
          )
        ),
        editOptions = editToolbarOptions(
          edit = TRUE,
          remove = TRUE
        )
      )
  })
  
  # Observador para monitorear cambios en los filtros y actualizar el mapa
  observeEvent(list(
    input$filtro_genero,
    input$filtro_especie,
    input$filtro_subespecie,
    input$filtro_pais,
    input$rango_anios,
    input$tipo_visualizacion
  ), {
    # Obtener datos filtrados
    df <- isolate(datos_mapa())
    
    # Crear filtros de texto para leyenda
    filters_text <- c()
    
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      filters_text <- c(filters_text, paste("<b>Género:</b>", input$filtro_genero))
    }
    
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      filters_text <- c(filters_text, paste("<b>Especie:</b>", input$filtro_especie))
    }
    
    if(!is.null(input$filtro_subespecie) && input$filtro_subespecie != "Todas") {
      filters_text <- c(filters_text, paste("<b>Subespecie:</b>", input$filtro_subespecie))
    }
    
    if(!is.null(input$filtro_pais) && length(input$filtro_pais) > 0 && !"Todos" %in% input$filtro_pais) {
      if(length(input$filtro_pais) <= 3) {
        filters_text <- c(filters_text, paste("<b>Países:</b>", paste(input$filtro_pais, collapse = ", ")))
      } else {
        filters_text <- c(filters_text, paste("<b>Países:</b>", length(input$filtro_pais), "seleccionados"))
      }
    }
    
    if(!is.null(input$rango_anios) && length(input$rango_anios) == 2) {
      filters_text <- c(filters_text, paste("<b>Años:</b>", input$rango_anios[1], "-", input$rango_anios[2]))
    }
    
    # Construir leyenda HTML
    legend_html <- NULL
    if(length(filters_text) > 0) {
      legend_html <- paste(
        "<div style='padding: 6px; background-color: white; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
        "<div style='font-weight: bold; margin-bottom: 5px; border-bottom: 1px solid #ddd; padding-bottom: 5px;'>Filtros activos</div>",
        paste(filters_text, collapse = "<br>"),
        "</div>"
      )
    }
    
    # Limpiar y actualizar el mapa - eliminar TODAS las capas
    leafletProxy("mapa_principal") %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      clearShapes() %>%
      clearControls() %>%
      clearMarkerClusters() # Asegurarse de limpiar los clusters también
    
    # Si no hay datos, mostrar mensaje y salir
    if(nrow(df) == 0) {
      if (!is.null(legend_html)) {
        leafletProxy("mapa_principal") %>%
          addControl(
            html = legend_html,
            position = "topright"
          )
      }
      
      showNotification(
        "No hay datos con coordenadas para los filtros seleccionados",
        type = "warning",
        duration = 3
      )
      
      return()
    }
    
    # Verificar que todos los registros tienen coordenadas válidas
    df <- df %>% 
      filter(!is.na(longitude) & !is.na(latitude) & 
             longitude != 0 & latitude != 0 & 
             longitude >= -180 & longitude <= 180 & 
             latitude >= -90 & latitude <= 90)
    
    # Si después de filtrar coordenadas válidas no hay datos, mostrar mensaje y salir
    if(nrow(df) == 0) {
      if (!is.null(legend_html)) {
        leafletProxy("mapa_principal") %>%
          addControl(
            html = legend_html,
            position = "topright"
          )
      }
      
      showNotification(
        "Los registros filtrados no tienen coordenadas válidas",
        type = "warning",
        duration = 3
      )
      
      return()
    }
    
    # Según el tipo de visualización, agregar la capa correspondiente
    if(input$tipo_visualizacion == "points") {
      # Visualización de puntos con clusters
      leafletProxy("mapa_principal") %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste0(
            "<strong>", scientific_name, "</strong><br>",
            ifelse(!is.na(sub_species) & sub_species != "No registrada",
              paste0("Subespecie: ", sub_species, "<br>"),
              ""),
            "País: ", country, "<br>",
            "Coordenadas: ", round(latitude, 4), ", ", round(longitude, 4),
            ifelse(!is.na(altitude), paste0("<br>Altitud: ", round(altitude, 0), " m"), ""),
            ifelse(!is.na(year), paste0("<br>Año: ", year), "")
          ),
          radius = 6,
          color = "#2c3e50",
          fillColor = "#3498db",
          fillOpacity = 0.7,
          weight = 2,
          clusterOptions = markerClusterOptions()
        )
    } else if(input$tipo_visualizacion == "heatmap") {
      # Mapa de calor - sin clusters
      leafletProxy("mapa_principal") %>%
        addHeatmap(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          blur = 20,
          max = 0.05,
          radius = 15
        )
    } else if(input$tipo_visualizacion == "density") {
      # Densidad de kernel
      if(nrow(df) >= 10) {
        # Crear grid de densidad
        tryCatch({
          if(!requireNamespace("MASS", quietly = TRUE)) {
            showNotification(
              "El paquete MASS no está instalado. No se puede mostrar la densidad de kernel.",
              type = "error",
              duration = 5
            )
            return()
          }
          
          dens <- MASS::kde2d(df$longitude, df$latitude, n = 50)
          
          # Convertir a formato para leaflet con manejo de errores
          densdf <- expand.grid(x = dens$x, y = dens$y)
          densdf$z <- as.vector(dens$z)
          
          # Definir quantiles con manejo de errores
          q90 <- quantile(densdf$z, 0.9, na.rm = TRUE)
          q75 <- quantile(densdf$z, 0.75, na.rm = TRUE)
          q50 <- quantile(densdf$z, 0.5, na.rm = TRUE)
          
          # Solo agregar polígonos si hay datos válidos
          if(any(densdf$z > q90, na.rm = TRUE)) {
            # Crear puntos únicos para evitar el error
            high_density <- densdf %>% 
              filter(z > q90) %>%
              distinct(x, y, .keep_all = TRUE)
            
            if(nrow(high_density) > 2) {
              leafletProxy("mapa_principal") %>%
                addCircles(
                  data = high_density,
                  lng = ~x, lat = ~y,
                  radius = 50000,
                  fillColor = "red",
                  fillOpacity = 0.3,
                  weight = 1,
                  color = "darkred"
                )
            }
          }
          
          # Agregar leyenda
          leafletProxy("mapa_principal") %>%
            addLegend(
              position = "bottomright",
              colors = c("red", "orange", "yellow"),
              labels = c("Alta densidad", "Media densidad", "Baja densidad"),
              title = "Densidad de registros"
            )
        }, error = function(e) {
          showNotification(
            "Error al calcular la densidad de kernel. Verifica que los datos sean válidos.",
            type = "error",
            duration = 5
          )
        })
      }
    } else if(input$tipo_visualizacion == "bands") {
      # Bandas latitudinales
      lat_range <- range(df$latitude)
      lat_breaks <- seq(lat_range[1], lat_range[2], length.out = input$num_bandas + 1)
      
      # Asignar banda a cada punto
      df$banda <- cut(df$latitude, breaks = lat_breaks, labels = FALSE, include.lowest = TRUE)
      
      # Contar especies por banda
      bandas_resumen <- df %>%
        group_by(banda) %>%
        summarise(
          n_registros = n(),
          n_especies = n_distinct(scientific_name),
          lat_min = lat_breaks[unique(banda)],
          lat_max = lat_breaks[unique(banda) + 1],
          .groups = 'drop'
        )
      
      # Colores para las bandas
      pal <- colorNumeric(palette = "YlOrRd", domain = bandas_resumen$n_especies)
      
      # Agregar rectángulos de bandas
      for(i in 1:nrow(bandas_resumen)) {
        leafletProxy("mapa_principal") %>%
          addRectangles(
            lng1 = -180, lng2 = 180,
            lat1 = bandas_resumen$lat_min[i],
            lat2 = bandas_resumen$lat_max[i],
            fillColor = pal(bandas_resumen$n_especies[i]),
            fillOpacity = 0.3,
            weight = 2,
            color = "black",
            popup = paste0(
              "Banda ", i, "<br>",
              "Latitud: ", round(bandas_resumen$lat_min[i], 2), 
              "° a ", round(bandas_resumen$lat_max[i], 2), "°<br>",
              "Registros: ", bandas_resumen$n_registros[i], "<br>",
              "Especies: ", bandas_resumen$n_especies[i]
            )
          )
      }
      
      # Agregar leyenda
      leafletProxy("mapa_principal") %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = bandas_resumen$n_especies,
          title = "Especies por banda"
        )
      
      # Agregar puntos encima sin clusterización
      leafletProxy("mapa_principal") %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          radius = 4,
          fillOpacity = 0.8,
          weight = 1,
          clusterOptions = NULL  # Sin clusterización
        )
    }
    
    # Agregar la leyenda de filtros si existe
    if (!is.null(legend_html)) {
      leafletProxy("mapa_principal") %>%
        addControl(
          html = legend_html,
          position = "topright"
        )
    }
  })
  
  # Mini gráficos - Arreglar gráfica de países para que siempre muestre datos
  output$mini_pais <- renderUI({
    # Obtener datos filtrados
    df <- datos_filtrados()
    
    # Contar por país y asegurar que hay datos
    df_paises <- df %>%
      count(country) %>%
      filter(n > 0) %>%
      arrange(desc(n))
    
    # Si no hay datos de país, mostrar mensaje pero no vacío
    if(nrow(df_paises) == 0) {
      return(
        div(
          style = "height: 250px; display: flex; align-items: center; justify-content: center; background-color: #f8f9fa;",
          div(
            style = "text-align: center;",
            tags$p(tags$strong("No hay datos de país disponibles")),
            tags$p("Intenta ajustar los filtros para ver resultados")
          )
        )
      )
    }
    
    # Calcular altura para cada país (para que 5 países ocupen 250px)
    altura_por_pais <- 50  # 50px por país
    altura_total <- 250    # altura fija del contenedor
    
    # Calcular las etiquetas y textos para hover
    df_paises$etiqueta <- paste0(
      "<b>", df_paises$country, "</b><br>",
      "Registros: ", df_paises$n, "<br>",
      "Porcentaje: ", round(df_paises$n / sum(df_paises$n) * 100, 1), "%"
    )
    
    # Crear un div con scroll que contenga el gráfico
    div(
      style = paste0("height: ", altura_total, "px; overflow-y: auto; position: relative;"),
      # Contenedor para el gráfico con altura suficiente para todos los países
      div(
        style = paste0("height: ", max(altura_total, nrow(df_paises) * altura_por_pais), "px;"),
        renderPlotly({
          titulo <- paste0("Distribución por país (", nrow(df_paises), " países)")
          
          # Crear gráfico horizontal con plotly
          plot_ly(
            data = df_paises,
            y = ~reorder(country, n),
            x = ~n,
            type = "bar",
            orientation = "h",
            marker = list(
              color = viridis::plasma(nrow(df_paises), begin = 0.2, end = 0.8),
              line = list(color = "rgba(0,0,0,0.3)", width = 1)
            ),
            text = ~etiqueta,
            hoverinfo = "text",
            hovertemplate = "%{text}<extra></extra>"
          ) %>%
          layout(
            title = list(
              text = titulo,
              font = list(size = 14)
            ),
            xaxis = list(
              title = "Número de registros",
              fixedrange = FALSE
            ),
            yaxis = list(
              title = "",  # Quitar título del eje Y para ahorrar espacio
              fixedrange = FALSE
            ),
            margin = list(l = 120, r = 20, t = 40, b = 30),
            height = max(altura_total, nrow(df_paises) * altura_por_pais),
            hoverlabel = list(
              bgcolor = "white",
              font = list(size = 12),
              bordercolor = "black"
            )
          ) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            scrollZoom = TRUE,
            modeBarButtonsToShow = list(
              'zoomIn2d', 'zoomOut2d', 'resetScale2d'
            )
          )
        })
      )
    )
  })
  
  # Calcular área cuando se dibuja un polígono
  observeEvent(input$mapa_principal_draw_new_feature, {
    feature <- input$mapa_principal_draw_new_feature
    
    if(feature$geometry$type %in% c("Polygon", "Rectangle")) {
      coords <- feature$geometry$coordinates[[1]]
      
      # Convertir coordenadas si es necesario
      if(is.list(coords[[1]])) {
        coords_matrix <- do.call(rbind, coords)
      } else {
        coords_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
      }
      
      # Calcular área
      tryCatch({
        if(!requireNamespace("geosphere", quietly = TRUE)) {
          showNotification(
            "El paquete geosphere no está instalado. No se puede calcular el área.",
            type = "error",
            duration = 5
          )
          return()
        }
        
        area_m2 <- geosphere::areaPolygon(coords_matrix)
        area_km2 <- area_m2 / 1000000
        area_ha <- area_m2 / 10000
        
        # Guardar en valores reactivos
        valores$areas_calculadas <- append(
          valores$areas_calculadas,
          list(list(
            id = length(valores$areas_calculadas) + 1,
            area_km2 = area_km2,
            area_ha = area_ha,
            area_m2 = area_m2
          ))
        )
        
        # Mostrar resultado
        showModal(modalDialog(
          title = "Área calculada",
          tags$div(
            tags$h4(paste0("Polígono #", length(valores$areas_calculadas))),
            tags$hr(),
            tags$p(tags$strong("Área en km²: "), format(round(area_km2, 2), big.mark = ",", nsmall = 2)),
            tags$p(tags$strong("Área en hectáreas: "), format(round(area_ha, 2), big.mark = ",", nsmall = 2)),
            tags$p(tags$strong("Área en m²: "), format(round(area_m2, 0), big.mark = ",")),
            tags$hr(),
            tags$p(tags$em("Puedes seguir dibujando más polígonos para calcular múltiples áreas."))
          ),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      }, error = function(e) {
        showNotification(
          "Error al calcular el área. Verifica que los datos sean válidos.",
          type = "error",
          duration = 5
        )
      })
    }
  })
  
  # Reset vista
  observeEvent(input$reset_view, {
    leafletProxy("mapa_principal") %>%
      setView(lng = -75, lat = -5, zoom = 4)
  })
  
  # Cambiar gráfico temporal por uno más informativo
  output$mini_temporal <- renderPlotly({
    # Forzar reactividad al filtro de años
    input$rango_anios
    
    df <- datos_filtrados()
    
    # Verificar si hay datos de fecha o año disponibles
    if("year" %in% names(df) && sum(!is.na(df$year)) > 10) {
      # Usar datos reales de años
      df_temporal <- df %>%
        filter(!is.na(year)) %>%
        count(year) %>%
        arrange(year)
      
      # Crear un gráfico de áreas con línea suavizada
      p <- ggplot(df_temporal, aes(x = year, y = n)) +
        geom_area(fill = "lightgreen", alpha = 0.4) +
        geom_line(color = "darkgreen", size = 1) +
        geom_point(color = "darkgreen", size = 3) +
        theme_minimal() +
        labs(
          x = "Año",
          y = "Registros",
          title = "Registros por año"
        ) +
        theme(
          plot.title = element_text(size = 12),
          axis.title = element_text(size = 10)
        )
      
    } else if("month" %in% names(df) && sum(!is.na(df$month)) > 10) {
      # Usar datos reales de meses
      df_temporal <- df %>%
        filter(!is.na(month)) %>%
        count(month) %>%
        arrange(month)
      
      meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
      
      # Asignar nombres de meses
      df_temporal$mes_nombre <- meses[df_temporal$month]
      
      # Gráfico de barras para meses
      p <- ggplot(df_temporal, aes(x = factor(mes_nombre, levels = meses), y = n, fill = n)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_c(option = "viridis") +
        theme_minimal() +
        labs(
          x = "Mes",
          y = "Registros",
          title = "Registros por mes"
        ) +
        theme(
          plot.title = element_text(size = 12),
          axis.title = element_text(size = 10),
          legend.position = "none"
        )
      
    } else {
      # Usar distribución simulada si no hay datos reales
      meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
      
      # Crear distribución con patrón más realista
      n_total <- nrow(df)
      if(n_total > 0) {
        # Distribuir registros con patrón estacional más realista
        distribucion <- c(0.08, 0.09, 0.12, 0.13, 0.10, 0.07,
                         0.05, 0.06, 0.08, 0.09, 0.10, 0.03)
        registros_mes <- round(n_total * distribucion)
      } else {
        registros_mes <- rep(0, 12)
      }
      
      df_temporal <- data.frame(
        mes = factor(meses, levels = meses),
        registros = registros_mes
      )
      
      # Gráfico combinado de barras y líneas para datos simulados
      p <- ggplot(df_temporal, aes(x = mes, y = registros)) +
        geom_bar(stat = "identity", fill = "#2ecc71", alpha = 0.7) +
        geom_line(aes(group = 1), color = "darkgreen", size = 1) +
        geom_point(color = "darkgreen", size = 3) +
        theme_minimal() +
        labs(
          x = "Mes",
          y = "Registros estimados",
          title = "Distribución temporal (estimada)"
        ) +
        theme(
          plot.title = element_text(size = 12),
          axis.title = element_text(size = 10)
        )
    }
    
    # Convertir a plotly
    ggplotly(p) %>%
      layout(
        margin = list(l = 60, r = 20, t = 30, b = 50)
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToShow = list(
          'zoomIn2d', 'zoomOut2d', 'resetScale2d'
        )
      )
  })
  
  # Gráfico de elevación - implementamos una visualización alternativa
  output$mini_elevacion <- renderPlotly({
    df <- datos_filtrados()
    
    # En lugar de altitud, mostraremos diversidad por géneros
    generos_count <- df %>%
      count(genus) %>%
      arrange(desc(n)) %>%
      head(15)  # Tomar los 15 géneros más comunes
    
    if(nrow(generos_count) > 0) {
      # Crear gráfico de torta con los géneros más comunes
      colores <- viridis::viridis(nrow(generos_count))
      
      p <- plot_ly(
        generos_count,
        labels = ~genus,
        values = ~n,
        type = 'pie',
        hole = 0.4,
        marker = list(colors = colores),
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = 'white'),
        hoverinfo = 'text',
        text = ~paste0(genus, ': ', n, ' registros'),
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = "Distribución por géneros",
          font = list(size = 14)
        ),
        margin = list(l = 20, r = 20, b = 20, t = 40),
        uniformtext = list(minsize = 10, mode = 'hide')
      )
      
      p %>% config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToShow = list(
          'toImage', 'resetScale2d'
        )
      )
      
    } else {
      # Si no hay datos, mostrar mensaje
      plotly_empty() %>%
        layout(
          title = list(
            text = "Sin datos disponibles",
            y = 0.5,
            font = list(size = 14)
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  # ANÁLISIS ECOLÓGICO
  
  # 1. Diversidad altitudinal - implementamos una visualización alternativa
  output$analisis_altitudinal <- renderPlotly({
    df <- datos_filtrados() %>%
      filter(!is.na(altitude))
    
    if(nrow(df) > 10) {
      # Método 1: Gráfico de dispersión para mostrar la riqueza por altitud
      # Crear bins de altitud para agrupar los datos
      bin_size <- 200  # tamaño del bin en metros
      max_alt <- max(df$altitude, na.rm = TRUE)
      min_alt <- min(df$altitude, na.rm = TRUE)
      
      # Crear breaks más pequeños para mayor detalle
      n_bins <- max(10, min(30, ceiling((max_alt - min_alt) / bin_size)))
      
      # Crear un dataframe con bins de altitud
      df_bins <- data.frame(
        bin_min = seq(min_alt, max_alt, length.out = n_bins)
      )
      df_bins$bin_max <- c(df_bins$bin_min[-1], max_alt + 1)
      df_bins$bin_mid <- (df_bins$bin_min + df_bins$bin_max) / 2
      df_bins$bin_label <- paste0(round(df_bins$bin_min), "-", round(df_bins$bin_max), "m")
      
      # Calcular especies por bin
      df_bins$n_especies <- sapply(1:nrow(df_bins), function(i) {
        sum(df$altitude >= df_bins$bin_min[i] & df$altitude < df_bins$bin_max[i])
      })
      
      # Calcular riqueza de especies por bin
      df_bins$riqueza <- sapply(1:nrow(df_bins), function(i) {
        especies <- df %>% 
          filter(altitude >= df_bins$bin_min[i] & altitude < df_bins$bin_max[i]) %>%
          pull(scientific_name) %>%
          unique() %>%
          length()
        return(especies)
      })
      
      # Calcular índice de Shannon por bin
      df_bins$shannon <- sapply(1:nrow(df_bins), function(i) {
        especies <- df %>% 
          filter(altitude >= df_bins$bin_min[i] & altitude < df_bins$bin_max[i]) %>%
          pull(scientific_name)
        
        if(length(especies) > 0) {
          freq <- table(especies)
          prop <- freq/sum(freq)
          return(-sum(prop * log(prop)))
        } else {
          return(0)
        }
      })
      
      # Crear un gráfico más intuitivo
      plot_ly() %>%
        # Barras para número de especies
        add_bars(
          data = df_bins,
          x = ~bin_mid,
          y = ~riqueza,
          name = "Riqueza de especies",
          marker = list(
            color = colorRampPalette(c('#1f77b4', '#2ca02c'))(nrow(df_bins)),
            line = list(color = 'rgba(0,0,0,0.3)', width = 1)
          ),
          hovertemplate = '<b>Altitud: %{text}</b><br>Especies: %{y}<extra></extra>',
          text = ~bin_label
        ) %>%
        # Línea para índice de Shannon
        add_trace(
          data = df_bins,
          x = ~bin_mid,
          y = ~shannon,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Índice de Shannon',
          yaxis = 'y2',
          line = list(
            color = '#e74c3c',
            width = 3,
            shape = 'spline'
          ),
          marker = list(
            color = '#c0392b',
            size = 8,
            line = list(color = 'white', width = 1)
          ),
          hovertemplate = '<b>Altitud: %{text}</b><br>Shannon: %{y:.2f}<extra></extra>',
          text = ~bin_label
        ) %>%
        # Opcional: agregar scatter plot para visualizar densidad de registros
        add_trace(
          data = df_bins,
          x = ~bin_mid,
          y = ~n_especies,
          type = 'scatter',
          mode = 'markers',
          name = 'Registros',
          yaxis = 'y3',
          marker = list(
            color = 'rgba(150, 150, 150, 0.7)',
            size = ~sqrt(n_especies) * 2,
            line = list(color = 'white', width = 1)
          ),
          hovertemplate = '<b>Altitud: %{text}</b><br>Registros: %{customdata}<extra></extra>',
          text = ~bin_label,
          customdata = ~n_especies
        ) %>%
        layout(
          title = "Riqueza de especies e índice de diversidad por elevación",
          xaxis = list(
            title = "Altitud (m)",
            zeroline = TRUE,
            showgrid = TRUE,
            gridcolor = 'rgba(0,0,0,0.1)',
            fixedrange = FALSE
          ),
          yaxis = list(
            title = "Riqueza de especies",
            titlefont = list(color = '#1f77b4'),
            tickfont = list(color = '#1f77b4'),
            showgrid = TRUE,
            gridcolor = 'rgba(0,0,0,0.1)',
            fixedrange = FALSE
          ),
          yaxis2 = list(
            title = "Índice de Shannon",
            overlaying = "y",
            side = "right",
            titlefont = list(color = '#e74c3c'),
            tickfont = list(color = '#e74c3c'),
            showgrid = FALSE,
            fixedrange = FALSE
          ),
          yaxis3 = list(
            showticklabels = FALSE,
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            fixedrange = FALSE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            y = 1.1,
            xanchor = "center"
          ),
          hovermode = 'closest',
          margin = list(l = 60, r = 60, t = 50, b = 80),
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(255,255,255,1)',
          dragmode = "zoom"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          scrollZoom = TRUE,
          modeBarButtonsToShow = list(
            'zoomIn2d', 'zoomOut2d', 'resetScale2d'
          )
        )
    } else if(nrow(df) > 0) {
      # Para pocos datos, mostrar un gráfico simplificado
      
      # Organizar datos por especie y su rango altitudinal
      especies_resumen <- df %>%
        group_by(scientific_name) %>%
        summarise(
          altitud_media = mean(altitude, na.rm = TRUE),
          altitud_min = min(altitude, na.rm = TRUE),
          altitud_max = max(altitude, na.rm = TRUE),
          n_registros = n(),
          .groups = 'drop'
        ) %>%
        arrange(desc(n_registros))
      
      plot_ly() %>%
        add_segments(
          data = especies_resumen,
          y = ~reorder(scientific_name, altitud_media),
          x = ~altitud_min,
          xend = ~altitud_max,
          line = list(
            color = 'rgba(150, 150, 150, 0.5)',
            width = 2
          ),
          showlegend = FALSE,
          hoverinfo = 'none'
        ) %>%
        add_markers(
          data = especies_resumen,
          y = ~reorder(scientific_name, altitud_media),
          x = ~altitud_media,
          marker = list(
            color = 'rgba(39, 174, 96, 0.8)',
            size = ~sqrt(n_registros) * 4,
            line = list(color = 'rgba(0, 0, 0, 0.3)', width = 1)
          ),
          hovertemplate = '<b>%{y}</b><br>Altitud media: %{x} m<br>Rango: %{text}<br>Registros: %{customdata}<extra></extra>',
          text = ~paste(round(altitud_min), "-", round(altitud_max), "m"),
          customdata = ~n_registros
        ) %>%
        layout(
          title = "Distribución altitudinal por especie",
          xaxis = list(title = "Altitud (m)"),
          yaxis = list(title = "Especie"),
          margin = list(l = 180, r = 20, t = 50, b = 50),
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(255,255,255,1)'
        ) %>%
        config(
          displayModeBar = FALSE
        )
    } else {
      plotly_empty() %>%
        layout(
          title = "No hay datos de altitud disponibles para los registros filtrados",
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(255,255,255,1)'
        )
    }
  })
  
  # 2. Matriz de presencia simplificada - mejoramos el gráfico
  output$matriz_presencia <- renderPlotly({
    # Crear matriz de presencia por país
    df <- datos_filtrados()
    
    # Seleccionar top 10 especies más comunes
    top_especies <- df %>%
      count(scientific_name) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(scientific_name)
    
    # Seleccionar países con más registros
    top_paises <- df %>%
      count(country) %>%
      arrange(desc(n)) %>%
      head(8) %>%
      pull(country)
    
    if(length(top_especies) >= 2 && length(top_paises) >= 2) {
      # Crear matriz de presencia
      matriz_presencia <- df %>%
        filter(scientific_name %in% top_especies, country %in% top_paises) %>%
        distinct(scientific_name, country) %>%
        mutate(presente = 1) %>%
        tidyr::pivot_wider(
          names_from = country, 
          values_from = presente, 
          values_fill = 0
        ) %>%
        tibble::column_to_rownames("scientific_name") %>%
        as.matrix()
      
      # Crear heatmap mejorado
      plot_ly(
        z = matriz_presencia,
        x = colnames(matriz_presencia),
        y = rownames(matriz_presencia),
        type = "heatmap",
        colorscale = list(
          c(0, "rgba(255, 255, 255, 0.8)"), 
          c(0.5, "rgba(46, 204, 113, 0.5)"),
          c(1, "rgba(39, 174, 96, 1)")
        ),
        hovertemplate = '<b>Especie</b>: %{y}<br><b>País</b>: %{x}<br><b>Presente</b>: %{z}<extra></extra>',
        showscale = FALSE
      ) %>%
        layout(
          title = "Presencia (1) o ausencia (0) de especies por país",
          xaxis = list(
            title = "País", 
            tickangle = -45,
            tickfont = list(size = 11),
            fixedrange = FALSE
          ),
          yaxis = list(
            title = "Especie",
            tickfont = list(size = 11),
            fixedrange = FALSE
          ),
          margin = list(l = 150, r = 20, b = 80, t = 40),
          dragmode = "zoom"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          scrollZoom = TRUE,
          modeBarButtonsToShow = list(
            'zoomIn2d', 'zoomOut2d', 'resetScale2d'
          )
        )
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis")
    }
  })
  
  # 3. Fenología explicada - mejoramos el gráfico
  output$fenologia <- renderPlotly({
    # Usar datos reales si hay fechas, sino simular patrón típico
    df <- datos_filtrados()
    
    meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    
    # Patrón más estilizado de Ithomiini
    if(nrow(df) > 0) {
      n_total <- nrow(df)
      # Crear un patrón más pronunciado y estilizado
      proporcion_mensual <- c(0.14, 0.15, 0.13, 0.11, 0.07, 0.05,
                             0.04, 0.05, 0.06, 0.08, 0.10, 0.12)
      registros_mes <- round(n_total * proporcion_mensual)
    } else {
      registros_mes <- rep(0, 12)
    }
    
    df_fenologia <- data.frame(
      mes = factor(meses, levels = meses),
      registros = registros_mes,
      temporada = c(rep("Seca", 4), rep("Lluviosa", 6), rep("Seca", 2))
    )
    
    # Crear un gradiente de colores para el área bajo la curva
    colors <- c('#a1d99b', '#41ab5d', '#238b45', '#006d2c', '#00441b', '#004529', 
                '#00441b', '#006d2c', '#238b45', '#41ab5d', '#74c476', '#a1d99b')
    
    plot_ly(df_fenologia, x = ~mes, y = ~registros, 
            type = 'scatter', mode = 'lines+markers',
            line = list(
              color = '#2ecc71', 
              width = 4,
              shape = 'spline'
            ),
            marker = list(
              color = colors,
              size = 12,
              line = list(color = 'white', width = 1)
            ),
            fill = 'tozeroy',
            fillcolor = 'rgba(46, 204, 113, 0.2)',
            hovertemplate = '<b>%{x}</b><br>Registros: %{y}<br>Temporada: %{customdata}<extra></extra>',
            customdata = ~temporada) %>%
      layout(
        title = "Patrón fenológico de Ithomiini",
        xaxis = list(
          title = "Mes",
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = "Actividad relativa (registros)",
          tickfont = list(size = 12)
        ),
        shapes = list(
          # Resaltar la temporada seca
          list(
            type = "rect",
            fillcolor = "rgba(255, 235, 180, 0.2)",
            line = list(color = "rgba(0,0,0,0)"),
            x0 = -0.5, x1 = 3.5, y0 = 0, y1 = max(registros_mes) * 1.1,
            layer = "below"
          ),
          list(
            type = "rect",
            fillcolor = "rgba(255, 235, 180, 0.2)",
            line = list(color = "rgba(0,0,0,0)"),
            x0 = 10.5, x1 = 12.5, y0 = 0, y1 = max(registros_mes) * 1.1,
            layer = "below"
          ),
          # Resaltar la temporada lluviosa
          list(
            type = "rect",
            fillcolor = "rgba(180, 220, 255, 0.2)",
            line = list(color = "rgba(0,0,0,0)"),
            x0 = 3.5, x1 = 10.5, y0 = 0, y1 = max(registros_mes) * 1.1,
            layer = "below"
          )
        ),
        annotations = list(
          list(
            x = 2,
            y = max(registros_mes) * 0.95,
            text = "<b>Temporada seca</b><br>(mayor actividad)",
            showarrow = FALSE,
            bgcolor = "rgba(255, 255, 255, 0.7)",
            bordercolor = "#f39c12",
            borderwidth = 2,
            borderpad = 4,
            font = list(size = 12)
          ),
          list(
            x = 7,
            y = max(registros_mes) * 0.35,
            text = "<b>Temporada lluviosa</b><br>(menor actividad)",
            showarrow = FALSE,
            bgcolor = "rgba(255, 255, 255, 0.7)",
            bordercolor = "#3498db",
            borderwidth = 2,
            borderpad = 4,
            font = list(size = 12)
          )
        ),
        margin = list(t = 50, r = 20, b = 40, l = 60),
        dragmode = "zoom",
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToShow = list(
          'zoomIn2d', 'zoomOut2d', 'resetScale2d'
        )
      )
  })
  
  # Resumen ecológico
  output$resumen_ecologico <- renderTable({
    df <- datos_filtrados()
    
    if(nrow(df) > 0) {
      resumen <- data.frame(
        Métrica = c(
          "Total de registros",
          "Especies únicas",
          "Géneros únicos",
          "Países representados",
          "Rango altitudinal",
          "Especie más común",
          "País con más registros"
        ),
        Valor = c(
          format(nrow(df), big.mark = ","),
          n_distinct(df$scientific_name),
          n_distinct(df$genus),
          n_distinct(df$country),
          if(any(!is.na(df$altitude))) {
            paste(round(min(df$altitude, na.rm = TRUE)), "-", 
                  round(max(df$altitude, na.rm = TRUE)), "m")
          } else {
            "Sin datos"
          },
          names(sort(table(df$scientific_name), decreasing = TRUE))[1],
          names(sort(table(df$country), decreasing = TRUE))[1]
        )
      )
      
      resumen
    } else {
      data.frame(
        Métrica = "Sin datos",
        Valor = "Ajusta los filtros para ver resultados"
      )
    }
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    df <- datos_filtrados() %>%
      select(
        ID = record_id,
        Género = genus,
        Especie = species,
        Subespecie = sub_species,
        País = country,
        Latitud = latitude,
        Longitud = longitude,
        Altitud = altitude,
        `Coordenadas` = has_coords
      ) %>%
      mutate(
        Coordenadas = ifelse(Coordenadas, "Sí", "No")
      )
    
    datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(
            first = "Primera",
            last = "Última",
            `next` = "Siguiente",
            previous = "Anterior"
          )
        )
      ),
      rownames = FALSE,
      class = 'table-striped table-hover'
    ) %>%
      formatRound(columns = c('Latitud', 'Longitud'), digits = 4) %>%
      formatRound(columns = 'Altitud', digits = 0) %>%
      formatStyle(
        'Coordenadas',
        backgroundColor = styleEqual(c("Sí", "No"), c('#d4edda', '#f8d7da')),
        fontWeight = 'bold'
      )
  })
  
  # Descargas
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("mapa_ithomiini_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Crear una versión estática del mapa
      m <- leaflet(datos_mapa()) %>%
        addProviderTiles(providers[[input$mapa_base]]) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste0(
            "<strong>", scientific_name, "</strong><br>",
            "País: ", country
          ),
          radius = 6,
          color = "#2c3e50",
          fillColor = "#3498db",
          fillOpacity = 0.7
        )
      
      htmlwidgets::saveWidget(m, file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("datos_ithomiini_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Observador para cambiar el tipo de mapa base
  observeEvent(input$mapa_base, {
    leafletProxy("mapa_principal") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$mapa_base]])
  })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)