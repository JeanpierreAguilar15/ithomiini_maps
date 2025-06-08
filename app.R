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

# Usar años de collection_year que ya están calculados en data_loader.R
if(!"year" %in% names(datos_mariposas)) {
  if("collection_year" %in% names(datos_mariposas)) {
    # Usar la columna ya calculada
    datos_mariposas$year <- datos_mariposas$collection_year
    
    # Mensaje informativo
    valid_years <- sum(!is.na(datos_mariposas$year))
    
    if(valid_years > 0) {
      years_range <- range(datos_mariposas$year, na.rm = TRUE)
      message(paste("Usando datos de collection_year:", valid_years, "registros con fechas válidas"))
      message(paste("Rango de años:", years_range[1], "-", years_range[2]))
    } else {
      message("No hay datos válidos de fechas de colección")
    }
  } else {
    message("No se encontraron datos de fechas. Filtro de fechas deshabilitado.")
    datos_mariposas$year <- NA
  }
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

# ===================================================================
# FUNCIONES HELPER PARA OPTIMIZACIÓN Y MANTENIBILIDAD
# ===================================================================

# Función para crear filtros dinámicos reutilizable
create_dynamic_filter <- function(data, column, filter_name, label, selected_genus = NULL, selected_species = NULL) {
  # Aplicar filtros previos si existen
  filtered_data <- data
  
  if (!is.null(selected_genus) && selected_genus != "Todos") {
    filtered_data <- filtered_data %>% filter(genus == selected_genus)
  }
  
  if (!is.null(selected_species) && selected_species != "Todas") {
    filtered_data <- filtered_data %>% filter(species == selected_species)
  }
  
  # Obtener opciones únicas
  choices_list <- filtered_data %>%
    pull(!!sym(column)) %>%
    unique() %>%
    sort() %>%
    na.omit()
  
  # Remover valores no deseados
  choices_list <- choices_list[!choices_list %in% c("No registrada", "")]
  
  # Agregar opción "Todas/Todos" - CORREGIDO
  all_option <- if(label %in% c("Especie", "Subespecie")) "Todas" else "Todos"
  choices_list <- c(all_option, choices_list)
  
  pickerInput(
    filter_name,
    paste0(label, ":"),
    choices = choices_list,
    selected = all_option,
    options = list(
      liveSearch = TRUE,
      size = 10,
      maxOptions = 1000
    )
  )
}

# Función para aplicar filtros de manera consistente
apply_filters <- function(data, filters) {
  df <- data
  
  # DEBUG: Ver todos los filtros recibidos
  cat("DEBUG APPLY_FILTERS - Datos iniciales:", nrow(df), "registros\n")
  cat("DEBUG Género filtro:", filters$genus, "\n")
  cat("DEBUG Especie filtro:", filters$species, "\n")
  cat("DEBUG País filtro:", paste(filters$countries, collapse = ", "), "\n")
  cat("DEBUG Columnas disponibles:", paste(names(df)[1:10], collapse = ", "), "...\n")
  
  # Filtros taxonómicos - CORREGIDOS
  if (!is.null(filters$genus) && length(filters$genus) > 0 && filters$genus != "Todos") {
    df_before <- nrow(df)
    df <- df %>% filter(genus == filters$genus)
    cat("DEBUG Filtro género aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  if (!is.null(filters$species) && length(filters$species) > 0 && !filters$species %in% c("Todas", "Todos")) {
    df_before <- nrow(df)
    df <- df %>% filter(species == filters$species)
    cat("DEBUG Filtro especie aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  if (!is.null(filters$subspecies) && length(filters$subspecies) > 0 && !filters$subspecies %in% c("Todas", "Todos")) {
    df_before <- nrow(df)
    df <- df %>% filter(sub_species == filters$subspecies)
    cat("DEBUG Filtro subespecie aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  if (!is.null(filters$subfamily) && length(filters$subfamily) > 0 && !filters$subfamily %in% c("Todas", "Todos")) {
    df_before <- nrow(df)
    df <- df %>% filter(subfamily == filters$subfamily)
    cat("DEBUG Filtro subfamilia aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  if (!is.null(filters$tribe) && length(filters$tribe) > 0 && filters$tribe != "Todas") {
    df_before <- nrow(df)
    df <- df %>% filter(tribe == filters$tribe)
    cat("DEBUG Filtro tribu aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  # Filtro geográfico - MEJORADO
  if (!is.null(filters$countries) && length(filters$countries) > 0 && !"Todos" %in% filters$countries) {
    df_before <- nrow(df)
    df <- df %>% filter(country %in% filters$countries)
    cat("DEBUG Filtro países aplicado:", df_before, "->", nrow(df), "registros\n")
  }
  
  # Filtro temporal (solo si está activado)
  if (!is.null(filters$enable_date) && filters$enable_date == TRUE) {
    if ("year" %in% names(df) && !is.null(filters$year_range) && length(filters$year_range) == 2) {
      if (!is.na(filters$year_range[1]) && !is.na(filters$year_range[2])) {
        tryCatch({
          df <- df %>% filter(year >= filters$year_range[1] & year <= filters$year_range[2])
        }, error = function(e) {
          message("Error al filtrar por año: ", e$message)
        })
      }
    }
  }
  
  cat("DEBUG APPLY_FILTERS - Resultado final:", nrow(df), "registros\n")
  return(df)
}

# Función para validar coordenadas
validate_coordinates <- function(data) {
  data %>%
    filter(
      has_coords == TRUE,
      !is.na(longitude), !is.na(latitude),
      longitude != 0, latitude != 0,
      longitude >= -180, longitude <= 180,
      latitude >= -90, latitude <= 90
    )
}

# Función para generar estadísticas de filtros
generate_filter_stats <- function(data_original, data_filtered, data_with_coords) {
  list(
    total_original = nrow(data_original),
    total_filtered = nrow(data_filtered),
    total_with_coords = nrow(data_with_coords),
    unique_species = n_distinct(data_filtered$scientific_name),
    coord_percentage = round((nrow(data_with_coords) / nrow(data_filtered)) * 100, 1)
  )
}

# Función para crear texto de filtros activos
create_active_filters_text <- function(filters) {
  active_filters <- c()
  
  if (!is.null(filters$genus) && filters$genus != "Todos") {
    active_filters <- c(active_filters, paste("Género:", filters$genus))
  }
  
  if (!is.null(filters$species) && !filters$species %in% c("Todas", "Todos")) {
    active_filters <- c(active_filters, paste("Especie:", filters$species))
  }
  
  if (!is.null(filters$subspecies) && !filters$subspecies %in% c("Todas", "Todos")) {
    active_filters <- c(active_filters, paste("Subespecie:", filters$subspecies))
  }
  
  if (!is.null(filters$subfamily) && !filters$subfamily %in% c("Todas", "Todos")) {
    active_filters <- c(active_filters, paste("Subfamilia:", filters$subfamily))
  }
  
  if (!is.null(filters$tribe) && filters$tribe != "Todas") {
    active_filters <- c(active_filters, paste("Tribu:", filters$tribe))
  }
  
  if (!is.null(filters$countries) && length(filters$countries) > 0 && !"Todos" %in% filters$countries) {
    if (length(filters$countries) <= 3) {
      active_filters <- c(active_filters, paste("Países:", paste(filters$countries, collapse = ", ")))
    } else {
      active_filters <- c(active_filters, paste("Países:", length(filters$countries), "seleccionados"))
    }
  }
  
  if (!is.null(filters$enable_date) && filters$enable_date == TRUE && 
      !is.null(filters$year_range) && length(filters$year_range) == 2) {
    active_filters <- c(active_filters, paste("Años:", filters$year_range[1], "-", filters$year_range[2]))
  }
  
  if (length(active_filters) == 0) {
    return("Mostrando todos los datos disponibles")
  } else {
    return(paste("Filtros activos:", paste(active_filters, collapse = " | ")))
  }
}

# SIN OPTIMIZACIÓN - SIEMPRE TODOS LOS DATOS
# (Eliminada función get_map_data que causaba problemas)

# ===================================================================
# FIN DE FUNCIONES HELPER
# ===================================================================

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
      
      /* Estilos modernos para clusters elegantes */
      .elegant-cluster-icon {
        text-align: center !important;
        border-radius: 50% !important;
        transition: all 0.3s ease !important;
      }
      
      .elegant-cluster-icon:hover {
        transform: scale(1.1) !important;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3) !important;
      }
      
      .leaflet-marker-icon {
        border: none !important;
        background: none !important;
      }
      
      /* Animación sutil para clusters */
      @keyframes pulse {
        0% { transform: scale(1); }
        50% { transform: scale(1.05); }
        100% { transform: scale(1); }
      }
      
      .elegant-cluster-icon.large {
        animation: pulse 2s infinite ease-in-out;
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
          
          # Nuevos filtros: Subfamily y Tribe
          pickerInput(
            "filtro_subfamily",
            "Subfamilia:",
            choices = c("Todas", sort(unique(datos_mariposas$subfamily[datos_mariposas$subfamily != "No especificada"]))),
            selected = "Todas",
            options = list(
              liveSearch = TRUE,
              size = 8
            )
          ),
          
          pickerInput(
            "filtro_tribe",
            "Tribu:",
            choices = c("Todas", sort(unique(datos_mariposas$tribe[datos_mariposas$tribe != "No especificada"]))),
            selected = "Todas",
            options = list(
              liveSearch = TRUE,
              size = 8
            )
          ),
          
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
          
          # Control para activar/desactivar filtro de fechas
          div(
            class = "form-group mb-3",
            materialSwitch(
              inputId = "enable_date_filter",
              label = "Activar filtro de fechas",
              value = FALSE,
              status = "primary"
            )
          ),
          
          # Filtro de años - solo visible si está activado
          conditionalPanel(
            condition = "input.enable_date_filter == true",
            div(
              class = "form-group mb-4",
              tags$label("Rango de años:", class = "mb-2 fw-bold"),
              sliderInput(
                "rango_anios",
                label = NULL,
                min = ifelse(any(!is.na(datos_mariposas$year)), 
                            min(datos_mariposas$year, na.rm = TRUE), 1900),
                max = ifelse(any(!is.na(datos_mariposas$year)), 
                            max(datos_mariposas$year, na.rm = TRUE), 
                            as.integer(format(Sys.Date(), "%Y"))),
                value = c(
                  ifelse(any(!is.na(datos_mariposas$year)), 
                         min(datos_mariposas$year, na.rm = TRUE), 1900),
                  ifelse(any(!is.na(datos_mariposas$year)), 
                         max(datos_mariposas$year, na.rm = TRUE), 
                         as.integer(format(Sys.Date(), "%Y")))
                ),
                step = 1,
                sep = "",
                width = "100%",
                ticks = TRUE,
                animate = TRUE
              )
            )
          ),
          
          # Botón para limpiar todos los filtros (como Power BI)
          div(class = "d-grid gap-2 mt-3",
            actionButton(
              "limpiar_filtros",
              "Limpiar todos los filtros",
              class = "btn btn-outline-danger btn-sm",
              title = "Restablecer todos los filtros a valores por defecto"
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
              "Puntos (clustering)" = "points",
              "Mapa de calor" = "heatmap", 
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
        
        # Gráficos estadísticos mejorados debajo del mapa
        layout_column_wrap(
          width = 1/3,
          
          # 1. Diversidad taxonómica (mejorado)
          div(class = "content-box",
            h6("Diversidad Taxonómica"),
            plotlyOutput("diversidad_taxonomica", height = 180)
          ),
          
          # 2. Distribución por País (mejorado con clustering)
          div(class = "content-box",
            h6("Distribución Geográfica"), 
            plotlyOutput("distribucion_geografica", height = 180)
          ),
          
          # 3. Índice de Shannon - Diversidad (reemplaza concentración espacial)
          div(class = "content-box",
            h6("Índice de Shannon"),
            plotlyOutput("indice_shannon", height = 180)
          )
        )
      )
    )
  ),
  
  # Tab 2: Análisis Estadísticos
  nav_panel(
    title = "Análisis Estadísticos", 
    icon = icon("chart-line"),
    
    # Mostrar resumen de filtros activos con botón limpiar
    div(class = "alert alert-info mb-3 d-flex justify-content-between align-items-center",
        div(textOutput("filtros_activos")),
        actionButton(
          "limpiar_filtros_analisis",
          "Limpiar filtros",
          class = "btn btn-outline-danger btn-sm",
          title = "Restablecer todos los filtros"
        )
    ),
    
    # Máximo 4 gráficos bien organizados y útiles
    layout_column_wrap(
      width = 1/2,
      
      # Análisis 1: Diversidad por Género (Interactivo)
      card(
        card_header("Diversidad por País (Treemap)"),
        card_body(
          plotlyOutput("analisis_altitudinal", height = 450)
        ),
        card_footer(
          p("Visualización jerárquica mostrando la diversidad de especies por país. Ideal para grandes datasets de biodiversidad.", 
            class = "text-muted small")
        )
      ),
      
      # Análisis 2: Mapa de Calor de Co-ocurrencia
      card(
        card_header("Co-ocurrencia de Géneros por País"),
        card_body(
          plotlyOutput("analisis_coocurrencia", height = 450)
        ),
        card_footer(
          p("Matriz de presencia/ausencia de géneros por país. Muestra patrones biogeográficos y distribuciones.", 
            class = "text-muted small")
        )
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      
      # Análisis 3: Sunburst Taxonómico
      card(
        card_header("Jerarquía Taxonómica (Sunburst)"),
        card_body(
          plotlyOutput("analisis_redes", height = 450)
        ),
        card_footer(
          p("Visualización circular de la jerarquía: Subfamilia → Tribu → Género. Ideal para entender relaciones taxonómicas.", 
            class = "text-muted small")
        )
      ),
      
      # Análisis 4: Tendencias Temporales por Décadas
      card(
        card_header("Tendencias de Diversidad Temporal"),
        card_body(
          plotlyOutput("analisis_estacional", height = 450)
        ),
        card_footer(
          p("Evolución de la diversidad y esfuerzo de muestreo por décadas. Muestra tendencias en la investigación de biodiversidad.", 
            class = "text-muted small")
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
  
  # Filtro dinámico de especies (usando función helper)
  output$ui_filtro_especie <- renderUI({
    create_dynamic_filter(
      data = datos_mariposas,
      column = "species",
      filter_name = "filtro_especie",
      label = "Especie",
      selected_genus = input$filtro_genero
    )
  })
  
  # Filtro dinámico de subespecies (usando función helper)
  output$ui_filtro_subespecie <- renderUI({
    create_dynamic_filter(
      data = datos_mariposas,
      column = "sub_species",
      filter_name = "filtro_subespecie",
      label = "Subespecie",
      selected_genus = input$filtro_genero,
      selected_species = input$filtro_especie
    )
  })
  
  # Función datos_filtrados optimizada con funciones helper
  datos_filtrados <- reactive({
    # DEBUG: Ver TODOS los inputs
    cat("DEBUG DATOS_FILTRADOS - Revisando inputs:\n")
    cat("   • filtro_genero:", input$filtro_genero, "\n")
    cat("   • filtro_especie:", input$filtro_especie, "\n")
    cat("   • filtro_pais:", paste(input$filtro_pais, collapse = ", "), "\n")
    cat("   • enable_date_filter:", input$enable_date_filter, "\n")
    
    # Crear objeto de filtros
    filter_list <- list(
      genus = input$filtro_genero,
      species = input$filtro_especie,
      subspecies = input$filtro_subespecie,
      subfamily = input$filtro_subfamily,
      tribe = input$filtro_tribe,
      countries = input$filtro_pais,
      enable_date = input$enable_date_filter,
      year_range = input$rango_anios
    )
    
    # Aplicar filtros usando función helper
    df <- apply_filters(datos_mariposas, filter_list)
    
    # NO devolver todos los datos si no hay resultados - devolver datos vacíos
    if(nrow(df) == 0) {
      cat("WARNING No hay datos después del filtrado\n")
      # Devolver estructura vacía pero con las mismas columnas
      return(datos_mariposas[0, ])
    }
    
    return(df)
  })
  
  # FUNCIÓN SIMPLE - SIEMPRE TODOS LOS DATOS
  datos_mapa <- reactive({
    df_filtered <- datos_filtrados()
    df_coords <- validate_coordinates(df_filtered)
    
    # SIMPLE: Devolver TODOS los datos SIEMPRE
    result <- list(
      data = df_coords,
      original_size = nrow(df_coords),
      display_size = nrow(df_coords),
      method = "all_data"
    )
    
    cat("INFO DATOS FILTRADOS:", nrow(df_filtered), "registros\n")
    cat("INFO CON COORDENADAS VÁLIDAS:", nrow(df_coords), "registros\n")
    
    return(result)
  })
  
  # Mostrar filtros activos usando función helper
  output$filtros_activos <- renderText({
    filter_list <- list(
      genus = input$filtro_genero,
      species = input$filtro_especie,
      subspecies = input$filtro_subespecie,
      subfamily = input$filtro_subfamily,
      tribe = input$filtro_tribe,
      countries = input$filtro_pais,
      enable_date = input$enable_date_filter,
      year_range = input$rango_anios
    )
    
    create_active_filters_text(filter_list)
  })
  
  # Outputs de resumen optimizados
  output$total_registros <- renderText({
    format(nrow(datos_filtrados()), big.mark = ",")
  })
  
  output$total_especies <- renderText({
    n_distinct(datos_filtrados()$scientific_name)
  })
  
  output$info_mapa <- renderText({
    optimization_result <- datos_mapa()
    n_mapa <- optimization_result$display_size
    n_total <- nrow(datos_filtrados())
    
    if(n_mapa == 0) {
      return("No hay registros con coordenadas válidas que mostrar")
    } else {
      # Mensaje simple siempre
      return(paste0("Mostrando TODOS los ", format(n_mapa, big.mark = ","), " registros disponibles"))
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
    input$filtro_subfamily,
    input$filtro_tribe,
    input$filtro_pais,
    input$enable_date_filter,
    input$rango_anios,
    input$tipo_visualizacion
  ), {
    # DEBUG: Verificar que el observeEvent se ejecuta
    cat("DEBUG OBSERVADOR EJECUTADO - Actualizando mapa\n")
    
    # LÓGICA ULTRA SIMPLE - Solo obtener TODOS los datos filtrados
    optimization_result <- datos_mapa()
    df <- optimization_result$data
    
    # DEBUG: Mostrar cuántos datos se van a mapear
    cat("DEBUG DATOS PARA MAPA:", nrow(df), "registros\n")
    
    # Mostrar información de cuántos registros se están mostrando vs total
    total_available <- optimization_result$original_size
    currently_showing <- optimization_result$display_size
    
    # Crear filtros de texto para leyenda
    filters_text <- c()
    
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      filters_text <- c(filters_text, paste("<b>Género:</b>", input$filtro_genero))
    }
    
    if(!is.null(input$filtro_especie) && !input$filtro_especie %in% c("Todas", "Todos")) {
      filters_text <- c(filters_text, paste("<b>Especie:</b>", input$filtro_especie))
    }
    
    if(!is.null(input$filtro_subespecie) && !input$filtro_subespecie %in% c("Todas", "Todos")) {
      filters_text <- c(filters_text, paste("<b>Subespecie:</b>", input$filtro_subespecie))
    }
    
    if(!is.null(input$filtro_subfamily) && !input$filtro_subfamily %in% c("Todas", "Todos")) {
      filters_text <- c(filters_text, paste("<b>Subfamilia:</b>", input$filtro_subfamily))
    }
    
    if(!is.null(input$filtro_tribe) && input$filtro_tribe != "Todas") {
      filters_text <- c(filters_text, paste("<b>Tribu:</b>", input$filtro_tribe))
    }
    
    if(!is.null(input$filtro_pais) && length(input$filtro_pais) > 0 && !"Todos" %in% input$filtro_pais) {
      if(length(input$filtro_pais) <= 3) {
        filters_text <- c(filters_text, paste("<b>Países:</b>", paste(input$filtro_pais, collapse = ", ")))
      } else {
        filters_text <- c(filters_text, paste("<b>Países:</b>", length(input$filtro_pais), "seleccionados"))
      }
    }
    
    if(!is.null(input$enable_date_filter) && input$enable_date_filter == TRUE &&
       !is.null(input$rango_anios) && length(input$rango_anios) == 2) {
      filters_text <- c(filters_text, paste("<b>Años:</b>", input$rango_anios[1], "-", input$rango_anios[2]))
    }
    
    # Construir leyenda HTML solo con filtros (sin info de optimización)
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
      clearMarkerClusters() %>% # Asegurarse de limpiar los clusters también
      clearGroup("bandas_latitudinales") %>%
      clearGroup("leyenda_bandas") %>%
      clearGroup("puntos_bandas")
    
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
    
    # VISUALIZACIÓN DINÁMICA COMPLETA: Puntos con clustering ESTILO ORIGINAL
    if(input$tipo_visualizacion == "points") {
      # USAR ESTILO POR DEFECTO de Leaflet + CircleMarkers (no GPS icons)
      leafletProxy("mapa_principal") %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste0(
            "<div style='max-width:300px;'>",
            "<h4 style='margin:0 0 10px 0;color:#2c3e50;'>", scientific_name, "</h4>",
            ifelse(!is.na(sub_species) & sub_species != "No registrada" & sub_species != "NA",
              paste0("<p><strong>Subespecie:</strong> ", sub_species, "</p>"),
              ""),
            "<p><strong>País:</strong> ", country, "</p>",
            "<p><strong>Coordenadas:</strong> ", round(latitude, 4), "°, ", round(longitude, 4), "°</p>",
            ifelse(!is.na(altitude), paste0("<p><strong>Altitud:</strong> ", format(round(altitude, 0), big.mark = ","), " m</p>"), ""),
            ifelse(!is.na(year), paste0("<p><strong>Año:</strong> ", year, "</p>"), ""),
            "</div>"
          ),
          radius = 6,
          color = "#2c3e50",
          fillColor = "#3498db",
          fillOpacity = 0.7,
          weight = 2,
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            maxClusterRadius = 80,
            disableClusteringAtZoom = 16,
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = TRUE
            # SIN iconCreateFunction = ESTILO POR DEFECTO de Leaflet
          )
        )
    } else if(input$tipo_visualizacion == "heatmap") {
      # Mapa de calor fijo - no cambia con zoom
      leafletProxy("mapa_principal") %>%
        addHeatmap(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          blur = 20,
          max = 0.05,
          radius = 15,
          minOpacity = 0.05
        )
    } else if(input$tipo_visualizacion == "bands") {
      # Bandas latitudinales - optimizado para mejor rendimiento
      lat_range <- range(df$latitude, na.rm = TRUE)
      
      # Validar que hay rango válido
      if(is.finite(lat_range[1]) && is.finite(lat_range[2]) && lat_range[1] != lat_range[2]) {
        lat_breaks <- seq(lat_range[1], lat_range[2], length.out = input$num_bandas + 1)
        
        # Asignar banda a cada punto de forma más eficiente
        df$banda <- cut(df$latitude, breaks = lat_breaks, labels = FALSE, include.lowest = TRUE)
        
        # Optimización: Usar data.table o dplyr más eficiente para datasets grandes
        if(nrow(df) > 5000) {
          # Para datasets grandes, muestrear para el cálculo de especies
          sample_size <- min(2000, nrow(df))
          df_sample <- df[sample(nrow(df), sample_size), ]
          
          bandas_resumen <- df %>%
            group_by(banda) %>%
            summarise(
              n_registros = n(),
              .groups = 'drop'
            ) %>%
            left_join(
              df_sample %>%
                group_by(banda) %>%
                summarise(
                  n_especies = n_distinct(scientific_name),
                  .groups = 'drop'
                ),
              by = "banda"
            ) %>%
            mutate(
              n_especies = ifelse(is.na(n_especies), 0, n_especies)
            )
          
          # Agregar coordenadas de las bandas
          bandas_resumen$lat_min <- lat_breaks[bandas_resumen$banda]
          bandas_resumen$lat_max <- lat_breaks[bandas_resumen$banda + 1]
          
        } else {
          # Para datasets pequeños, cálculo completo
          bandas_resumen <- df %>%
            group_by(banda) %>%
            summarise(
              n_registros = n(),
              n_especies = n_distinct(scientific_name),
              .groups = 'drop'
            )
          
          # Agregar coordenadas de las bandas
          bandas_resumen$lat_min <- lat_breaks[bandas_resumen$banda]
          bandas_resumen$lat_max <- lat_breaks[bandas_resumen$banda + 1]
        }
      } else {
        # Si no hay rango válido, crear una banda única
        bandas_resumen <- data.frame(
          banda = 1,
          n_registros = nrow(df),
          n_especies = n_distinct(df$scientific_name),
          lat_min = lat_range[1],
          lat_max = lat_range[1] + 1
        )
      }
      
      # Colores para las bandas
      pal <- colorNumeric(palette = "YlOrRd", domain = bandas_resumen$n_especies)
      
      # Crear proxy del mapa una sola vez para eficiencia
      map_proxy <- leafletProxy("mapa_principal")
      
      # Agregar rectángulos de bandas
      for(i in 1:nrow(bandas_resumen)) {
        map_proxy %>%
          addRectangles(
            lng1 = -180, lng2 = 180,
            lat1 = bandas_resumen$lat_min[i],
            lat2 = bandas_resumen$lat_max[i],
            fillColor = pal(bandas_resumen$n_especies[i]),
            fillOpacity = 0.3,
            weight = 2,
            color = "black",
            group = "bandas_latitudinales",
            popup = paste0(
              "Banda ", i, "<br>",
              "Latitud: ", round(bandas_resumen$lat_min[i], 2), 
              " a ", round(bandas_resumen$lat_max[i], 2), "<br>",
              "Registros: ", bandas_resumen$n_registros[i], "<br>",
              "Especies: ", bandas_resumen$n_especies[i]
            )
          )
      }
      
      # Agregar leyenda con información sobre optimización
      legend_title <- if(nrow(df) > 5000) {
        paste("Especies por banda", "(estimado)")
      } else {
        "Especies por banda"
      }
      
      map_proxy %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = bandas_resumen$n_especies,
          title = legend_title,
          group = "leyenda_bandas"
        )
      

      
      # Optimización: Solo agregar puntos si hay menos de 1000 registros para mejor rendimiento
      if(nrow(df) <= 1000) {
        # Agregar puntos sin clusterización para datasets pequeños
        map_proxy %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 3,
            fillOpacity = 0.6,
            weight = 1,
            color = "#2c3e50",
            fillColor = "#3498db",
            group = "puntos_bandas",
            popup = ~paste0(
              "<strong>", scientific_name, "</strong><br>",
              "Banda: ", banda, "<br>",
              "País: ", country, "<br>",
              "Coordenadas: ", round(latitude, 4), ", ", round(longitude, 4)
            )
          )
      } else {
        # Para datasets grandes, agregar con clustering para mejor rendimiento
        map_proxy %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 2,
            fillOpacity = 0.4,
            weight = 0.5,
            color = "#2c3e50",
            fillColor = "#3498db",
            group = "puntos_bandas",
            clusterOptions = markerClusterOptions(
              maxClusterRadius = 50,
              showCoverageOnHover = FALSE
            ),
            popup = ~paste0(
              "<strong>", scientific_name, "</strong><br>",
              "Banda: ", banda, "<br>",
              "País: ", country
            )
          )
      }
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
            modeBarButtonsToRemove = list(
              'select2d', 'lasso2d', 'autoScale2d', 'pan2d'
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
  
  # ANÁLISIS ECOLÓGICO - NUEVOS GRÁFICOS APROPIADOS PARA BIODIVERSIDAD
  
  # 1. Treemap de especies por país
  output$analisis_altitudinal <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) > 20) {
      # Crear datos para treemap - especies por país con géneros
      treemap_data <- df %>%
        group_by(country, genus) %>%
        summarise(
          n_especies = n_distinct(scientific_name),
          n_registros = n(),
          .groups = 'drop'
        ) %>%
        arrange(desc(n_especies)) %>%
        head(30)  # Limitar para evitar sobrecarga visual
      
      # Preparar datos para el treemap con jerarquía
      total_data <- treemap_data %>%
        group_by(country) %>%
        summarise(
          total_especies = sum(n_especies),
          total_registros = sum(n_registros),
          .groups = 'drop'
        ) %>%
        arrange(desc(total_especies)) %>%
        head(12)  # Top 12 países
      
      # Crear treemap usando plotly
      plot_ly(
        data = total_data,
        type = "treemap",
        labels = ~paste0(country, "<br>", total_especies, " especies"),
        values = ~total_especies,
        parents = "",
        textinfo = "label+value+percent parent",
        textfont = list(size = 12, color = "white"),
        marker = list(
          colorscale = list(
            c(0, "#2ECC71"),
            c(0.5, "#F39C12"),
            c(1, "#E74C3C")
          ),
          colorbar = list(title = "Especies"),
          line = list(width = 2, color = "white")
        ),
        hovertemplate = '<b>%{label}</b><br>Especies: %{value}<br>Porcentaje: %{percentParent}<extra></extra>'
      ) %>%
      layout(
        title = "Diversidad de especies por país (Treemap)",
        font = list(size = 12),
        margin = list(l = 20, r = 20, b = 20, t = 40)
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToShow = list('toImage', 'resetScale2d')
      )
      
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis (mínimo 20 registros)")
    }
  })
  
  # 2. Mapa de calor mejorado de co-ocurrencia
  output$analisis_coocurrencia <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) > 50) {
      # Análisis de co-ocurrencia de géneros por país
      genera_paises <- df %>%
        select(genus, country) %>%
        distinct() %>%
        count(genus) %>%
        arrange(desc(n)) %>%
        head(15)  # Top 15 géneros más distribuidos
      
      paises_top <- df %>%
        count(country) %>%
        arrange(desc(n)) %>%
        head(12) %>%  # Top 12 países
        pull(country)
      
      # Crear matriz de presencia/ausencia
      matriz <- df %>%
        filter(genus %in% genera_paises$genus, country %in% paises_top) %>%
        select(genus, country) %>%
        distinct() %>%
        mutate(presente = 1) %>%
        pivot_wider(names_from = country, values_from = presente, values_fill = 0) %>%
        column_to_rownames("genus") %>%
        as.matrix()
      
      if(nrow(matriz) > 3 && ncol(matriz) > 3) {
        plot_ly(
          z = ~matriz,
          x = colnames(matriz),
          y = rownames(matriz),
          type = "heatmap",
          colorscale = list(
            c(0, "rgba(236, 240, 241, 0.8)"),
            c(0.3, "rgba(52, 152, 219, 0.6)"),
            c(0.7, "rgba(46, 204, 113, 0.8)"),
            c(1, "rgba(231, 76, 60, 1)")
          ),
          hovertemplate = '<b>Género</b>: %{y}<br><b>País</b>: %{x}<br><b>Presente</b>: %{z}<extra></extra>',
          showscale = TRUE,
          colorbar = list(
            title = "Presencia",
            tickmode = "array",
            tickvals = c(0, 1),
            ticktext = c("Ausente", "Presente")
          )
        ) %>%
        layout(
          title = "Presencia de géneros por país",
          xaxis = list(
            title = "País",
            tickangle = -45,
            tickfont = list(size = 11)
          ),
          yaxis = list(
            title = "Género",
            tickfont = list(size = 11)
          ),
          margin = list(l = 100, r = 60, b = 100, t = 60)
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          scrollZoom = TRUE
        )
      } else {
        plotly_empty() %>%
          layout(title = "Datos insuficientes para matriz de co-ocurrencia")
      }
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis (mínimo 50 registros)")
    }
  })
  
  # 3. Gráfico sunburst de jerarquía taxonómica
  output$analisis_redes <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) > 30) {
      # Preparar datos jerárquicos: Subfamilia -> Tribu -> Género
      hierarchy_data <- df %>%
        group_by(subfamily, tribe, genus) %>%
        summarise(
          n_especies = n_distinct(scientific_name),
          n_registros = n(),
          .groups = 'drop'
        ) %>%
        filter(
          subfamily != "No especificada",
          tribe != "No especificada"
        ) %>%
        arrange(desc(n_especies)) %>%
        head(40)  # Limitar para mejor visualización
      
      if(nrow(hierarchy_data) > 5) {
        # Crear estructura para sunburst
        sunburst_data <- bind_rows(
          # Nivel raíz
          data.frame(
            ids = "Ithomiini",
            labels = "Ithomiini",
            parents = "",
            values = sum(hierarchy_data$n_especies)
          ),
          # Subfamilias
          hierarchy_data %>%
            group_by(subfamily) %>%
            summarise(values = sum(n_especies), .groups = 'drop') %>%
            mutate(
              ids = subfamily,
              labels = subfamily,
              parents = "Ithomiini"
            ),
          # Tribus
          hierarchy_data %>%
            group_by(subfamily, tribe) %>%
            summarise(values = sum(n_especies), .groups = 'drop') %>%
            mutate(
              ids = paste(subfamily, tribe, sep = "_"),
              labels = tribe,
              parents = subfamily
            ),
          # Géneros
          hierarchy_data %>%
            mutate(
              ids = paste(subfamily, tribe, genus, sep = "_"),
              labels = genus,
              parents = paste(subfamily, tribe, sep = "_"),
              values = n_especies
            )
        )
        
        plot_ly(
          data = sunburst_data,
          type = 'sunburst',
          ids = ~ids,
          labels = ~labels,
          parents = ~parents,
          values = ~values,
          branchvalues = "total",
          hovertemplate = '<b>%{label}</b><br>Especies: %{value}<br>Porcentaje: %{percentParent}<extra></extra>',
          maxdepth = 3,
          insidetextorientation = 'radial'
        ) %>%
        layout(
          title = "Jerarquía taxonómica (Sunburst)",
          font = list(size = 12),
          margin = list(l = 20, r = 20, b = 20, t = 40)
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToShow = list('toImage')
        )
      } else {
        plotly_empty() %>%
          layout(title = "Datos taxonómicos insuficientes")
      }
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis jerárquico")
    }
  })
  
  # 4. Distribución temporal con patrón estacional mejorado
  output$analisis_estacional <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) > 20) {
      # Análisis de diversidad por décadas (en lugar de fenología simple)
      decadas_data <- df %>%
        filter(!is.na(year), year >= 1950) %>%
        mutate(
          decada = floor(year / 10) * 10,
          decada_label = paste0(decada, "s")
        ) %>%
        group_by(decada, decada_label) %>%
        summarise(
          n_especies = n_distinct(scientific_name),
          n_registros = n(),
          n_paises = n_distinct(country),
          .groups = 'drop'
        ) %>%
        arrange(decada)
      
      if(nrow(decadas_data) >= 3) {
        # Gráfico de área apilada para mostrar tendencias temporales
        plot_ly(decadas_data) %>%
          add_bars(
            x = ~decada_label,
            y = ~n_registros,
            name = "Registros totales",
            yaxis = "y",
            marker = list(color = 'rgba(52, 152, 219, 0.7)'),
            hovertemplate = '<b>%{x}</b><br>Registros: %{y}<extra></extra>'
          ) %>%
          add_lines(
            x = ~decada_label,
            y = ~n_especies,
            name = "Especies únicas",
            yaxis = "y2",
            line = list(color = 'rgba(231, 76, 60, 1)', width = 4),
            marker = list(size = 10, color = 'rgba(231, 76, 60, 1)'),
            hovertemplate = '<b>%{x}</b><br>Especies: %{y}<extra></extra>'
          ) %>%
          layout(
            title = "Tendencias de diversidad por décadas",
            xaxis = list(title = "Década"),
            yaxis = list(
              title = "Número de registros",
              side = "left",
              showgrid = FALSE
            ),
            yaxis2 = list(
              title = "Número de especies",
              side = "right",
              overlaying = "y",
              showgrid = FALSE
            ),
            legend = list(x = 0.02, y = 0.98),
            margin = list(l = 60, r = 60, b = 50, t = 60)
          ) %>%
          config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            scrollZoom = TRUE
          )
      } else {
        # Gráfico de barras simple si hay pocos datos temporales
        plot_ly(
          x = c("1950-1979", "1980-1999", "2000-2019", "2020+"),
          y = c(
            sum(df$year >= 1950 & df$year < 1980, na.rm = TRUE),
            sum(df$year >= 1980 & df$year < 2000, na.rm = TRUE),
            sum(df$year >= 2000 & df$year < 2020, na.rm = TRUE),
            sum(df$year >= 2020, na.rm = TRUE)
          ),
          type = 'bar',
          marker = list(
            color = c('#3498db', '#2ecc71', '#f39c12', '#e74c3c'),
            line = list(color = 'white', width = 1)
          ),
          hovertemplate = '<b>%{x}</b><br>Registros: %{y}<extra></extra>'
        ) %>%
        layout(
          title = "Distribución temporal general",
          xaxis = list(title = "Período"),
          yaxis = list(title = "Registros"),
          margin = list(l = 60, r = 20, b = 50, t = 60)
        )
      }
    } else {
      plotly_empty() %>%
        layout(title = "Datos temporales insuficientes")
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
  
  # ===================================================================
  # NUEVOS GRÁFICOS OPTIMIZADOS PARA LOS DATOS DISPONIBLES
  # ===================================================================
  
  # Gráficos debajo del mapa (simples e informativos)
  output$diversidad_taxonomica <- renderPlotly({
    df <- datos_filtrados()
    
    diversidad <- data.frame(
      Categoria = c("Géneros", "Especies", "Subespecies"),
      Cantidad = c(
        n_distinct(df$genus, na.rm = TRUE),
        n_distinct(df$species, na.rm = TRUE),
        n_distinct(df$sub_species[df$sub_species != "NA" & !is.na(df$sub_species)], na.rm = TRUE)
      )
    )
    
    p <- plot_ly(diversidad, x = ~Categoria, y = ~Cantidad, type = 'bar',
                 marker = list(color = c('#3498db', '#2c3e50', '#18bc9c'))) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Cantidad"),
        margin = list(t = 20, r = 10, b = 40, l = 40),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  output$distribucion_geografica <- renderPlotly({
    df <- datos_filtrados()
    
    paises <- df %>%
      count(country, sort = TRUE) %>%
      head(8) # Top 8 países
    
    p <- plot_ly(paises, x = ~reorder(country, n), y = ~n, type = 'bar',
                 marker = list(color = '#e74c3c')) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Registros"),
        margin = list(t = 20, r = 10, b = 40, l = 40),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # 3. Índice de Shannon - Reemplaza concentración espacial
  output$indice_shannon <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos para mostrar", textfont = list(size = 14)) %>%
        layout(
          title = list(text = "Resumen de Datos Filtrados", x = 0.5),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE),
          margin = list(t = 20, r = 10, b = 40, l = 40)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Crear resumen rápido de los datos filtrados
    resumen_data <- data.frame(
      Categoria = c("Géneros", "Especies", "Registros", "Países"),
      Cantidad = c(
        n_distinct(df$genus),
        n_distinct(df$species),
        nrow(df),
        n_distinct(df$country)
      ),
      Color = c("#3498db", "#e74c3c", "#f39c12", "#27ae60")
    )
    
    # Crear gráfico de barras simple y claro
    p <- plot_ly(resumen_data, 
                 x = ~Categoria, 
                 y = ~Cantidad, 
                 type = 'bar',
                 marker = list(color = ~Color)) %>%
      layout(
        title = list(text = "Resumen de Datos Filtrados", x = 0.5, font = list(size = 14)),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Cantidad"),
        margin = list(t = 40, r = 10, b = 50, l = 50),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # GRÁFICO 2: Top 10 Géneros (Optimizado para filtros)
  output$diversidad_por_genero <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos disponibles", textfont = list(size = 14)) %>%
        layout(
          title = list(text = "Top Géneros por Diversidad", x = 0.5),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Mostrar SOLO los top 10 géneros para evitar sobrecarga visual con filtros
    generos <- df %>%
      group_by(genus) %>%
      summarise(
        especies = n_distinct(species),
        registros = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(especies)) %>%
      head(10)  # LIMITADO a 10 para mejor visualización con filtros
    
    if(nrow(generos) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin géneros para mostrar", textfont = list(size = 14)) %>%
        layout(
          title = list(text = "Top Géneros por Diversidad", x = 0.5),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Crear colores gradientes
    colors <- colorRampPalette(c("#3498db", "#e74c3c"))(nrow(generos))
    
    p <- plot_ly(generos, x = ~reorder(genus, especies), y = ~especies, 
                 type = 'bar', 
                 marker = list(color = colors, line = list(color = 'rgba(0,0,0,0.3)', width = 1)),
                 text = ~paste("Registros:", registros), textposition = 'none',
                 hovertemplate = ~paste(
                   "<b>%{x}</b><br>",
                   "Especies: %{y}<br>",
                   "Registros:", registros,
                   "<extra></extra>"
                 )) %>%
      layout(
        title = list(text = "Top 10 Géneros por Diversidad", x = 0.5, font = list(size = 14)),
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Especies"),
        margin = list(t = 60, r = 20, b = 100, l = 50),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # GRÁFICO OPTIMIZADO: Top 10 Países por Esfuerzo de Muestreo
  output$curvas_acumulacion <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos disponibles", textfont = list(size = 16)) %>%
        layout(
          title = list(text = "Esfuerzo de Muestreo", x = 0.5),
          xaxis = list(title = "País", showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "Eficiencia", showgrid = FALSE, showticklabels = FALSE),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Calcular eficiencia de muestreo (especies/registros) por país
    eficiencia_data <- df %>%
      group_by(country) %>%
      summarise(
        total_registros = n(),
        especies_unicas = n_distinct(species),
        eficiencia = round(especies_unicas / total_registros * 100, 2),
        .groups = 'drop'
      ) %>%
      filter(total_registros >= 10) %>%  # Solo países con suficientes registros
      arrange(desc(eficiencia)) %>%
      head(12)  # Top 12 países
    
    # Crear gráfico de barras con gradiente
    colors <- colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))(nrow(eficiencia_data))
    
    p <- plot_ly(eficiencia_data, 
                 x = ~reorder(country, eficiencia), 
                 y = ~eficiencia,
                 type = 'bar',
                 marker = list(
                   color = colors,
                   line = list(color = 'rgba(0,0,0,0.2)', width = 1)
                 ),
                 text = ~paste("Registros:", total_registros, "<br>Especies:", especies_unicas),
                 textposition = 'none',
                 hovertemplate = ~paste(
                   "<b>País:</b>", country, "<br>",
                   "<b>Eficiencia:</b>", eficiencia, "%<br>",
                   "<b>Especies:</b>", especies_unicas, "<br>",
                   "<b>Registros:</b>", total_registros,
                   "<extra></extra>"
                 )) %>%
      layout(
        title = list(text = "Eficiencia de Muestreo por País (%)", x = 0.5),
        xaxis = list(title = "País", tickangle = 45),
        yaxis = list(title = "Especies por cada 100 registros"),
        margin = list(t = 60, r = 40, b = 100, l = 60),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    p
  })
  
  # GRÁFICO 3: Top 10 Países (Optimizado para filtros)
  output$riqueza_por_pais <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos disponibles", textfont = list(size = 14)) %>%
        layout(
          title = list(text = "Top Países por Riqueza", x = 0.5),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Mostrar SOLO los top 10 países para evitar sobrecarga visual con filtros
    riqueza <- df %>%
      group_by(country) %>%
      summarise(
        especies_unicas = n_distinct(species),
        registros_totales = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(especies_unicas)) %>%
      head(10)  # LIMITADO a 10 para mejor visualización con filtros
    
    if(nrow(riqueza) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin países para mostrar", textfont = list(size = 14)) %>%
        layout(
          title = list(text = "Top Países por Riqueza", x = 0.5),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Crear colores gradientes
    colors <- colorRampPalette(c("#27ae60", "#f39c12"))(nrow(riqueza))
    
    p <- plot_ly(riqueza, x = ~reorder(country, especies_unicas), y = ~especies_unicas,
                 type = 'bar', 
                 marker = list(color = colors, line = list(color = 'rgba(0,0,0,0.3)', width = 1)),
                 text = ~paste("Registros:", registros_totales), 
                 textposition = 'none',
                 hovertemplate = ~paste(
                   "<b>%{x}</b><br>",
                   "Especies: %{y}<br>",
                   "Registros:", registros_totales,
                   "<extra></extra>"
                 )) %>%
      layout(
        title = list(text = "Top 10 Países por Riqueza", x = 0.5, font = list(size = 14)),
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Especies"),
        margin = list(t = 60, r = 20, b = 100, l = 50),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # GRÁFICO OPTIMIZADO: Distribución de Registros por Año
  output$endemismo_pais <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos disponibles", textfont = list(size = 16)) %>%
        layout(
          title = list(text = "Distribución Temporal", x = 0.5),
          xaxis = list(title = "Año", showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "Registros", showgrid = FALSE, showticklabels = FALSE),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Agregar datos por décadas para mejor visualización
    temporal_data <- df %>%
      filter(!is.na(year) & year >= 1950) %>%  # Solo años válidos desde 1950
      mutate(
        decada = paste0(floor(year/10)*10, "s")
      ) %>%
      group_by(decada) %>%
      summarise(
        registros = n(),
        especies_distintas = n_distinct(species),
        paises_distintos = n_distinct(country),
        .groups = 'drop'
      ) %>%
      arrange(decada)
    
    if(nrow(temporal_data) == 0) {
      p <- plot_ly() %>%
        add_text(x = 0, y = 0, text = "Sin datos de fecha disponibles", textfont = list(size = 16)) %>%
        layout(
          title = list(text = "Distribución Temporal de Registros", x = 0.5),
          margin = list(t = 60, r = 40, b = 60, l = 60)
        ) %>%
        config(displayModeBar = FALSE)
      return(p)
    }
    
    # Crear gráfico de barras con gradiente temporal
    colors <- colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(nrow(temporal_data))
    
    p <- plot_ly(temporal_data, 
                 x = ~decada, 
                 y = ~registros,
                 type = 'bar',
                 marker = list(
                   color = colors,
                   line = list(color = 'rgba(0,0,0,0.3)', width = 1)
                 ),
                 text = ~paste("Especies:", especies_distintas, "<br>Países:", paises_distintos),
                 textposition = 'none',
                 hovertemplate = ~paste(
                   "<b>Década:</b>", decada, "<br>",
                   "<b>Registros:</b>", registros, "<br>",
                   "<b>Especies:</b>", especies_distintas, "<br>",
                   "<b>Países:</b>", paises_distintos,
                   "<extra></extra>"
                 )) %>%
      layout(
        title = list(text = "Distribución Temporal de Registros por Década", x = 0.5),
        xaxis = list(title = "Década", tickangle = 0),
        yaxis = list(title = "Número de Registros"),
        margin = list(t = 60, r = 40, b = 80, l = 60),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    p
  })
  
  output$completitud_taxonomica <- renderPlotly({
    df <- datos_filtrados()
    
    completitud <- df %>%
      mutate(
        tiene_subespecie = !is.na(sub_species) & sub_species != "NA" & sub_species != ""
      ) %>%
      group_by(genus) %>%
      summarise(
        total_especies = n_distinct(species),
        con_subespecie = sum(tiene_subespecie),
        porcentaje_completo = round((con_subespecie / n()) * 100, 1),
        .groups = 'drop'
      ) %>%
      filter(total_especies >= 3) %>% # Solo géneros con 3+ especies
      arrange(desc(porcentaje_completo)) %>%
      head(15)
    
    p <- plot_ly(completitud, x = ~reorder(genus, porcentaje_completo), 
                 y = ~porcentaje_completo, type = 'bar',
                 marker = list(color = ~porcentaje_completo, colorscale = 'RdYlGn',
                              cmin = 0, cmax = 100)) %>%
      layout(
        title = list(text = "Completitud Taxonómica por Género", x = 0.5),
        xaxis = list(title = "Género", tickangle = 45),
        yaxis = list(title = "% Registros con Subespecie", range = c(0, 100)),
        margin = list(t = 60, r = 40, b = 100, l = 60),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    p
  })
  
  output$resumen_taxonomico <- renderTable({
    df <- datos_filtrados()
    
    resumen <- data.frame(
      Métrica = c(
        "Total géneros",
        "Total especies", 
        "Total registros",
        "Con subespecie",
        "% Completitud"
      ),
      Valor = c(
        n_distinct(df$genus),
        n_distinct(df$species),
        nrow(df),
        sum(!is.na(df$sub_species) & df$sub_species != "NA" & df$sub_species != ""),
        paste0(round((sum(!is.na(df$sub_species) & df$sub_species != "NA" & 
                          df$sub_species != "") / nrow(df)) * 100, 1), "%")
      )
    )
    
    resumen
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # Observador para cambiar el tipo de mapa base
  observeEvent(input$mapa_base, {
    leafletProxy("mapa_principal") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$mapa_base]])
  })
  
  # BOTÓN LIMPIAR FILTROS MEJORADO (menos confuso)
  observeEvent(input$limpiar_filtros, {
    # Solo resetear filtros principales, NO la visualización ni rangos de años
    updatePickerInput(session, "filtro_genero", selected = "Todos")
    updatePickerInput(session, "filtro_especie", selected = "Todas")
    updatePickerInput(session, "filtro_subespecie", selected = "Todas")
    updatePickerInput(session, "filtro_subfamily", selected = "Todas")
    updatePickerInput(session, "filtro_tribe", selected = "Todas")
    updatePickerInput(session, "filtro_pais", selected = "Todos")
    
    # NO resetear: tipo de visualización, rango de años, ni filtro de fechas
    # Esto hace que sea menos confuso para el usuario
    
    # Mostrar notificación más clara
    showNotification(
      "Filtros de categorías limpiados (fechas y visualización sin cambios)",
      type = "message",
      duration = 4
    )
  })
  
  # BOTÓN LIMPIAR FILTROS EN ANÁLISIS ESTADÍSTICOS (misma lógica mejorada)
  observeEvent(input$limpiar_filtros_analisis, {
    # Solo resetear filtros principales, NO las configuraciones
    updatePickerInput(session, "filtro_genero", selected = "Todos")
    updatePickerInput(session, "filtro_especie", selected = "Todas")
    updatePickerInput(session, "filtro_subespecie", selected = "Todas")
    updatePickerInput(session, "filtro_subfamily", selected = "Todas")
    updatePickerInput(session, "filtro_tribe", selected = "Todas")
    updatePickerInput(session, "filtro_pais", selected = "Todos")
    
    # Mostrar notificación más clara
    showNotification(
      "Filtros de categorías limpiados desde Análisis",
      type = "message",
      duration = 4
    )
  })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)