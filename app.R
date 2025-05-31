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

# Cargar datos
source("data_loader.R")

# Tema personalizado
mi_tema <- bs_theme(
  version = 5,
  bootswatch = "pulse",
  primary = "#3a4a8c",
  secondary = "#b2c6f6",
  success = "#28a745",
  info = "#81a4e3",
  warning = "#ffc107",
  base_font = font_google("Poppins"),
  heading_font = font_google("Montserrat"),
  font_scale = 0.95
)

# Función para crear tarjetas personalizadas
card_custom <- function(title, content, height = NULL, class = NULL, icon = NULL) {
  div(
    class = paste("dashboard-card", class),
    style = if(!is.null(height)) paste0("height:", height, ";"),
    if(!is.null(icon)) div(class = "card-icon", icon),
    h5(title, class = "card-title mt-2"),
    div(class = "card-body", content)
  )
}

# UI
ui <- page_fillable(
  theme = mi_tema,
  title = "SIG Mariposas Ithomiini - Sistema de Monitoreo y Análisis",
  
  # CSS personalizado
  tags$head(
    tags$style(HTML('
      body { 
        background: linear-gradient(135deg, #e8f2ff 0%, #f0e6ff 100%); 
      }
      .dashboard-card {
        background: rgba(255, 255, 255, 0.95);
        border-radius: 16px;
        box-shadow: 0 4px 15px rgba(60, 80, 180, 0.08);
        padding: 20px;
        margin-bottom: 15px;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      .dashboard-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(60, 80, 180, 0.15);
      }
      .dashboard-card::before {
        content: "";
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #3a4a8c 0%, #b2c6f6 100%);
      }
      .card-icon {
        font-size: 2.5em;
        color: #3a4a8c;
        opacity: 0.2;
        position: absolute;
        right: 20px;
        top: 20px;
      }
      .value-box-custom {
        background: linear-gradient(135deg, #3a4a8c 0%, #5a6aac 100%);
        color: white;
        padding: 20px;
        border-radius: 12px;
        text-align: center;
        margin-bottom: 10px;
        box-shadow: 0 4px 15px rgba(60, 80, 180, 0.2);
      }
      .value-box-custom h3 {
        margin: 0;
        font-size: 2em;
        font-weight: 700;
      }
      .value-box-custom h6 {
        margin: 0;
        opacity: 0.9;
        font-weight: 400;
      }
      .leaflet-container {
        border-radius: 16px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
      }
      .nav-pills .nav-link {
        border-radius: 25px;
        padding: 10px 20px;
        margin: 0 5px;
        transition: all 0.3s ease;
      }
      .nav-pills .nav-link:hover {
        background-color: rgba(58, 74, 140, 0.1);
      }
      .nav-pills .nav-link.active {
        background-color: #3a4a8c !important;
        box-shadow: 0 4px 15px rgba(58, 74, 140, 0.3);
      }
      .sidebar {
        background: rgba(255, 255, 255, 0.98) !important;
        backdrop-filter: blur(10px);
        border-right: 1px solid rgba(58, 74, 140, 0.1);
      }
      .btn-custom {
        background: linear-gradient(135deg, #3a4a8c 0%, #5a6aac 100%);
        color: white;
        border: none;
        border-radius: 25px;
        padding: 8px 20px;
        transition: all 0.3s ease;
      }
      .btn-custom:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(58, 74, 140, 0.3);
      }
      .species-badge {
        display: inline-block;
        padding: 4px 12px;
        background-color: #e8f2ff;
        color: #3a4a8c;
        border-radius: 20px;
        font-size: 0.85em;
        margin: 2px;
      }
      @media (max-width: 768px) {
        .dashboard-card {
          margin-bottom: 10px;
        }
      }
    '))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros",
      bg = "transparent",
      width = 320,
      
      # Filtros principales
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Taxonomía"),
            actionButton("reset_filtros", "Limpiar", 
                         class = "btn-sm btn-outline-secondary",
                         icon = icon("refresh"))
          )
        ),
        uiOutput("ui_filtro_genero"),
        uiOutput("ui_filtro_especie"),
        uiOutput("ui_filtro_subespecie"),
        hr(),
        
        # Filtro de país en un acordeón
        accordion(
          accordion_panel(
            "Filtros geográficos",
            pickerInput(
              "filtro_pais", "País:", 
              choices = c("Todos", sort(unique(datos_mariposas$country))),
              selected = "Todos",
              multiple = TRUE,
              options = list(
                actionsBox = TRUE,
                liveSearch = TRUE,
                style = "btn-outline-primary",
                dropupAuto = FALSE
              )
            )
          )
        ),
        
        # Otros filtros
        accordion(
          accordion_panel(
            "Opciones de visualización",
            sliderTextInput(
              "anio", "Año:", 
              choices = 2010:2023,
              selected = 2023,
              animate = animationOptions(interval = 800),
              grid = TRUE
            ),
            pickerInput(
              "mapa_estilo", "Estilo de mapa:",
              choices = c("CartoDB.Positron", "Stamen.TonerLite", "Esri.WorldImagery"),
              selected = "CartoDB.Positron",
              options = list(style = "btn-outline-primary")
            )
          )
        )
      ),
      
      # Imagen decorativa
      div(
        class = "text-center mt-4",
        img(src = "https://cdn.pixabay.com/photo/2017/01/31/13/14/butterfly-2027663_1280.png", 
            alt = "Mariposa", 
            width = "120px",
            class = "img-fluid")
      )
    ),
    
    # Contenido principal
    navset_card_pill(
      title = div(
        bsicons::bs_icon("flower1", size = "1.5em"), 
        " Sistema de Información Geográfica - Mariposas Ithomiini",
        style = "display: flex; align-items: center; gap: 10px;"
      ),
      
      # Tab 1: Mapa Principal
      nav_panel(
        title = "Mapa de Distribución",
        icon = bsicons::bs_icon("pin-map"),
        
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Distribución Geográfica de Especies"),
              div(
                actionButton("reset_view", "Restablecer vista", 
                           class = "btn-sm btn-custom me-2"),
                actionButton("download_map", "Descargar mapa", 
                           class = "btn-sm btn-outline-primary")
              )
            ),
            leafletOutput("mapa_principal", height = "65vh"),
            card_footer(
              textOutput("info_seleccion"),
              class = "text-muted small"
            )
          )
        ),
        
        layout_column_wrap(
          width = 1/4,
          card_custom(
            "Diversidad por País",
            plotlyOutput("grafico_paises", height = 200),
            height = "280px",
            icon = bsicons::bs_icon("globe2")
          ),
          card_custom(
            "Top Géneros",
            plotlyOutput("grafico_generos", height = 200),
            height = "280px",
            icon = bsicons::bs_icon("diagram-3")
          ),
          card_custom(
            "Distribución Altitudinal",
            plotlyOutput("grafico_altitud", height = 200),
            height = "280px",
            icon = bsicons::bs_icon("graph-up")
          ),
          card_custom(
            "Especies por Hábitat",
            plotlyOutput("grafico_habitat", height = 200),
            height = "280px",
            icon = bsicons::bs_icon("tree")
          )
        )
      ),
      
      # Tab 2: Análisis Ecológico
      nav_panel(
        title = "Análisis Ecológico",
        icon = bsicons::bs_icon("diagram-2"),
        
        layout_column_wrap(
          width = 1/2,
          
          card(
            card_header("Patrones de Distribución Altitudinal"),
            plotlyOutput("violin_altitud", height = 400),
            card_footer(
              "Distribución de especies por rango altitudinal. Los violines muestran la densidad de registros.",
              class = "text-muted small"
            )
          ),
          
          card(
            card_header("Análisis de Diversidad por Zona Climática"),
            plotlyOutput("diversidad_zona", height = 400),
            card_footer(
              "Índices de diversidad calculados por zona latitudinal.",
              class = "text-muted small"
            )
          )
        ),
        
        card(
          card_header("Matriz de Co-ocurrencia de Especies"),
          plotlyOutput("matriz_coocurrencia", height = 500),
          card_footer(
            "Frecuencia de co-ocurrencia entre las especies más comunes.",
            class = "text-muted small"
          )
        ),
        
        layout_column_wrap(
          width = 1/2,
          
          card(
            card_header("Preferencias de Hábitat"),
            plotlyOutput("sankey_habitat", height = 350)
          ),
          
          card(
            card_header("Riqueza de Especies por País"),
            plotlyOutput("mapa_riqueza", height = 350)
          )
        )
      ),
      
      # Tab 3: Análisis Temporal
      nav_panel(
        title = "Análisis Temporal",
        icon = bsicons::bs_icon("calendar-range"),
        
        card(
          card_header("Registros a lo Largo del Tiempo"),
          plotlyOutput("serie_temporal", height = 300),
          card_footer(
            "Tendencia temporal de registros. Útil para identificar periodos de mayor actividad de muestreo.",
            class = "text-muted small"
          )
        ),
        
        layout_column_wrap(
          width = 1/2,
          
          card(
            card_header("Estacionalidad de Registros"),
            plotlyOutput("grafico_estacional", height = 300)
          ),
          
          card(
            card_header("Actividad por Colector"),
            plotlyOutput("grafico_colectores", height = 300)
          )
        ),
        
        card(
          card_header("Fenología de Especies Seleccionadas"),
          pickerInput(
            "especies_fenologia",
            "Seleccionar especies para análisis fenológico:",
            choices = sort(unique(datos_mariposas$scientific_name)),
            selected = head(sort(unique(datos_mariposas$scientific_name)), 3),
            multiple = TRUE,
            options = list(
              maxOptions = 5,
              liveSearch = TRUE
            )
          ),
          plotlyOutput("fenologia_especies", height = 350)
        )
      ),
      
      # Tabla de datos
      nav_panel(
        title = "Base de Datos",
        icon = bsicons::bs_icon("database"),
        
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Registros Detallados"),
            div(
              span(class = "text-muted me-3", 
                   textOutput("info_registros_tabla", inline = TRUE)),
              downloadButton("download_filtered", "Descargar filtrados", 
                           class = "btn-sm btn-primary me-2"),
              downloadButton("download_all", "Descargar todos", 
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          card_body(
            DTOutput("tabla_datos")
          )
        )
      ),
      
      # Tab 5: Estadísticas Avanzadas
      nav_panel(
        title = "Estadísticas",
        icon = bsicons::bs_icon("graph-up-arrow"),
        
        layout_column_wrap(
          width = 1/2,
          
          card(
            card_header("Resumen Estadístico General"),
            tableOutput("tabla_resumen_stats")
          ),
          
          card(
            card_header("Índices de Diversidad"),
            tableOutput("tabla_diversidad")
          )
        ),
        
        card(
          card_header("Análisis de Completitud de Datos"),
          plotlyOutput("completitud_datos", height = 400)
        ),
        
        layout_column_wrap(
          width = 1/2,
          
          card(
            card_header("Curva de Acumulación de Especies"),
            plotlyOutput("curva_acumulacion", height = 350)
          ),
          
          card(
            card_header("Estimadores de Riqueza"),
            plotlyOutput("estimadores_riqueza", height = 350)
          )
        )
      ),
      
      # Tab 6: Conservación
      nav_panel(
        title = "Conservación",
        icon = bsicons::bs_icon("shield-check"),
        
        card(
          card_header("Análisis para Conservación"),
          p("Esta sección proporciona información relevante para estrategias de conservación de mariposas Ithomiini."),
          
          layout_column_wrap(
            width = 1/2,
            
            div(
              h5("Especies con Menor Número de Registros"),
              tableOutput("especies_raras"),
              class = "mb-4"
            ),
            
            div(
              h5("Áreas de Alta Diversidad"),
              plotlyOutput("hotspots_conservacion", height = 300)
            )
          )
        ),
        
        card(
          card_header("Análisis de Gaps de Muestreo"),
          leafletOutput("mapa_gaps", height = "50vh"),
          card_footer(
            "Las áreas en rojo indican zonas con potencial alto pero pocos registros.",
            class = "text-muted small"
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Valores reactivos
  valores <- reactiveValues(
    centro_mapa = c(-75, -5),
    zoom_mapa = 5,
    datos_mapa = NULL,
    marcador_seleccionado = NULL
  )
  
  # Filtros anidados: Género -> Especie -> Subespecie
  output$ui_filtro_genero <- renderUI({
    pickerInput(
      "filtro_genero",
      "Género:",
      choices = c("Todos", sort(unique(datos_mariposas$genus))),
      selected = "Todos",
      options = list(
        liveSearch = TRUE,
        size = 5,
        style = "btn-primary btn-block",
        dropupAuto = FALSE,
        container = "body"
      )
    )
  })
  
  output$ui_filtro_especie <- renderUI({
    # Si hay un género seleccionado, filtrar especies por ese género
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      especies_disponibles <- datos_mariposas %>% 
        filter(genus == input$filtro_genero) %>%
        pull(species) %>%
        unique() %>%
        sort()
      
      pickerInput(
        "filtro_especie",
        "Especie:",
        choices = c("Todas", especies_disponibles),
        selected = "Todas",
        options = list(
          liveSearch = TRUE,
          size = 5,
          style = "btn-info btn-block",
          dropupAuto = FALSE,
          container = "body"
        )
      )
    } else {
      # Si no hay género seleccionado o es "Todos", mostrar todas las especies
      pickerInput(
        "filtro_especie",
        "Especie:",
        choices = c("Todas", sort(unique(datos_mariposas$species))),
        selected = "Todas",
        options = list(
          liveSearch = TRUE,
          size = 5,
          style = "btn-info btn-block",
          dropupAuto = FALSE,
          container = "body"
        )
      )
    }
  })
  
  output$ui_filtro_subespecie <- renderUI({
    # Si hay una especie seleccionada, filtrar subespecies por esa especie
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      # Filtrar por género si está seleccionado
      df <- datos_mariposas
      if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
        df <- df %>% filter(genus == input$filtro_genero)
      }
      
      # Filtrar por especie
      subespecies_disponibles <- df %>% 
        filter(species == input$filtro_especie) %>%
        pull(sub_species) %>%
        unique() %>%
        sort()
      
      # Si no hay subespecies, mostrar mensaje adecuado
      if(length(subespecies_disponibles) == 0) {
        subespecies_disponibles <- "No hay subespecies"
      }
      
      pickerInput(
        "filtro_subespecie",
        "Subespecie:",
        choices = c("Todas", subespecies_disponibles),
        selected = "Todas",
        options = list(
          liveSearch = TRUE,
          size = 5,
          style = "btn-success btn-block",
          dropupAuto = FALSE,
          container = "body"
        )
      )
    } else {
      # Si no hay especie seleccionada, deshabilitar el filtro de subespecies
      pickerInput(
        "filtro_subespecie",
        "Subespecie:",
        choices = c("Seleccione primero una especie"),
        selected = "Seleccione primero una especie",
        options = list(
          liveSearch = TRUE,
          size = 5,
          style = "btn-secondary btn-block",
          dropupAuto = FALSE,
          container = "body"
        )
      )
    }
  })
  
  # Datos filtrados según selección
  datos_filtrados <- reactive({
    # Iniciar con todos los datos
    df <- datos_mariposas
    
    # Filtros taxonómicos
    if(!is.null(input$filtro_genero) && input$filtro_genero != "Todos") {
      df <- df %>% filter(genus == input$filtro_genero)
    }
    
    if(!is.null(input$filtro_especie) && input$filtro_especie != "Todas") {
      df <- df %>% filter(species == input$filtro_especie)
    }
    
    if(!is.null(input$filtro_subespecie) && 
       input$filtro_subespecie != "Todas" && 
       input$filtro_subespecie != "No hay subespecies" &&
       input$filtro_subespecie != "Seleccione primero una especie") {
      df <- df %>% filter(sub_species == input$filtro_subespecie)
    }
    
    # Filtros geográficos
    if(!is.null(input$filtro_pais) && length(input$filtro_pais) > 0 && !"Todos" %in% input$filtro_pais) {
      df <- df %>% filter(country %in% input$filtro_pais)
    }
    
    # Si no hay datos, devolver un dataframe vacío pero con estructura correcta
    if(nrow(df) == 0) {
      return(datos_mariposas[0, ])
    }
    
    return(df)
  })
  
  # Datos para el mapa (con control de cantidad)
  datos_mapa <- reactive({
    # Filtrar solo registros con coordenadas válidas para el mapa
    df <- datos_filtrados() %>%
      filter(!is.na(latitude) & !is.na(longitude))
    
    # Si no hay datos con coordenadas, devolver dataframe vacío con estructura
    if(nrow(df) == 0) {
      return(datos_mariposas[0, ])
    }
    
    # Asignar un color por defecto
    df$color_group <- as.factor("default")
    df$marker_color <- "#3a4a8c"
    
    return(df)
  })
  
  # Añadir resumen de fuentes
  output$resumen_fuentes <- renderTable({
    df <- datos_filtrados()
    
    # Si no hay datos, mostrar mensaje
    if(nrow(df) == 0) {
      return(data.frame(Mensaje = "No hay datos que mostrar"))
    }
    
    # Resumir por fuente
    df %>%
      count(source_file) %>%
      rename(Fuente = source_file, Registros = n) %>%
      arrange(desc(Registros))
  }, striped = TRUE, bordered = FALSE, hover = TRUE)

  # Outputs de resumen
  output$resumenRegistros <- renderText({
    format(nrow(datos_filtrados()), big.mark = ",")
  })
  
  # Información de selección
  output$info_seleccion <- renderText({
    total_filtrados <- nrow(datos_filtrados())
    con_coords <- nrow(datos_mapa())
    
    # Texto por defecto si no hay coordenadas
    return("No hay registros con coordenadas válidas para mostrar en el mapa")
  })
  
  # Mapa principal
  output$mapa_principal <- renderLeaflet({
    # Valores predeterminados por si no están definidos los inputs
    estilo_mapa <- if(is.null(input$mapa_estilo)) "CartoDB.Positron" else input$mapa_estilo
    
    leaflet() %>%
      addProviderTiles(providers[[estilo_mapa]]) %>%
      setView(lng = -75, lat = 0, zoom = 4) %>%
      addScaleBar(position = "bottomright")
  })
  
  # Actualizar marcadores del mapa
  observe({
    # Verificar que el mapa principal ya exista
    req(input$mapa_estilo)
    
    df <- datos_mapa()
    
    # Utilizar leafletProxy solo si el mapa ya ha sido renderizado
    tryCatch({
      map <- leafletProxy("mapa_principal")
      
      map %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearHeatmap()
      
      if(nrow(df) > 0) {
        # Usar clusters para agrupar los puntos
        map %>%
          addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            popup = ~paste0(
              "<strong>", genus, " ", species, "</strong><br>",
              "País: ", country, "<br>",
              "Coordenadas: ", round(latitude, 4), ", ", round(longitude, 4)
            ),
            radius = 6,
            color = df$marker_color,
            fillColor = df$marker_color,
            fillOpacity = 0.7,
            weight = 2,
            clusterOptions = markerClusterOptions(
              spiderfyOnMaxZoom = TRUE,
              showCoverageOnHover = TRUE,
              zoomToBoundsOnClick = TRUE,
              iconCreateFunction = JS("
                function(cluster) {
                  return L.divIcon({
                    html: '<div style=\"background-color: #3a4a8c; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; width: 30px; height: 30px; font-weight: bold;\">' + cluster.getChildCount() + '</div>',
                    className: 'marker-cluster',
                    iconSize: L.point(30, 30)
                  });
                }
              ")
            )
          )
      }
    }, error = function(e) {
      # En caso de error, simplemente lo ignoramos
      message("Error al actualizar el mapa: ", e$message)
    })
  })
  
  # Gráficos del dashboard principal - simplificados para evitar errores
  output$grafico_paises <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Sin datos para mostrar"))
    }
    
    df <- df %>%
      count(country) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(df, x = ~reorder(country, n), y = ~n, type = 'bar',
            marker = list(color = '#3a4a8c'),
            hovertemplate = '%{x}<br>Registros: %{y}<extra></extra>') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Registros"))
  })
  
  output$grafico_generos <- renderPlotly({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Sin datos para mostrar"))
    }
    
    df <- df %>%
      count(genus) %>%
      arrange(desc(n)) %>%
      head(8)
    
    plot_ly(df, labels = ~genus, values = ~n, type = 'pie') %>%
      layout(showlegend = FALSE)
  })
  
  output$grafico_altitud <- renderPlotly({
    plotly_empty() %>% 
      layout(title = "Gráfico no disponible")
  })
  
  output$grafico_habitat <- renderPlotly({
    plotly_empty() %>% 
      layout(title = "Gráfico no disponible")
  })
  
  # Análisis ecológico
  output$violin_altitud <- renderPlotly({
    df <- datos_filtrados() %>%
      filter(!is.na(altitude), !is.na(genus)) %>%
      group_by(genus) %>%
      filter(n() >= 5) %>%
      ungroup()
    
    if(nrow(df) > 0) {
      plot_ly(df, x = ~genus, y = ~altitude, type = 'violin',
              box = list(visible = TRUE),
              meanline = list(visible = TRUE),
              color = ~genus,
              colors = viridis(n_distinct(df$genus))) %>%
        layout(title = "Distribución altitudinal por género",
               xaxis = list(title = "Género"),
               yaxis = list(title = "Altitud (m)"),
               showlegend = FALSE)
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis")
    }
  })
  
  output$diversidad_zona <- renderPlotly({
    df <- datos_filtrados() %>%
      filter(!is.na(lat_zone)) %>%
      group_by(lat_zone) %>%
      summarise(
        n_especies = n_distinct(scientific_name),
        n_registros = n(),
        shannon = -sum(prop.table(table(scientific_name)) * 
                      log(prop.table(table(scientific_name))))
      ) %>%
      filter(n_registros >= 10)
    
    if(nrow(df) > 0) {
      plot_ly(df, x = ~lat_zone, y = ~shannon, type = 'bar',
              marker = list(color = ~n_especies,
                           colorscale = 'Viridis',
                           showscale = TRUE,
                           colorbar = list(title = "N° Especies")),
              text = ~paste("Especies:", n_especies, "<br>Registros:", n_registros),
              hovertemplate = '%{x}<br>Índice Shannon: %{y:.2f}<br>%{text}<extra></extra>') %>%
        layout(title = "Diversidad (Shannon) por zona latitudinal",
               xaxis = list(title = "Zona"),
               yaxis = list(title = "Índice de Shannon"))
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis")
    }
  })
  
  output$matriz_coocurrencia <- renderPlotly({
    # Seleccionar las 15 especies más comunes
    top_species <- datos_filtrados() %>%
      count(scientific_name) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      pull(scientific_name)
    
    if(length(top_species) >= 2) {
      # Crear matriz de co-ocurrencia por localidad
      cooc_matrix <- matrix(0, nrow = length(top_species), ncol = length(top_species))
      rownames(cooc_matrix) <- top_species
      colnames(cooc_matrix) <- top_species
      
      # Calcular co-ocurrencias
      df_locs <- datos_filtrados() %>%
        filter(scientific_name %in% top_species) %>%
        select(scientific_name, latitude, longitude) %>%
        mutate(location = paste(round(latitude, 2), round(longitude, 2)))
      
      for(i in 1:length(top_species)) {
        for(j in i:length(top_species)) {
          locs_i <- df_locs %>% 
            filter(scientific_name == top_species[i]) %>% 
            pull(location)
          locs_j <- df_locs %>% 
            filter(scientific_name == top_species[j]) %>% 
            pull(location)
          
          cooc_matrix[i, j] <- length(intersect(locs_i, locs_j))
          cooc_matrix[j, i] <- cooc_matrix[i, j]
        }
      }
      
      plot_ly(
        z = cooc_matrix,
        x = colnames(cooc_matrix),
        y = rownames(cooc_matrix),
        type = "heatmap",
        colorscale = "Blues",
        hovertemplate = '%{x}<br>%{y}<br>Co-ocurrencias: %{z}<extra></extra>'
      ) %>%
        layout(title = "Matriz de co-ocurrencia espacial",
               xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = ""))
    } else {
      plotly_empty() %>%
        layout(title = "Datos insuficientes para análisis")
    }
  })
  
  # Análisis temporal
  output$serie_temporal <- renderPlotly({
    df <- datos_filtrados() %>%
      filter(!is.na(collection_date)) %>%
      mutate(year_month = format(collection_date, "%Y-%m")) %>%
      count(year_month) %>%
      mutate(date = as.Date(paste0(year_month, "-01")))
    
    if(nrow(df) > 0) {
      plot_ly(df, x = ~date, y = ~n, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#3a4a8c', width = 3),
              marker = list(color = '#5a6aac', size = 8)) %>%
        layout(title = "Registros por mes",
               xaxis = list(title = "Fecha"),
               yaxis = list(title = "Número de registros"))
    } else {
      plotly_empty() %>%
        layout(title = "Sin datos temporales disponibles")
    }
  })
  
  # Información sobre registros en tabla
  output$info_registros_tabla <- renderText({
    total <- nrow(datos_filtrados())
    con_coords <- sum(datos_filtrados()$has_coords)
    sin_coords <- total - con_coords
    paste0("Total: ", format(total, big.mark = ","), 
           " (", format(con_coords, big.mark = ","), " con coordenadas, ",
           format(sin_coords, big.mark = ","), " sin coordenadas)")
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    df <- datos_filtrados() %>%
      select(record_id, scientific_name, sub_species, country, 
             latitude, longitude, altitude, habitat, 
             collection_date, collector, source_file, has_coords)
    
    datatable(
      df,
      options = list(
        pageLength = 15,
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
      formatRound(columns = c('latitude', 'longitude'), digits = 4) %>%
      formatRound(columns = 'altitude', digits = 0) %>%
      formatStyle('has_coords',
                  backgroundColor = styleEqual(c(TRUE, FALSE), 
                                             c('#d4edda', '#f8d7da')))
  })
  
  # Descargas
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("ithomiini_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("ithomiini_todos_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_mariposas, file, row.names = FALSE)
    }
  )

  # Resetear filtros
  observeEvent(input$reset_filtros, {
    updatePickerInput(session, "filtro_genero", selected = "Todos")
    # Los otros filtros se actualizarán automáticamente debido a la dependencia
  })
}

shinyApp(ui, server)