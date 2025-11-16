#Aplicación Shiny emigración El Salvador 2014-2024 - Trabajo Grupal AD#
#Agregando tabla por grupo etario y año# 
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)

pop <- read.csv("BDDEmigra2024.csv", 
                sep = ";", stringsAsFactors = FALSE)
# UI - Interfaz de usuario profesional
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # CSS personalizado para mejorar la estética
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      /* Estilos generales */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f8f9fa;
      }
      
      /* Header personalizado */
      .custom-header {
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        color: white;
        padding: 20px 0;
        margin-bottom: 20px;
        border-radius: 0 0 10px 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      /* Panel de filtros */
      .filter-panel {
        background: white;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        padding: 20px;
        height: fit-content;
        margin-bottom: 20px;
      }
      
      /* Contenedores de gráficos y tablas */
      .content-panel {
        background: white;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      
      /* Títulos de secciones */
      .section-title {
        color: #2c3e50;
        border-bottom: 3px solid #3498db;
        padding-bottom: 10px;
        margin-bottom: 20px;
        font-weight: 600;
        position: relative;
      }
      
      /* Subtítulo del gráfico */
      .graph-subtitle {
        color: #7f8c8d;
        font-size: 0.9em;
        margin-top: -15px;
        margin-bottom: 20px;
        font-style: italic;
      }
      
      /* Botones personalizados */
      .btn-custom {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        border: none;
        border-radius: 5px;
        padding: 10px 20px;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .btn-custom:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(52, 152, 219, 0.3);
      }
      
      .btn-reset {
        background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
        color: white;
        border = none;
        border-radius: 5px;
        padding: 10px 20px;
        font-weight: 500;
        transition: all 0.3s ease;
        width: 100%;
      }
      
      .btn-reset:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(231, 76, 60, 0.3);
      }
      
      /* Selectores personalizados */
      .selectize-control.multi .selectize-input {
        border: 2px solid #ecf0f1;
        border-radius: 5px;
        padding: 8px;
        transition: border-color 0.3s ease;
      }
      
      .selectize-control.multi .selectize-input.focus {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      
      /* Slider personalizado */
      .irs--flat .irs-bar {
        background: #3498db;
      }
      
      .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single {
        background: #3498db;
      }
      
      .irs--flat .irs-handle>i:first-child {
        background: #2980b9;
      }
      
      /* Badges informativos */
      .info-badge {
        background: #3498db;
        color: white;
        padding: 3px 8px;
        border-radius: 12px;
        font-size: 0.8em;
        margin-left: 5px;
      }
      
      /* Pestañas personalizadas */
      .nav-tabs-custom > .nav-tabs > li.active > a {
        border-top-color: #3498db;
        font-weight: 600;
      }
    "))
  ),
  
  # Header personalizado
  div(class = "custom-header",
      div(class = "container-fluid",
          div(class = "row",
              div(class = "col-md-12",
                  h1(icon("chart-line"), " Análisis de Emigración - Censo 2024", 
                     style = "margin: 0; font-weight: 700;"),
                  p("Datos oficiales del Censo de Población y Vivienda 2024 - El Salvador",
                    style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1em;")
              )
          )
      )
  ),
  
  # Contenido principal
  div(class = "container-fluid",
      div(class = "row",
          
          # Panel de filtros - izquierda
          div(class = "col-md-3",
              div(class = "filter-panel",
                  h3(icon("filter"), " Filtros", class = "section-title"),
                  
                  # Información de uso
                  div(style = "background: #e8f4fc; padding: 10px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #3498db;",
                      p(icon("info-circle"), " Selecciona filtros para explorar los datos", 
                        style = "margin: 0; font-size: 0.9em; color: #2c3e50;")
                  ),
                  
                  # Filtro de Departamento
                  div(
                    h5(icon("map"), " Departamento",
                       span(class = "info-badge", textOutput("contador_deptos", inline = TRUE))),
                    selectizeInput("departamento", 
                                   label = NULL,
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = 'Todos los departamentos',
                                     plugins = list('remove_button'),
                                     maxItems = 20,
                                     maxOptions = 100
                                   ))
                  ),
                  
                  # Filtro de Municipio
                  div(style = "margin-top: 20px;",
                      h5(icon("building"), " Municipio",
                         span(class = "info-badge", textOutput("contador_munis", inline = TRUE))),
                      selectizeInput("municipio", 
                                     label = NULL,
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(
                                       placeholder = 'Todos los municipios',
                                       plugins = list('remove_button'),
                                       maxItems = 20,
                                       maxOptions = 500
                                     ))
                  ),
                  
                  # Filtro de Distrito
                  div(style = "margin-top: 20px;",
                      h5(icon("location-dot"), " Distrito",
                         span(class = "info-badge", textOutput("contador_distritos", inline = TRUE))),
                      selectizeInput("distrito", 
                                     label = NULL,
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(
                                       placeholder = 'Todos los distritos',
                                       plugins = list('remove_button'),
                                       maxItems = 20,
                                       maxOptions = 500
                                     ))
                  ),
                  
                  # Filtro de Años
                  div(style = "margin-top: 20px;",
                      h5(icon("calendar"), " Rango de Años"),
                      sliderInput("anios", 
                                  label = NULL,
                                  min = 2000, 
                                  max = 2024, 
                                  value = c(2000, 2024),
                                  sep = "",
                                  step = 1)
                  ),
                  
                  # Botones de acción
                  div(style = "margin-top: 25px;",
                      actionButton("select_all_deptos", "Seleccionar Todos los Departamentos",
                                   icon = icon("check-circle"),
                                   class = "btn-custom",
                                   style = "width: 100%; margin-bottom: 10px;"),
                      actionButton("reset_filtros", "Limpiar Todos los Filtros",
                                   icon = icon("broom"),
                                   class = "btn-reset")
                  ),
                  
                  # Información de resultados
                  div(style = "margin-top: 20px; padding: 15px; background: #f8f9fa; border-radius: 5px;",
                      h5(icon("chart-bar"), " Resumen:"),
                      textOutput("info_filtros"),
                      textOutput("total_registros"),
                      textOutput("rango_anios")
                  )
              )
          ),
          
          # Contenido principal - derecha
          div(class = "col-md-9",
              
              # Métricas rápidas
              div(class = "row",
                  div(class = "col-md-3",
                      div(style = "background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); color: white; padding: 15px; border-radius: 8px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                          h4(icon("users"), " Total"),
                          h3(textOutput("total_emigrantes", inline = TRUE))
                      )
                  ),
                  div(class = "col-md-3",
                      div(style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%); color: white; padding: 15px; border-radius: 8px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                          h4(icon("calendar"), " Edad Promedio"),
                          h3(textOutput("edad_promedio", inline = TRUE))
                      )
                  ),
                  div(class = "col-md-3",
                      div(style = "background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); color: white; padding: 15px; border-radius: 8px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                          h4(icon("globe-americas"), " Año Pico"),
                          h3(textOutput("ano_pico", inline = TRUE))
                      )
                  ),
                  div(class = "col-md-3",
                      div(style = "background: linear-gradient(135deg, #9b59b6 0%, #8e44ad 100%); color: white; padding: 15px; border-radius: 8px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                          h4(icon("chart-simple"), " Tendencia"),
                          h3(textOutput("tendencia", inline = TRUE))
                      )
                  )
              ),
              
              br(),
              
              # Gráfico principal
              div(class = "content-panel",
                  h3(icon("line-chart"), " Evolución de la Emigración por Año", 
                     class = "section-title"),
                  # Subtítulo debajo de la línea azul
                  div(class = "graph-subtitle",
                      textOutput("subtitulo_grafico")
                  ),
                  withSpinner(plotlyOutput("line_plot", height = "400px"), type = 6, color = "#3498db")
              ),
              
              # Tablas de datos
              div(class = "content-panel",
                  h3(icon("table"), " Datos Agregados", class = "section-title"),
                  
                  # Pestañas para las diferentes tablas
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      title = tagList(icon("chart-bar"), " Resumen por Año"),
                      br(),
                      p("Agregados anuales de emigración (corresponde a la gráfica superior)"),
                      withSpinner(DTOutput("tabla_resumen"), type = 6, color = "#3498db")
                    ),
                    tabPanel(
                      title = tagList(icon("list-alt"), " Detalle por Año y Causa"),
                      br(),
                      p("Desglose de emigración por año y causa de emigración"),
                      withSpinner(DTOutput("tabla_causas"), type = 6, color = "#3498db")
                    ),
                    tabPanel(
                      title = tagList(icon("users"), " Resumen por Año y Grupo Etario"),
                      br(),
                      p("Distribución de emigración por año y grupos de edad"),
                      withSpinner(DTOutput("tabla_etarios"), type = 6, color = "#3498db")
                    ),
                    tabPanel(
                      title = tagList(icon("table"), " Datos Individuales"),
                      br(),
                      p("Registros individuales de emigración"),
                      withSpinner(DTOutput("data_table"), type = 6, color = "#3498db")
                    )
                  )
              )
          )
      )
  )
)

# Server - Lógica de la aplicación
server <- function(input, output, session) {
  
  # Valores reactivos para almacenar las opciones disponibles
  opciones <- reactiveValues(
    departamentos = NULL,
    municipios = NULL,
    distritos = NULL
  )
  
  # Inicializar opciones cuando se carga pop
  observe({
    req(pop)
    
    # Obtener valores únicos
    opciones$departamentos <- sort(unique(pop$DEPTOL))
    opciones$municipios <- sort(unique(pop$MUNICL))
    opciones$distritos <- sort(unique(pop$DISTOL))
    
    # Debug: mostrar conteos en consola
    cat("Departamentos encontrados:", length(opciones$departamentos), "\n")
    cat("Municipios encontrados:", length(opciones$municipios), "\n")
    cat("Distritos encontrados:", length(opciones$distritos), "\n")
    
    # Actualizar rango de años basado en datos reales
    anos_range <- range(pop$E01_5_EMI_ANO, na.rm = TRUE)
    
    updateSliderInput(session, "anios", 
                      min = anos_range[1],
                      max = anos_range[2],
                      value = anos_range)
    
    # Inicializar selectores con todas las opciones disponibles
    updateSelectizeInput(session, "departamento", 
                         choices = opciones$departamentos,
                         selected = character(0))
    
    updateSelectizeInput(session, "municipio", 
                         choices = opciones$municipios,
                         selected = character(0))
    
    updateSelectizeInput(session, "distrito", 
                         choices = opciones$distritos,
                         selected = character(0))
  })
  
  # Actualizar municipios basados en departamento seleccionado
  observeEvent(input$departamento, {
    req(pop)
    
    if (length(input$departamento) == 0) {
      # Si no hay departamentos seleccionados, mostrar TODOS los municipios
      municipios_disponibles <- sort(unique(pop$MUNICL))
    } else {
      # Si hay departamentos seleccionados, filtrar municipios
      municipios_disponibles <- pop %>%
        filter(DEPTOL %in% input$departamento) %>%
        distinct(MUNICL) %>%
        pull(MUNICL) %>%
        sort()
    }
    
    # Mantener las selecciones actuales que todavía estén disponibles
    selecciones_actuales <- input$municipio
    selecciones_validas <- selecciones_actuales[selecciones_actuales %in% municipios_disponibles]
    
    updateSelectizeInput(session, "municipio", 
                         choices = municipios_disponibles,
                         selected = selecciones_validas)
  })
  
  # Actualizar distritos basados en municipio seleccionado
  observeEvent(input$municipio, {
    req(pop)
    
    if (length(input$municipio) == 0) {
      # Si no hay municipios seleccionados, mostrar TODOS los distritos
      distritos_disponibles <- sort(unique(pop$DISTOL))
    } else {
      # Si hay municipios seleccionados, filtrar distritos
      distritos_disponibles <- pop %>%
        filter(MUNICL %in% input$municipio) %>%
        distinct(DISTOL) %>%
        pull(DISTOL) %>%
        sort()
    }
    
    # Mantener las selecciones actuales que todavía estén disponibles
    selecciones_actuales <- input$distrito
    selecciones_validas <- selecciones_actuales[selecciones_actuales %in% distritos_disponibles]
    
    updateSelectizeInput(session, "distrito", 
                         choices = distritos_disponibles,
                         selected = selecciones_validas)
  })
  
  # También actualizar distritos cuando cambien los departamentos (para casos donde municipios estén vacíos)
  observeEvent(input$departamento, {
    req(pop)
    
    # Si no hay municipios seleccionados, pero sí departamentos, actualizar distritos también
    if (length(input$departamento) > 0 && length(input$municipio) == 0) {
      distritos_disponibles <- pop %>%
        filter(DEPTOL %in% input$departamento) %>%
        distinct(DISTOL) %>%
        pull(DISTOL) %>%
        sort()
      
      # Mantener las selecciones actuales que todavía estén disponibles
      selecciones_actuales <- input$distrito
      selecciones_validas <- selecciones_actuales[selecciones_actuales %in% distritos_disponibles]
      
      updateSelectizeInput(session, "distrito", 
                           choices = distritos_disponibles,
                           selected = selecciones_validas)
    }
  })
  
  # Datos filtrados reactivos - maneja selecciones vacías
  datos_filtrados <- reactive({
    req(pop)
    
    datos <- pop
    
    # Aplicar filtros solo si hay selección
    if (length(input$departamento) > 0) {
      datos <- datos %>% filter(DEPTOL %in% input$departamento)
    }
    
    if (length(input$municipio) > 0) {
      datos <- datos %>% filter(MUNICL %in% input$municipio)
    }
    
    if (length(input$distrito) > 0) {
      datos <- datos %>% filter(DISTOL %in% input$distrito)
    }
    
    # Filtrar por años
    datos <- datos %>% 
      filter(E01_5_EMI_ANO >= input$anios[1] & E01_5_EMI_ANO <= input$anios[2])
    
    datos
  })
  
  # Datos agregados para el gráfico (por año)
  datos_agregados <- reactive({
    req(datos_filtrados())
    
    datos_filtrados() %>%
      group_by(Año = E01_5_EMI_ANO) %>%
      summarise(
        Total_Emigrantes = n(),
        Edad_Promedio = round(mean(E01_4_EMI_EDAD, na.rm = TRUE), 1),
        Edad_Mínima = min(E01_4_EMI_EDAD, na.rm = TRUE),
        Edad_Máxima = max(E01_4_EMI_EDAD, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Año)
  })
  
  # Datos agregados por año y causa
  datos_agregados_causa <- reactive({
    req(datos_filtrados())
    
    datos_filtrados() %>%
      group_by(Año = E01_5_EMI_ANO, Causa) %>%
      summarise(
        Total_Emigrantes = n(),
        Porcentaje = round(n() / nrow(datos_filtrados()) * 100, 2),
        .groups = 'drop'
      ) %>%
      arrange(Año, desc(Total_Emigrantes))
  })
  
  # Datos agregados por año y grupo etario (MODIFICADO)
  datos_agregados_etarios <- reactive({
    req(datos_filtrados())
    
    datos_filtrados() %>%
      group_by(Año = E01_5_EMI_ANO, Grupo_Etario = Etario) %>%
      summarise(
        Total_Emigrantes = n(),
        Porcentaje_Anual = round(n() / sum(n()) * 100, 2),
        Porcentaje_Total = round(n() / nrow(datos_filtrados()) * 100, 2),
        Edad_Promedio = round(mean(E01_4_EMI_EDAD, na.rm = TRUE), 1),
        Edad_Mínima = min(E01_4_EMI_EDAD, na.rm = TRUE),
        Edad_Máxima = max(E01_4_EMI_EDAD, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Año, desc(Total_Emigrantes))
  })
  
  # Contadores para los badges
  output$contador_deptos <- renderText({
    total_deptos <- length(opciones$departamentos)
    if (length(input$departamento) == 0) {
      paste0(total_deptos, "/", total_deptos)
    } else {
      paste0(length(input$departamento), "/", total_deptos)
    }
  })
  
  output$contador_munis <- renderText({
    if (length(input$departamento) == 0) {
      total_munis <- length(opciones$municipios)
    } else {
      total_munis <- pop %>%
        filter(DEPTOL %in% input$departamento) %>%
        distinct(MUNICL) %>%
        nrow()
    }
    
    if (length(input$municipio) == 0) {
      paste0(total_munis, "/", total_munis)
    } else {
      paste0(length(input$municipio), "/", total_munis)
    }
  })
  
  output$contador_distritos <- renderText({
    if (length(input$municipio) == 0) {
      if (length(input$departamento) == 0) {
        total_distritos <- length(opciones$distritos)
      } else {
        total_distritos <- pop %>%
          filter(DEPTOL %in% input$departamento) %>%
          distinct(DISTOL) %>%
          nrow()
      }
    } else {
      total_distritos <- pop %>%
        filter(MUNICL %in% input$municipio) %>%
        distinct(DISTOL) %>%
        nrow()
    }
    
    if (length(input$distrito) == 0) {
      paste0(total_distritos, "/", total_distritos)
    } else {
      paste0(length(input$distrito), "/", total_distritos)
    }
  })
  
  # Métricas para los value boxes
  output$total_emigrantes <- renderText({
    total <- nrow(datos_filtrados())
    format(total, big.mark = ",", scientific = FALSE)
  })
  
  output$edad_promedio <- renderText({
    edad <- mean(datos_filtrados()$E01_4_EMI_EDAD, na.rm = TRUE)
    paste0(round(edad, 1), " años")
  })
  
  output$ano_pico <- renderText({
    datos_agr <- datos_agregados()
    if (nrow(datos_agr) > 0) {
      datos_agr$Año[which.max(datos_agr$Total_Emigrantes)]
    } else {
      "N/A"
    }
  })
  
  output$tendencia <- renderText({
    datos_agr <- datos_agregados()
    if (nrow(datos_agr) >= 2) {
      ultimos <- tail(datos_agr$Total_Emigrantes, 2)
      cambio <- ((ultimos[2] - ultimos[1]) / ultimos[1]) * 100
      if (cambio > 0) {
        paste0("↗ ", round(cambio, 1), "%")
      } else if (cambio < 0) {
        paste0("↘ ", round(abs(cambio), 1), "%")
      } else {
        "→ Estable"
      }
    } else {
      "N/A"
    }
  })
  
  # Información de filtros
  output$info_filtros <- renderText({
    deptos <- if (length(input$departamento) == 0) "Todos" else paste(length(input$departamento), "seleccionados")
    munis <- if (length(input$municipio) == 0) "Todos" else paste(length(input$municipio), "seleccionados")
    distritos <- if (length(input$distrito) == 0) "Todos" else paste(length(input$distrito), "seleccionados")
    
    paste("Departamentos:", deptos, "| Municipios:", munis, "| Distritos:", distritos)
  })
  
  output$total_registros <- renderText({
    paste("Registros:", format(nrow(datos_filtrados()), big.mark = ","))
  })
  
  output$rango_anios <- renderText({
    paste("Años:", input$anios[1], "-", input$anios[2])
  })
  
  output$subtitulo_grafico <- renderText({
    deptos <- if (length(input$departamento) == 0) "Todos los departamentos" else paste(length(input$departamento), "departamentos")
    munis <- if (length(input$municipio) == 0) "todos los municipios" else paste(length(input$municipio), "municipios")
    distritos <- if (length(input$distrito) == 0) "todos los distritos" else paste(length(input$distrito), "distritos")
    
    paste("Mostrando:", deptos, "-", munis, "-", distritos)
  })
  
  # Gráfico principal de líneas
  output$line_plot <- renderPlotly({
    req(datos_agregados())
    
    datos <- datos_agregados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "No hay datos disponibles para los filtros seleccionados",
                   x = 0.5,
                   y = 0.5,
                   xanchor = 'center',
                   yanchor = 'middle'
                 )
               ))
    }
    
    # Calcular tendencia
    tendencia <- lm(Total_Emigrantes ~ Año, data = datos)
    
    plot_ly(datos) %>%
      add_trace(x = ~Año, y = ~Total_Emigrantes,
                type = 'scatter', mode = 'lines+markers',
                name = 'Emigrantes',
                line = list(color = '#3498db', width = 4, shape = 'spline'),
                marker = list(color = '#2980b9', size = 8, 
                              line = list(color = 'white', width = 2)),
                hovertemplate = paste(
                  "<b>Año:</b> %{x}<br>",
                  "<b>Total Emigrantes:</b> %{y:,}<br>",
                  "<extra></extra>"
                )) %>%
      add_trace(x = ~Año, y = ~fitted(tendencia),
                type = 'scatter', mode = 'lines',
                name = 'Tendencia',
                line = list(color = '#e74c3c', width = 2, dash = 'dash'),
                hoverinfo = 'skip') %>%
      layout(
        title = list(
          text = "Evolución Temporal de la Emigración Salvadoreña",
          font = list(size = 16, color = '#2c3e50')
        ),
        xaxis = list(
          title = "Año",
          tickangle = -45,
          gridcolor = '#ecf0f1',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Número de Emigrantes",
          gridcolor = '#ecf0f1',
          showgrid = TRUE
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        hoverlabel = list(
          bgcolor = 'white',
          bordercolor = '#3498db',
          font = list(color = '#2c3e50')
        ),
        legend = list(
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2,
          yanchor = 'top'
        ),
        margin = list(t = 50, b = 80)
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d'),
             displaylogo = FALSE)
  })
  
  # Tabla 1: Resumen por año (corresponde a la gráfica)
  output$tabla_resumen <- renderDT({
    req(datos_agregados())
    
    datos <- datos_agregados() %>%
      mutate(
        Total_Emigrantes = format(Total_Emigrantes, big.mark = ",", scientific = FALSE)
      )
    
    datatable(
      datos,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copiar'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'cell-border stripe hover',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #2c3e50; font-weight: bold;',
        'Resumen Anual de Emigración'
      )
    ) %>%
      formatStyle(
        columns = names(datos),
        backgroundColor = 'white',
        color = '#2c3e50'
      )
  })
  
  # Tabla 2: Agregados por año y causa
  output$tabla_causas <- renderDT({
    req(datos_agregados_causa())
    
    datos <- datos_agregados_causa() %>%
      mutate(
        Total_Emigrantes = format(Total_Emigrantes, big.mark = ",", scientific = FALSE),
        Porcentaje = paste0(Porcentaje, "%")
      )
    
    datatable(
      datos,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copiar'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'cell-border stripe hover',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #2c3e50; font-weight: bold;',
        'Emigración por Año y Causa'
      )
    ) %>%
      formatStyle(
        columns = names(datos),
        backgroundColor = 'white',
        color = '#2c3e50'
      )
  })
  
  # Tabla 3: Resumen por año y grupo etario (MODIFICADA)
  output$tabla_etarios <- renderDT({
    req(datos_agregados_etarios())
    
    datos <- datos_agregados_etarios() %>%
      mutate(
        Total_Emigrantes = format(Total_Emigrantes, big.mark = ",", scientific = FALSE),
        Porcentaje_Anual = paste0(Porcentaje_Anual, "%"),
        Porcentaje_Total = paste0(Porcentaje_Total, "%")
      ) %>%
      select(
        Año,
        Grupo_Etario,
        Total_Emigrantes,
        `Porcentaje Anual` = Porcentaje_Anual,
        `Porcentaje Total` = Porcentaje_Total,
        Edad_Promedio,
        Edad_Mínima,
        Edad_Máxima
      )
    
    datatable(
      datos,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copiar'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'cell-border stripe hover',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #2c3e50; font-weight: bold;',
        'Distribución de Emigración por Año y Grupo Etario'
      )
    ) %>%
      formatStyle(
        columns = names(datos),
        backgroundColor = 'white',
        color = '#2c3e50'
      )
  })
  
  # Tabla 4: Datos individuales (original)
  output$data_table <- renderDT({
    req(datos_filtrados())
    
    datos <- datos_filtrados() %>%
      select(
        Departamento = DEPTOL,
        Municipio = MUNICL,
        Distrito = DISTOL,
        Sexo,
        Edad = E01_4_EMI_EDAD,
        Año = E01_5_EMI_ANO,
        País = Pais,
        Causa
      )
    
    datatable(
      datos,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copiar'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe hover',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #2c3e50; font-weight: bold;',
        'Registros Individuales de Emigración'
      )
    ) %>%
      formatStyle(
        columns = names(datos),
        backgroundColor = 'white',
        color = '#2c3e50'
      )
  })
  
  # Botón "Seleccionar Todos los Departamentos"
  observeEvent(input$select_all_deptos, {
    updateSelectizeInput(session, "departamento", 
                         selected = opciones$departamentos)
  })
  
  # Botón "Limpiar Todos los Filtros"
  observeEvent(input$reset_filtros, {
    updateSelectizeInput(session, "departamento", 
                         selected = character(0))
    updateSelectizeInput(session, "municipio", 
                         selected = character(0))
    updateSelectizeInput(session, "distrito", 
                         selected = character(0))
    
    # Resetear años al rango completo
    anos_range <- range(pop$E01_5_EMI_ANO, na.rm = TRUE)
    updateSliderInput(session, "anios", 
                      value = anos_range)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
