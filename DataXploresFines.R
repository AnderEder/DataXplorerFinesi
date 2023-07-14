library(shiny)
library(DT)
library(shinyWidgets)
library(psych)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(magrittr)
library(caret)
library(tidyverse)
library(sjPlot)
library(formattable)
library(rmarkdown)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Estilos CSS */
      /* Estilo para la pestaña 'Inicio' */
      .nav-tabs > li[data-value='inicio'] > a {
        background-color: #51F0FD;
        color: #fff;
        font-weight: bold;
      }

      body {
        background-color: #F2F2F2;
        font-family: Arial, sans-serif;
      }

      .header {
        background-color: #2A1849;
        color: #fff;
        padding: 10px;
        font-size: 36px;
        text-align: center;
      }

      .content {
        margin: 20px;
      }

      .logo {
        text-align: left;
        margin-bottom: 10px;
      }

      .title {
        text-align: center;
        margin-bottom: 20px;
        font-size: 24px;
        font-weight: bold;
      }

      .data-upload-form {
        max-width: 500px;
        margin: 0 auto;
      }

      .data-upload-form .form-group {
        margin-bottom: 20px;
      }

      .data-upload-form .btn-primary {
        width: 100%;
      }

      .data-table {
        margin-top: 20px;
      }

      /* Animación de las opciones del menú */
      .nav-tabs > li > a {
        transition: font-size 0.5s;
      }

      .nav-tabs > li > a:focus {
        font-size: 110%;
        background-color: #A2FF24;
        color: #010101;
      }


      /* Estilo para la imagen de presentación */
      .image-presentation {
        max-width: 90%;
        margin: 0 auto;
      }
      
     /* Estilos para la pestaña 'Resumen de Datos' */
      #resumen_datos {
        background-color: #F6F6F6;
        padding: 20px;
      }
      
      /* Estilos para la pestaña 'Gráficos básicos' */
      #graficos_basicos {
        background-color: #F2F9FF;
        padding: 20px;
      }
      
      /* Estilos para la pestaña 'Análisis de regresión' */
      #analisis_regresion {
        background-color: #E9F9E1;
        padding: 20px;
      }

    "))
  ),
  tags$div(class = "header",
           tags$div(class = "logo",
                    style = "text-align: center;",
                    img(src = "https://cdn-icons-png.flaticon.com/512/2965/2965300.png", height = "100px")
           ),
           tags$h1("DataXplorerFines")
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel("Inicio", value = "inicio",
             tags$div(class = "content",
                      tags$div(
                        style = "background-color: #EAF2F8; border: 2px solid #135091; border-radius: 10px; padding: 20px;",
                        tags$h3(class = "title", "Bienvenido a nuestra aplicación Shiny"),
                        tags$p("Aquí encontrarás una variedad de funcionalidades para analizar y visualizar datos."),
                        tags$p("A continuación, te presentamos una descripción de las funcionalidades disponibles:"),
                        tags$ul(
                          tags$li("Cargar datos: Permite cargar tus datos para su análisis y visualización."),
                          tags$li("Gestión de datos: Realiza diversas operaciones de gestión de datos para prepararlos para el análisis."),
                          tags$li("Visualizaciones interactivas: Genera visualizaciones interactivas de tus datos."),
                          tags$li("Análisis avanzado: Realiza análisis avanzados en tus datos."),
                          tags$li("Contacto: Si tienes alguna pregunta o necesitas ayuda, no dudes en contactarnos.")
                        ),
                        tags$p("¡Te invitamos a probar todas las funcionalidades y explorar tus datos de manera interactiva!")
                      )
             )
    ),
    
    
    
    
    ############## agregar esto ##################
    tabPanel("Cargar datos", value = "cargar_datos",
             tags$div(class = "content",
                      tags$h3(class = "title", "Cargar datos"),
                      tags$p("Selecciona el tipo de datos que deseas cargar:"),
                      shinyWidgets::pickerInput(inputId = "tipo_datos",
                                                label = "Tipo de datos",
                                                choices = c("CSV", "Excel"),
                                                multiple = FALSE,
                                                options = list(`actions-box` = TRUE,
                                                               `selected-text-format` = "count > 1"
                                                )
                      ),
                      fileInput(inputId = "archivo_datos",
                                label = "Archivo de datos"
                      ),
                      actionButton(inputId = "cargar_button",
                                   label = "Cargar datos",
                                   class = "btn-primary"
                      ),
                      DT::dataTableOutput("data_table")
             )
    ),
    
    #########################################################
    tabPanel("Gestión de datos", value = "gestion_datos",
             uiOutput("gestion_menu"),
             uiOutput("gestion_contenido"),
             verbatimTextOutput("resumen_estadistico")
    ),

    ###################################################
    ###### agregar  graficos ----
    
    tabPanel("Resumen de Datos", value = "resumen_datos",
             fluidRow(
               column(
                 width = 6,
                 selectInput(
                   inputId = "variable_select",
                   label = "Seleccionar Variable:",
                   choices = NULL,
                   selected = NULL
                 ),
                 plotOutput("hist_plot", width = "100%", height = "400px")
               ),
               column(
                 width = 6,
                 DT::DTOutput("summary_table")
               )
             )
    ),
    
    ####################################################
    tabPanel("Gráficos básicos", value = "graficos_basicos",
             fluidRow(
               column(
                 width = 6,
                 selectInput(
                   inputId = "tipo_grafico",
                   label = "Tipo de Gráfico:",
                   choices = c("Barras", "Dispersión", "Circular", "Diagrama de Cajas", "Puntos"),
                   selected = "Diagrama de Cajas"
                 ),
                 plotOutput("grafico_output", width = "100%", height = "400px")
               )
             )
    ),
    ####################### analissi avanvando ##############################
    tabPanel("Análisis avanzado", value = "analisis_avanzado",
             fluidPage(
               titlePanel(""),
               sidebarLayout(
                 sidebarPanel(
                   tags$div(
                     style = "background-color: #DBD439; padding: 20px; border-radius: 5px;",
                     tags$h4("Pasos:"),
                     tags$h6("Desarrollado en R Shiny"),
                     tags$h6("Para empezar, ingrese sus datos de Series de Tiempo que desea analizar."),
                     tags$h1(""),
                     fileInput("csvs",
                               label = "Subir serie de tiempo en formato .csv",
                               multiple = FALSE
                     ),
                     textInput("caption", "Ingrese el título de su gráfico", ""),
                     sliderInput("ForecastPer",
                                 "Número de períodos a pronosticar:",
                                 min = 1,
                                 max = 25,
                                 value = 5
                     )
                   )
                 ),
                 mainPanel(
                   style = "background-color: #078698; padding: 10px; border-radius: 7px; color: #FFFFFF",
                   tags$h1(".::::::Pronóstico de Series de Tiempo::::::."),
                   tags$h6("Modelos ARIMA y SARIMA"),
                   tags$h1(""),
                   tags$h4("Gráfica de la serie en niveles:"),
                   plotOutput("niveles"),
                   tags$h4("Estadístico Ljung-Box:"),
                   plotOutput("plot"),
                   tags$h4("Diferencias de la serie:"),
                   plotOutput("difer"),
                   tags$h4("Pronóstico:"),
                   plotOutput("pron"),
                   tags$h4("Valores del pronóstico:"),
                   tableOutput("values_pron")
                 )
               )
             )
    ),
    
      tabPanel("Contacto", value = "contacto",
               fluidPage(
                 div(style = "text-align: center; color: #078698 ",  # Agregamos el estilo CSS "text-align: center;"
                     titlePanel("Contacto")
                 ),
                 sidebarLayout(
                   sidebarPanel(
                     style = "background-color: #078698; padding: 10px; border-radius: 7px; color: #FFFFFF",
                     h4("Información de contacto"),
                     p("Si tienes alguna pregunta o necesitas ayuda, no dudes en contactarnos.")
                   ),
                   mainPanel(
                     style = "background-color: #A4E31D; padding: 10px; border-radius: 7px; color: #FFFFFF",
                     class = "text-center",  # Agregamos la clase CSS "text-center"
                     h4("Correos electrónicos"),
                     tags$div(
                       style = "display: inline-block;",  # Agregamos el estilo CSS "display: inline-block;"
                       p(a("ar.garcia@esta.unap.edu.pe", href = "mailto:ar.garcia@esta.unap.edu.pe", style = "color: #FFFFFF; text-decoration: underline;")),
                       p(a("ea.quispe@est.unap.edu.pe", href = "mailto:ea.quispe@est.unap.edu.pe", style = "color: #FFFFFF; text-decoration: underline;"))
                     )
                   )
                 )
               )
      )
    )
  )


#### Define el servidor ####
server <- function(input, output, session) {
  
  
  
  #### Datos cargados ####
  datos <- reactiveVal(NULL)
  
  # Mostrar los datos en una tabla
  output$data_table <- DT::renderDataTable({
    datos_cargados <- datos()
    
    if (!is.null(datos_cargados)) {
      DT::datatable(datos_cargados, class = "data-table")
    }
  })

  ##Cargar los datos al presionar el botón ##
  observeEvent(input$cargar_button, {
    tipo_datos <- input$tipo_datos
    archivo_datos <- input$archivo_datos
    
    if (!is.null(archivo_datos)) {
      datos_cargados <- NULL
      
      if (tipo_datos == "CSV") {
        datos_cargados <- read.csv2(archivo_datos$datapath)
      } else if (tipo_datos == "Excel") {
        datos_cargados <- readxl::read_excel(archivo_datos$datapath)
      }
      
      datos(datos_cargados)
      showNotification("Datos cargados exitosamente", type = "message")
      
    }
  })
  
  # Mostrar el panel "Inicio" al seleccionar la pestaña
  observeEvent(input$tabs, {
    if (input$tabs == "inicio") {
      updateTabsetPanel(session, "tabs", selected = "inicio")
    }
  })
  
  
  
  
  
  
  
  ##### Menú desplegable de gestión de datos ####
  output$gestion_menu <- renderUI({
    selectInput(inputId = "opcion_gestion",
                label = "Opción de gestión",
                choices = c("Limpieza de datos"),
                selected = "Limpieza de datos")
  })
  
  # Contenido del menú desplegable
  output$gestion_contenido <- renderUI({
    opcion_gestion <- input$opcion_gestion
    
    
    ## menu limpieza  ####
    
    if (opcion_gestion == "Limpieza de datos") {
      fluidRow(
        column(width = 12,
               actionButton(inputId = "eliminar_duplicados_button",
                            label = "Eliminar duplicados",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "eliminar_vacios_button",
                            label = "Eliminar filas con valores vacíos",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "eliminar_columnas_button",
                            label = "Eliminar columnas",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "renombrar_columnas_button",
                            label = "Renombrar columnas",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "rellenar_vacios_button",
                            label = "Rellenar valores vacíos",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "normalizar_datos_button",
                            label = "Normalizar datos",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "codificar_categoricas_button",
                            label = "Codificar variables categóricas",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "agregar_columna_button",
                            label = "Agregar columna",
                            class = "btn-primary")),
        column(width = 12,
               actionButton(inputId = "unir_columnas_button",
                            label = "Unir columnas",
                            class = "btn-primary"))
      )
    }
  })
  
  
  #### Funcionalidades resumen estadistico ####
  
  
  # Definir las funciones
  # Función para generar el gráfico de boxplot interactivo de una variable
  # Generar resumen estadístico al presionar el botón
  generarBoxplot <- function(variables) {
    tryCatch({
      datos_cargados <- datos()
      
      if (is.null(datos_cargados)) {
        showNotification("No se encontraron datos", type = "warning")
        return(NULL)
      }
      
      boxplot_data <- datos_cargados[, variables, drop = FALSE]
      
      # Generar el gráfico de boxplot con todas las variables seleccionadas
      boxplot(boxplot_data, outline = FALSE)
    }, error = function(e) {
      showNotification("Error al generar el gráfico de boxplot", type = "error")
      NULL
    })
  }
  
  generarBoxplot <- function(variables) {
    tryCatch({
      datos_cargados <- datos()
      
      if (is.null(datos_cargados)) {
        showNotification("No se encontraron datos", type = "warning")
        return(NULL)
      }
      
      boxplot_data <- datos_cargados[, variables, drop = FALSE]
      
      # Generar el gráfico de boxplot con todas las variables seleccionadas
      boxplot(boxplot_data, outline = FALSE)
    }, error = function(e) {
      showNotification("Error al generar el gráfico de boxplot", type = "error")
      NULL
    })
  }
  
  output$boxplot_output <- plotly::renderPlotly({
    variables_seleccionadas <- input$variables_seleccionadas
    
    if (!is.null(variables_seleccionadas) && length(variables_seleccionadas) > 0) {
      boxplot_result <- generarBoxplot(variables_seleccionadas)
      if (!is.null(boxplot_result)) {
        plotly::plot_ly(y = boxplot_result$stats, type = "box")
      }
    }
  })
  
  
  
  
  #### Funcionalidades de menu limpieza #####
  # Eliminar duplicados al presionar el botón
  observeEvent(input$eliminar_duplicados_button, {
    datos_cargados <- datos()
    
    tryCatch({
      datos_cargados <- unique(datos_cargados)
      datos(datos_cargados)
      showNotification("Duplicados eliminados exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al eliminar los duplicados", type = "error")
    })
  })
  
  
  # Eliminar filas con valores vacíos al presionar el botón
  observeEvent(input$eliminar_vacios_button, {
    datos_cargados <- datos()
    
    tryCatch({
      datos_cargados <- na.omit(datos_cargados)
      datos(datos_cargados)
      showNotification("Filas con valores vacíos eliminadas exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al eliminar filas con valores vacíos", type = "error")
    })
  })
  
  
  # Eliminar columnas al presionar el botón
  observeEvent(input$eliminar_columnas_button, {
    showModal(modalDialog(
      title = "Eliminar columnas",
      selectInput(inputId = "columnas_eliminar",
                  label = "Selecciona las columnas a eliminar",
                  choices = names(datos()),
                  multiple = TRUE),
      footer = tagList(
        actionButton(inputId = "eliminar_columnas_confirm_button", label = "Eliminar", class = "btn-primary"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$eliminar_columnas_confirm_button, {
    columnas_eliminar <- input$columnas_eliminar
    datos_cargados <- datos()
    
    tryCatch({
      datos_cargados <- datos_cargados[, !(names(datos_cargados) %in% columnas_eliminar)]
      datos(datos_cargados)
      removeModal()
      showNotification("Columnas eliminadas exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al eliminar las columnas", type = "error")
    })
  })
  
  
  # Renombrar columnas al presionar el botón
  observeEvent(input$renombrar_columnas_button, {
    showModal(modalDialog(
      title = "Renombrar columnas",
      selectInput(inputId = "columnas_renombrar",
                  label = "Selecciona las columnas a renombrar",
                  choices = names(datos()),
                  multiple = TRUE),
      actionButton(inputId = "renombrar_columnas_confirm_button", label = "Renombrar", class = "btn-primary"),
      footer = tagList(
        actionButton(inputId = "renombrar_columnas_confirm_button", label = "Renombrar", class = "btn-primary"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$renombrar_columnas_confirm_button, {
    columnas_renombrar <- input$columnas_renombrar
    nombres_nuevos <- character(length(columnas_renombrar))
    
    for (i in seq_along(columnas_renombrar)) {
      nombres_nuevos[i] <- textInput(inputId = paste0("nuevo_nombre_", i),
                                     label = paste("Nuevo nombre para", columnas_renombrar[i]),
                                     value = columnas_renombrar[i])
    }
    
    datos_cargados <- datos()
    
    tryCatch({
      for (i in seq_along(columnas_renombrar)) {
        datos_cargados <- rename(datos_cargados, !!as.name(columnas_renombrar[i]) := !!as.name(nombres_nuevos[i]))
      }
      
      datos(datos_cargados)
      removeModal()
      showNotification("Columnas renombradas exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al renombrar las columnas", type = "error")
    })
  })
  
  
  # Rellenar valores vacíos al presionar el botón
  observeEvent(input$rellenar_vacios_button, {
    showModal(modalDialog(
      title = "Rellenar valores vacíos",
      selectInput(inputId = "columnas_rellenar",
                  label = "Selecciona las columnas a rellenar",
                  choices = names(datos()),
                  multiple = TRUE),
      selectInput(inputId = "valor_relleno",
                  label = "Valor de relleno",
                  choices = c("Promedio", "Mediana", "Moda"),
                  selected = "Promedio"),
      footer = tagList(
        actionButton(inputId = "rellenar_vacios_confirm_button", label = "Rellenar", class = "btn-primary"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$rellenar_vacios_confirm_button, {
    columnas_rellenar <- input$columnas_rellenar
    valor_relleno <- input$valor_relleno
    datos_cargados <- datos()
    
    tryCatch({
      if (valor_relleno == "Promedio") {
        for (columna in columnas_rellenar) {
          datos_cargados[[columna]][is.na(datos_cargados[[columna]])] <- mean(datos_cargados[[columna]], na.rm = TRUE)
        }
      } else if (valor_relleno == "Mediana") {
        for (columna in columnas_rellenar) {
          datos_cargados[[columna]][is.na(datos_cargados[[columna]])] <- median(datos_cargados[[columna]], na.rm = TRUE)
        }
      } else if (valor_relleno == "Moda") {
        for (columna in columnas_rellenar) {
          datos_cargados[[columna]][is.na(datos_cargados[[columna]])] <- Mode(datos_cargados[[columna]])
        }
      }
      
      datos(datos_cargados)
      removeModal()
      showNotification("Valores vacíos rellenados exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al rellenar valores vacíos", type = "error")
    })
  })
  
  
  # Unir columnas al presionar el botón
  observeEvent(input$unir_columnas_button, {
    showModal(modalDialog(
      title = "Unir columnas",
      selectInput(inputId = "columnas_unir",
                  label = "Selecciona las columnas a unir",
                  choices = names(datos()),
                  multiple = TRUE),
      actionButton(inputId = "unir_columnas_confirm_button", label = "Unir", class = "btn-primary"),
      footer = tagList(
        actionButton(inputId = "unir_columnas_confirm_button", label = "Unir", class = "btn-primary"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$unir_columnas_confirm_button, {
    columnas_unir <- input$columnas_unir
    datos_cargados <- datos()
    
    tryCatch({
      datos_cargados$NuevaColumna <- do.call(paste, c(datos_cargados[columnas_unir], sep = ""))
      datos(datos_cargados)
      removeModal()
      showNotification("Columnas unidas exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Error al unir las columnas", type = "error")
    })
  })
  
  
  # Generar resumen estadístico al presionar el botón
  output$resumen_estadistico <- renderPrint({
    datos_cargados <- datos()
    
    if (!is.null(datos_cargados)) {
      summary(datos_cargados)
    }
  })
  
  
  #### 
  
  
  
  
  #### analisis avanzado funcianalidades ####
  # Función para calcular la integral de una serie de tiempo
  integrate_ts <- function(ts) {
    sum(ts) * frequency(ts)
  }
  
  # Función para diferenciar una serie de tiempo
  difference_ts <- function(ts) {
    n_diff <- forecast::ndiffs(ts, test = "adf")
    diff(ts, n_diff)
  }
  
  # Función para pronosticar una serie de tiempo usando un modelo SARIMA
  forecast_sarima <- function(ts, h) {
    modelo <- forecast::auto.arima(ts)
    pronostico <- forecast::forecast(modelo, h = h)
    pronostico$mean
  }
  
  # Código para los gráficos y pronósticos de la serie de tiempo
  output$niveles <- renderPlot({
    if (is.null(input$csvs$datapath)) {
      return()
    }
    var <- read.csv(input$csvs$datapath, header = TRUE)
    var_ts <- var[, 2]
    var <- var[, 1]
    var_ts <- as.numeric(var_ts)
    
    ts_var <- ts(var_ts, start = c(1990, 1), frequency = 1)
    
    plot(ts_var,
         main = input$caption,
         ylab = "Valor",
         xlab = "Año")
  })
  output$plot <- renderPlot({
    if (length(input$csvs$datapath[1]) == 0) {
      # Si no se ha seleccionado ningún archivo CSV, no se realiza ninguna acción
      return()
    } else {
      # Leer el archivo CSV
      var <- read.csv(input$csvs$datapath[1], header = TRUE, sep = ",")
      var_ts <- var[, 2] # Columna que contiene los valores de la serie de tiempo
      var <- var[, 1] # Columna que contiene los índices de tiempo
      
      # Procesar los índices de tiempo
      anios <- as.Date(var, format = "%Y-%m-%d")
      # Crear una serie de tiempo
      var_ts <- ts(var_ts, start = min(anios), frequency = 1)
      # Ajustar el modelo ARIMA y realizar el diagnóstico
      modelo <- forecast::auto.arima(var_ts)
      residuos <- residuals(modelo)
      forecast::checkresiduals(residuos)
    }
  })
  output$difer <- renderPlot({
    if (is.null(input$csvs$datapath)) {
      return()
    }
    
    var <- read.csv(input$csvs$datapath, header = TRUE)
    var_ts <- var[, 2]
    var <- var[, 1]
    var_ts <- as.numeric(var_ts)
    
    ts_var <- ts(var_ts, start = c(1990, 1), frequency = 1)
    n_diff <- forecast::ndiffs(ts_var, test = "adf")
    var_diff <- diff(ts_var, n_diff)
    
    plot(var_diff,
         main = "Diferencias de la serie",
         ylab = "Valor",
         xlab = "Año")
    
    abline(h = mean(var_diff), col = "blue")
  })
  
  output$pron <- renderPlot({
    if (is.null(input$csvs$datapath)) {
      return()
    }
    var <- read.csv(input$csvs$datapath, header = TRUE)
    var_ts <- var[, 2]
    var <- var[, 1]
    var_ts <- as.numeric(var_ts)
    
    ts_var <- ts(var_ts, start = c(1990, 1), frequency = 1)
    modelo <- forecast::auto.arima(ts_var)
    pronostico <- forecast::forecast(modelo, h = input$ForecastPer)
    
    plot(pronostico,
         main = "Pronóstico",
         ylab = "Valor",
         xlab = "Año")
  })
  output$values_pron <- renderTable({
    if (is.null(input$csvs$datapath)) {
      return()
    }
    
    var <- read.csv(input$csvs$datapath, header = TRUE)
    var_ts <- var[, 2]
    var <- var[, 1]
    var_ts <- as.numeric(var_ts)
    
    ts_var <- ts(var_ts, start = c(1990, 1), frequency = 1)
    modelo <- forecast::auto.arima(ts_var)
    pronostico <- forecast::forecast(modelo, h = input$ForecastPer)
    
    pronostico$mean
  })
  
  # Ejecutar las funciones cuando se cargue una serie de tiempo
  observeEvent(input$csvs, {
    if (is.null(input$csvs$datapath)) {
      return()
    }
    
    var <- read.csv(input$csvs$datapath, header = TRUE)
    var_ts <- var[, 2]
    var_ts <- as.numeric(var_ts)
    
    ts_var <- ts(var_ts, start = c(1990, 1), frequency = 1)
    
    # Calcular la integral de la serie de tiempo
    integral <- integrate_ts(ts_var)
    
    # Diferenciar la serie de tiempo
    diff_ts <- difference_ts(ts_var)
    
    # Pronosticar usando un modelo SARIMA
    sarima_forecast <- forecast_sarima(ts_var, input$ForecastPer)
    
    output$values_pron <- renderTable({
      data.frame(
        Integral = integral,
        Diferencia = mean(diff_ts),
        SARIMA = sarima_forecast
      )
    })
  })
  #############################################################################################
  #### Actualizar selecciones en la pestaña "Resumen de Datos" ----
  observeEvent(datos(), {
    if (!is.null(datos())) {
      updateSelectInput(session, "variable_select", choices = colnames(datos()))
    }
  })
  
  # Generar histograma y tabla resumen
  output$hist_plot <- renderPlot({
    if (!is.null(datos())) {
      ggplot(datos(), aes(x = .data[[input$variable_select]])) +
        geom_histogram(fill = "#007BFF", color = "white") +
        labs(title = "Histograma", x = input$variable_select, y = "Frecuencia") +
        theme_bw()
    }
  })
  
  output$summary_table <- renderDT({
    if (!is.null(datos())) {
      datatable(summary(datos()), options = list(paging = FALSE, searching = FALSE))
    }
  })
  
  
  
  
  #### Generar gráfico básico seleccionado ----
  output$grafico_output <- renderPlot({
    tipo_grafico <- input$tipo_grafico
    
    if (!is.null(datos())) {
      variable_select <- input$variable_select
      
      if (tipo_grafico == "Barras") {
        tryCatch({
          # Código para generar un gráfico de barras usando los datos
          ggplot(datos(), aes(x = .data[[variable_select]])) +
            geom_bar(fill = "#007BFF") +
            labs(title = "Gráfico de Barras", x = variable_select, y = "Frecuencia") +
            theme_bw()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el gráfico de barras", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Líneas") {
        tryCatch({
          # Código para generar un gráfico de líneas usando los datos
          ggplot(datos(), aes(x = .data[[variable_select]])) +
            geom_line(color = "#007BFF") +
            labs(title = "Gráfico de Líneas", x = variable_select, y = "Valores") +
            theme_bw()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el gráfico de líneas", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Dispersión") {
        tryCatch({
          # Código para generar un gráfico de dispersión usando los datos
          ggplot(datos(), aes(x = .data[[variable_select]], y = .data[[variable_select]])) +
            geom_point(color = "#007BFF") +
            labs(title = "Gráfico de Dispersión", x = variable_select, y = variable_select) +
            theme_bw()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el gráfico de dispersión", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Circular") {
        tryCatch({
          # Código para generar un gráfico circular usando los datos
          ggplot(datos(), aes(x = "")) +
            geom_bar(fill = "#007BFF", width = 1) +
            coord_polar(theta = "y") +
            labs(title = "Gráfico Circular", fill = "") +
            theme_void()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el gráfico circular", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Diagrama de Cajas") {
        tryCatch({
          # Código para generar un diagrama de cajas usando los datos
          ggplot(datos(), aes(x = "", y = .data[[variable_select]])) +
            geom_boxplot(fill = "#007BFF") +
            labs(title = "Diagrama de Cajas", y = variable_select) +
            theme_bw()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el diagrama de cajas", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Beanplots") {
        tryCatch({
          # Código para generar beanplots usando los datos (requiere el paquete 'beanplot')
          beanplot(.data[[variable_select]], col = "#007BFF", xlab = "", ylab = variable_select)
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar los beanplots", type = "warning")
          return(NULL)
        })
      } else if (tipo_grafico == "Puntos") {
        tryCatch({
          # Código para generar un gráfico de puntos usando los datos
          ggplot(datos(), aes(x = .data[[variable_select]], y = .data[[variable_select]])) +
            geom_point(color = "#007BFF") +
            labs(title = "Gráfico de Puntos", x = variable_select, y = variable_select) +
            theme_bw()
        }, error = function(e) {
          # Manejo de errores
          showNotification("Error al generar el gráfico de puntos", type = "warning")
          return(NULL)
        })
      }
    }
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)
