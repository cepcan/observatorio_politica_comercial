## APP para consultas de comercio con EE. UU. y dashboard ejecutivo
## Versión 2.3 - local (Solución de detalles menores de visualización)
## Nota: tema de warning en el ggplot de las barras para programa (label)
## Autor: Sergio Iglesias - CEPCAN
## Fecha: 30/09/2026

##Agregar 2 secciones (Laboratorio de datos)
# Comparativa de países 
# Gráfica arancel + tasas y (línea del tiempo)

## Nuestro data set por países cubre el 73.5% de la base de importaciones del USITC a 8 dígitos (2026) 
## 8471 (92.3%)

# Librerías ---------------------------------------------------------------
{
  if(require(tidyverse) == FALSE){
    install.packages("tidyverse")
    library(tidyvere)
  }else{
    library(tidyverse)
  }
  
  if(require(usethis) == FALSE){
    install.packages("usethis")
    library(usethis)
  }else{
    library(usethis)
  }
  
  if(require(httr2) == FALSE){
    install.packages("httr2")
    library(httr2)
  }else{
    library(httr2)
  }
  
  if(require(jsonlite) == FALSE){
    install.packages("jsonlite")
    library(jsonlite)
  }else{
    library(jsonlite)
  }
  
  if(require(memoise) == FALSE){
    install.packages("memoise")
    library(memoise)
  }else{
    library(memoise)
  }
  
  if(require(tibble) == FALSE){
    install.packages("tibble")
    library(tibble)
  }else{
    library(tibble)
  }
  
  if(require(shiny) == FALSE){
    install.packages("shiny")
    library(shiny)
  }else{
    library(shiny)
  }
  
  if(require(shinydashboard) == FALSE){
    install.packages("shinydashboard")
    library(shinydashboard)
  }else{
    library(shinydashboard)
  }
  
  if(require(plotly) == FALSE){
    install.packages("plotly")
    library(plotly)
  }else{
    library(plotly)
  }
  
  if(require(DT) == FALSE){
    install.packages("DT")
    library(DT)
  }else{
    library(DT)
  }
  
  if(require(scales) == FALSE){
    install.packages("scales")
    library(scales)
  }else{
    library(scales)
  }
  
  if(require(ggrepel) == FALSE){
    install.packages("ggrepel")
    library(ggrepel)
  }else{
    library(ggrepel)
  }
  
  if(require(shinymanager) == FALSE){
    install.packages("shinymanager")
    library(shinymanager)
  }else{
    library(shinymanager)
  }
  
  if(require(webshot2) == FALSE){
    install.packages("webshot2")
    library(webshot2)
  }else{
    library(webshot2)
  }

  if(require(duckdb) == FALSE){
    install.packages("duckdb")
    library(duckdb)
  }else{
    library(duckdb)
  }
}

# Contactar con la API ----------------------------------------------------
## TODAVÍA NO LO HE DESARROLLADO, APENAS ESTOY ENTENDIENDO COMO FUNCIONAN LAS API
## ESPERO QUE PROXIMAMENTE LO PODAMOS CONECTAR PARA HACER CONSULTAS INMEDIATAS EN TIEMPO REAL

## Actualización: el volumen de datos es inmenso y son muchas llamadas a la API de USITC
## Tengo que explorar más para aprender a hacerlo sin que nos corte el sistema

## Actualización 2: Milo está liderando esta parte

# Datos reales - 8hts - USITC junio 26 -------------------------------------
#12 países seleccionados
DATOS_USITC_BASE <- read_csv("InPutApp/data_panel_12_826.csv")
DATOS_USITC_TOTALES <- read_csv("InPutApp/datos_global_12_826.csv")

addResourcePath(prefix = "www", directoryPath = file.path(getwd(), "www"))

# Interfaz de usuario (UI) ------------------------------------------------
### Colores
paleta_programas <- c(
  "Sin programa" = "#d62728",
  "GSP" = "#954e9b",
  "AT Pharma" = "#e47c54",
  "AT Civil Aircraft" = "lightblue",
  "Origin Unknown" = "gray",
  "T-MEC" = "#2ca02c",
  "URC on Chemicals" = "#ffa600",
  "USJTA" = "darkblue",
  "KORUS FTA" = "#123160" 
)

# Contraseñas temporales de acceso ----------------------------------------
#credentials <- data.frame(
 # user = c("admincepcan", "economia_guest_2026", "jorgecepcan"),
  #password = c("cepcanecsg26", "se20mxg26", "cepjc26mtyg"),
  #is_admin = TRUE, FALSE, FALSE,
  #stringsAsFactors = FALSE
#)

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(
      tags$img(src = "www/cepcan.png", height = "50px", style = "margin-right: 8px; vertical-align: middle;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Ejecutivo", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Consulta Arancelaria (HS)", tabName = "consulta", icon = icon("search")),
      menuItem("Laboratorio de datos", tabName = "laboratorio", icon = icon("flask"))
    ),
    hr(),
    div(
      style = "padding: 10px;",
      selectInput("filter_country", "País de Origen/Socio:", 
                  choices = NULL, 
                  multiple = FALSE), 
      
      selectInput("filter_year", "Año de Análisis:", 
                  choices = NULL, 
                  multiple = TRUE),
      
      selectInput("hs_level_select", "Nivel de Desagregación HS:",
                  choices = c("HS-2 (Capítulos)" = 2,
                              "HS-4 (Partidas)" = 4,
                              "HS-6 (Subpartidas)" = 6,
                              "HTS-8 (Fracciones)" = 8),
                  selected = 8),
      
      textInput("hts_search_input", "Filtrar por Código HS:", 
                value = "", placeholder = "Ej. 01, 0101, 010121..."),
      
      actionButton("reset_hs_filter", "Reiniciar Filtro HS (Totales)", 
                   icon = icon("rotate-left"), 
                   class = "btn-warning btn-block", 
                   style = "margin-bottom: 15px; color: #000; font-weight: bold;"),
      
      radioButtons("filter_timeframe", "Frecuencia Temporal:",
                   choices = c("Anual" = "annual", "Vista Mensual" = "monthly"),
                   selected = "monthly"),
      uiOutput("nota_fuente")
    )
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        h2(textOutput("titulo_pais"), style = "font-weight: bold; color: #1f3a60; margin-top: 5px; margin-bottom: 15px;")
      )
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("kpi_customs_value", width = 3),
          valueBoxOutput("kpi_variacion", width = 3),
          valueBoxOutput("kpi_effective_rate", width = 2),
          valueBoxOutput("kpi_implicit_rate", width = 2),
          valueBoxOutput("kpi_dutiable_pct", width = 2)
        ),
        fluidRow(
          box(
            title = uiOutput("title_tmec_chart"), 
            status = "primary", solidHeader = TRUE, width = 8,
            div(
              style = "position: relative;", 
              div(
                style = "position: absolute; right: 15px; top: 25px; z-index: 10;",
                downloadButton("download_tmec_plot_hd", "HD", class = "btn-success btn-xs") 
                ),
            plotlyOutput("plot_tmec_bar_chart", height = "360px")
            )
          ),
          box(
            title = "Proporción de Programas Comerciales", 
            status = "info", solidHeader = TRUE, width = 4,
            div(
              style = "position: relative;", 
              div(
                style = "position: absolute; right: 15px; top: 25px; z-index: 10;",
                downloadButton("download_program_donut_hd", "HD", class = "btn-success btn-xs")
                ),
            plotlyOutput("plot_program_donut", height = "360px")
            )
          )
        ),
        fluidRow(
          box(
            title = "Principales Productos Importados (según nivel HS seleccionado)", 
            status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "position: relative;", 
              div(
                style = "position: absolute; right: 15px; top: 25px; z-index: 10;",
                downloadButton("download_top_products_hd", "Descarga HD", class = "btn-success btn-xs") 
              ),
            plotlyOutput("plot_top_products", height = "380px")
            )
          )
        ),
        fluidRow(
          box(title = "Principales países de origen de las importaciones seleccionadas",
              status = "primary", solidHeader = TRUE, width = 12,
              div(
                style = "position: relative;", 
                div(
                  style = "position: absolute; right: 15px; top: 25px; z-index: 10;",
                  downloadButton("download_top_countries_hd", "Descarga HD", class = "btn-success btn-xs") 
                ),
              plotlyOutput("plot_top_countries", height = "500px")
              )
              )
        )
      ), 
      tabItem(
        tabName = "consulta",
        fluidRow(
          box(
            title = "Exportación de Datos", 
            status = "warning", solidHeader = TRUE, width = 12,
            downloadButton("download_excel", "Exportar Consulta Actual a CSV", class = "btn-success")
          )
        ),
        fluidRow(
          box(
            title = "Tabla Consolidada Agrupada al Nivel HS Seleccionado", 
            status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("table_hs_query")
          )
        )
      ), 
      
      # LABORATORIO DE DATOS
      tabItem(
        tabName = "laboratorio",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Laboratorio de Datos",
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  inputId  = "tb_countries", 
                  label    = "Socios Comerciales a Analizar:",
                  choices  = NULL, 
                  multiple = TRUE,
                  options  = list(maxItems = 4, placeholder = "Selecciona hasta 4 países")
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId  = "tb_metrics", 
                  label    = "Selección de Indicadores:",
                  choices  = c(
                    "Importaciones"           = "customs_value",
                    "Valor Gravable"          = "dutiable_value",
                    "Aranceles Pagados"       = "calculated_duties",
                    "Tasa Implícita (%)"      = "tasa_implicita",
                    "Tasa Efectiva (%)"       = "tasa_efectiva",
                    "Base Gravable (%)" = "pct_gravable"
                  ),
                  selected = c("customs_value"),
                  multiple = TRUE,
                  options  = list(maxItems = 2, placeholder = "Selecciona 1 o 2 indicadores")
                )
              )
            )
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_us_exports", width = 4),
          valueBoxOutput("kpi_us_imports", width = 4),
          valueBoxOutput("kpi_trade_balance", width = 4)
          ),
        fluidRow(
          box(
            title = uiOutput("title_trade_balance_chart"), 
            status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "position: relative;", 
              div(
                style = "position: absolute; right: 15px; top: 25px; z-index: 10;",
                downloadButton("download_trade_plot_hd", "Descarga HD", class = "btn-success btn-xs")
              ),
              plotlyOutput("plot_trade_balance_series", height = "500px")
            )
          )
        )
      ) 
    ) 
  ) 
)

# Envolver UI en interfaz de seguridad ------------------------------------
#ui <- secure_app(
 # ui_base, language = "es"
#)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #Meses abreviados y largos en español
  meses_nombre_full <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                         "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  meses_nombre_largo <- c("Ene" = "Enero", "Feb" = "Febrero", "Mar" = "Marzo", 
                          "Abr" = "Abril", "May" = "Mayo", "Jun" = "Junio",
                          "Jul" = "Julio", "Ago" = "Agosto", "Sep" = "Septiembre", 
                          "Oct" = "Octubre", "Nov" = "Noviembre", "Dic" = "Diciembre")
  
  ### REACTIVO CENTRAL DE ÚLTIMOS DATOS PUBLICADOS 
  info_ultima_fecha <- reactive({
    req(exists("DATOS_USITC_BASE"))
    fecha_max <- max(DATOS_USITC_BASE$date, na.rm = TRUE)
    
    m_num <- lubridate::month(fecha_max)
    m_abbr <- meses_nombre_full[m_num]
    
    list(
      fecha       = fecha_max,
      year        = lubridate::year(fecha_max),
      month_abbr  = m_abbr,                       
      month_name  = meses_nombre_largo[[m_abbr]]   
    )
  })
  
  #Inicializador de controles (defaults)
  observe({
    req(exists("DATOS_USITC_BASE"))
    
    paises_unicos <- sort(unique(DATOS_USITC_BASE$country))
    anios_unicos  <- sort(unique(DATOS_USITC_BASE$year), decreasing = TRUE)
    
    pais_default <- if ("México" %in% paises_unicos) "México" else paises_unicos[1]
    
    ult_info <- info_ultima_fecha()
    
    updateSelectInput(session, "filter_country", choices = paises_unicos, selected = pais_default)
    updateSelectInput(session, "tb_countries", choices = paises_unicos, selected = pais_default)
    updateSelectInput(session, "filter_year", choices = anios_unicos, selected = ult_info$year)
  })
  
  observeEvent(input$reset_hs_filter, {
    updateTextInput(session, "hts_search_input", value = "")
  })
  
  observeEvent(input$filter_country, {
    updateSelectInput(session, "tb_countries", selected = input$filter_country)
  })
  
  output$titulo_pais <- renderText({
    req(input$filter_country)
    paste("Reporte: Importaciones de Estados Unidos provenientes de ", input$filter_country)
  })
  
  # DATASET GENERAL (Contiene TODOS los meses para las gráficas)
  datos_filtrados <- reactive({
    req(input$filter_country, input$filter_year, input$hs_level_select)
    
    df <- DATOS_USITC_BASE %>% 
      filter(country == input$filter_country, year %in% as.numeric(input$filter_year))
    
    if (input$hs_level_select == "2") {
      df <- df %>% mutate(hs_code = hts2, description = desc_hs2)
    } else if (input$hs_level_select == "4") {
      df <- df %>% mutate(hs_code = hts4, description = desc_hs4)
    } else if (input$hs_level_select == "6") {
      df <- df %>% mutate(hs_code = hts6, description = desc_hs6)
    } else {
      df <- df %>% mutate(hs_code = hts8, description = desc_hs8)
    }
    
    #if (nzchar(input$hts_search_input)) {
    # q <- tolower(trimws(input$hts_search_input))
    #df <- df %>% filter(str_detect(tolower(hs_code), q) | str_detect(tolower(description), q))
    #}
    
    ##Corrección del filter: str_starts y quité el match en la descripción
    if (!is.null(input$hts_search_input) && nzchar(trimws(input$hts_search_input))) {
      q <- trimws(input$hts_search_input)
      df <- df %>% filter(str_starts(as.character(hs_code), fixed(q)))
    }
    
    return(df)
  })
  
  #DATASET ESPECÍFICO PARA KPIS (Filtra a 1 mes SOLO si la frecuencia es "monthly")
  datos_kpi <- reactive({
    df <- datos_filtrados()
    if (input$filter_timeframe == "monthly") {
      ult_month <- info_ultima_fecha()$month_abbr
      df <- df %>% filter(as.character(month) == ult_month)
    }
    return(df)
  })
  
####### AQUÍ ESTOY PROBANDO
  datos_kpi_tasas <- reactive({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    if (input$filter_timeframe == "monthly") {
      max_date <- max(df$date, na.rm = TRUE)
      df <- df %>% filter(date == max_date)
    }
    return(df)
  })
  
  #DATASET BALANZA GENERAL (Para gráficas)
  datos_balanza <- reactive({
    req(input$tb_countries, input$filter_year)
    
    hs_len <- as.numeric(input$hs_level_select)
    
    df <- DATOS_USITC_BASE %>% 
      filter(country %in% input$tb_countries, year %in% as.numeric(input$filter_year)) %>% 
      mutate(hs_code = str_sub(as.character(hts_number), 1, hs_len))
    
    if (nzchar(input$hts_search_input)) {
      q <- tolower(trimws(input$hts_search_input))
      df <- df %>% filter(str_detect(tolower(hs_code), q))
    }
    
    return(df)
  })
  
  #DATASET BALANZA PARA KPIS
  datos_balanza_kpi <- reactive({
    df <- datos_balanza()
    if (input$filter_timeframe == "monthly") {
      ult_month <- info_ultima_fecha()$month_abbr
      df <- df %>% filter(as.character(month) == ult_month)
    }
    return(df)
  })
  
  ### NOTA FUENTE DE DATOS
  output$nota_fuente <- renderUI({
    ult_info <- info_ultima_fecha()
    
    tags$div(
      style = "padding: 10px; font-size: 12px; color: #e1e1e1; border-top: 1px solid #e3e3e3; margin-top: 15px;",
      tags$strong("Fuente: "), "USITC - United States International Trade Commission", tags$br(),
      tags$strong("Últimos datos publicados: "), paste(ult_info$month_name, ult_info$year)
    )
  })
  
  output$title_tmec_chart <- renderUI({
    if (input$filter_timeframe == "annual") "Importaciones Anuales Totales por Programa Comercial" 
    else "Importaciones Mensuales por Programa Comercial"
  })
  
  output$title_trade_balance_chart <- renderUI({
    if (input$filter_timeframe == "annual") "Análisis de series de indicadores de comercio" 
    else "Evolución Mensual de indicadores de comercio seleccionados"
  })
  
  ### Esta versión es la misma lógica que para el KPI de variación anual
  ### identificamos el mes máximo pero del año seleccionado
  get_period_text_kpi <- function() {
    req(input$filter_year) 
    
    if (input$filter_timeframe == "monthly") {
      df_f <- datos_filtrados()
      if (nrow(df_f) > 0) {
        max_dt <- max(df_f$date, na.rm = TRUE)
        m_num <- lubridate::month(max_dt)
        m_yr  <- lubridate::year(max_dt)
        m_abbr <- meses_nombre_full[m_num]
        return(paste0(m_abbr, " ", m_yr))
      } else {
        return("")
      }
    } else {
      yrs <- sort(as.numeric(input$filter_year))
      if (length(yrs) == 1) {
        años_txt <- as.character(yrs)
      } else {
        grupos <- cumsum(c(1, diff(yrs) != 1))
        rangos <- unname(tapply(yrs, grupos, function(g) {
          if (length(g) == 1) as.character(g) else paste0(min(g), "-", max(g))
        }))
        años_txt <- paste(rangos, collapse = ", ")
      }
      return(años_txt)
    }
  }
  # Tarjetas KPIs 
  output$kpi_customs_value <- renderValueBox({
    periodo_txt <- get_period_text_kpi()
    lbl <- paste0("Importaciones USD (", periodo_txt, ")")
    total_val <- sum(datos_kpi_tasas()$customs_value, na.rm = TRUE)
    valueBox(
      if (abs(total_val) > 1e12) {dollar(total_val, scale = 1e-12, suffix = " B", accuracy = 0.01)} 
      else {dollar(total_val, scale = 1e-6, suffix = " M", accuracy = 0.1)}, 
      lbl, icon = icon("ship"), color = "blue")
  })
  
  output$kpi_effective_rate <- renderValueBox({
    df <- datos_kpi_tasas()
    duties <- sum(df$calculated_duties, na.rm = TRUE)
    dutiable <- sum(df$dutiable_value, na.rm = TRUE)
    rate <- if_else(dutiable > 0, duties / dutiable, 0)
    periodo_txt <- get_period_text_kpi()
    
    lbl <- HTML(paste0("Tasa Efectiva (", periodo_txt, ")<br><small style='font-size: 0.8em; opacity: 0.85;'>Derechos / Valor Gravable</small>"))
    valueBox(percent(rate, accuracy = 0.1), lbl, icon = icon("percent"), color = "blue")
  })
  
  output$kpi_implicit_rate <- renderValueBox({
    df <- datos_kpi_tasas()
    duties <- sum(df$calculated_duties, na.rm = TRUE)
    customs <- sum(df$customs_value, na.rm = TRUE)
    rate <- if_else(customs > 0, duties / customs, 0)
    periodo_txt <- get_period_text_kpi()
    
    lbl <- HTML(paste0("Tasa Implícita (", periodo_txt, ")<br><small style='font-size: 0.8em; opacity: 0.85;'>Derechos / Valor Total</small>"))
    valueBox(percent(rate, accuracy = 0.1), lbl, icon = icon("percent"), color = "blue")
  })
  
  output$kpi_dutiable_pct <- renderValueBox({
    df <- datos_kpi_tasas()
    dutiable <- sum(df$dutiable_value, na.rm = TRUE)
    customs <- sum(df$customs_value, na.rm = TRUE)
    pct <- if_else(customs > 0, dutiable / customs, 0)
    periodo_txt <- get_period_text_kpi()
    
    lbl <- HTML(paste0("Base Gravable (", periodo_txt, ")<br><small style='font-size: 0.8em; opacity: 0.85;'>Valor Gravable / Valor Total</small>"))
    valueBox(percent(pct, accuracy = 0.1), lbl, icon = icon("chart-pie"), color = "blue")
  })
  
  output$kpi_variacion <- renderValueBox({
    req(exists("DATOS_USITC_BASE"), input$filter_country, input$filter_year)
    
    # Obtenemos el año de análisis (tomamos el más reciente si hay varios)
    year_actual <- as.numeric(max(input$filter_year))
    year_previo <- year_actual - 1
    
    df_base <- DATOS_USITC_BASE %>%
      filter(country == input$filter_country)
    
    if (!is.null(input$hts_search_input) && nzchar(trimws(input$hts_search_input))) {
      q <- trimws(input$hts_search_input)
      df_base <- df_base %>% filter(str_starts(as.character(hts_number), fixed(q)))
    }
    
    # determinamos el útlimo mes del año seleccionado (NO DEL AÑO MÁXIMO DEL DF)
    df_year_actual <- df_base %>% filter(year == year_actual)
    
    # Si para ese año no hay datos, evitamos que rompa el código
    req(nrow(df_year_actual) > 0)
    
    fecha_max_year <- max(df_year_actual$date, na.rm = TRUE)
    m_num_max      <- lubridate::month(fecha_max_year)
    m_abbr         <- meses_nombre_full[m_num_max]
    
    if (input$filter_timeframe == "monthly") {
      # Comparación mensual del último mes disponible de ese año vs mismo mes del año previo
      v_act <- df_base %>% 
        filter(year == year_actual, lubridate::month(date) == m_num_max) %>% 
        summarise(t = sum(customs_value, na.rm = TRUE)) %>% 
        pull(t)
      
      v_ant <- df_base %>% 
        filter(year == year_previo, lubridate::month(date) == m_num_max) %>% 
        summarise(t = sum(customs_value, na.rm = TRUE)) %>% 
        pull(t)
      
      lbl <- paste0("Var. Interanual (", m_abbr, " ", year_actual, " vs ", m_abbr, " ", year_previo, ")")
      
    } else {
      # Comparación Anual, si es un año parcial compara ytd, si es completo compara año completo
      v_act <- df_base %>% 
        filter(year == year_actual, lubridate::month(date) <= m_num_max) %>% 
        summarise(t = sum(customs_value, na.rm = TRUE)) %>% 
        pull(t)
      
      v_ant <- df_base %>% 
        filter(year == year_previo, lubridate::month(date) <= m_num_max) %>% 
        summarise(t = sum(customs_value, na.rm = TRUE)) %>% 
        pull(t)
      
      # Truquito para poner etiqueta anual si es un año completo, menos de 12 ~ ytd
      if (m_num_max == 12) {
        lbl <- paste0("Var. Anual (", year_actual, " vs ", year_previo, ")")
      } else {
        lbl <- paste0("Var. YTD (Ene-", m_abbr, " ", year_actual, " vs ", year_previo, ")")
      }
    }
    
    # Calculamos la variación en %
    var_pct <- if (!is.null(v_ant) && !is.na(v_ant) && v_ant > 0) {
      ((v_act - v_ant) / v_ant) * 100
    } else {
      0
    }
    
    signo   <- if (var_pct > 0) "+" else ""
    c_color <- if (var_pct >= 0) "green" else "red"
    i_icon  <- if (var_pct >= 0) icon("arrow-trend-up") else icon("arrow-trend-down")
    
    valueBox(
      value = paste0(signo, format(round(var_pct, 1), nsmall = 1), "%"),
      subtitle = lbl,
      icon = i_icon,
      color = c_color
    )
  })
  
  #GRÁFICA BARRAS PROGRAMAS 
  plot_tmec_bar_chart_rx <- reactive({
    df_raw <- datos_filtrados()
    req(nrow(df_raw) > 0)
    
    pais_txt <- paste(input$filter_country, collapse = ", ")
    yrs <- sort(as.numeric(input$filter_year))
    # Incrementa un grupo cada vez que la diferencia entre años es mayor a 1
    grupos <- cumsum(c(1, diff(yrs) != 1))
    rangos <- tapply(yrs, grupos, function(g) {
      if (length(g) == 1) {
        as.character(g)
      } else {
        paste0(min(g), "-", max(g))
      }
    })
    años_txt <- paste(rangos, collapse = ", ")
    subtitulo_hts <- if (!is.null(input$hts_search_input) && nchar(trimws(input$hts_search_input)) > 0) {
      paste0("Partida: ", input$hts_search_input)
    } else {
      "Comercio Total"
    }
    
    if (input$filter_timeframe == "annual") {
      df <- df_raw %>% 
        group_by(year, program_shrt, program) %>% 
        summarise(
          customs   = sum(customs_value, na.rm = TRUE),
          dutiable  = sum(dutiable_value, na.rm = TRUE),
          duties    = sum(calculated_duties, na.rm = TRUE),
          .groups   = "drop"
        ) %>% 
        # Calculamos el total por año para obtener la proporción de cada programa
        group_by(year) %>% 
        mutate(
          total_year_customs = sum(customs, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        mutate(
          year_lbl       = factor(as.character(year)),
          value          = customs / 1e6,
          val_form       = formatC(value, format = "f", digits = 1, big.mark = ","),
          pct_programa   = if_else(total_year_customs > 0, customs / total_year_customs, 0),
          tasa_efectiva  = if_else(dutiable > 0, duties / dutiable, 0),
          tasa_implicita = if_else(customs > 0, duties / customs, 0),
          pct_gravable   = if_else(customs > 0, dutiable / customs, 0),
          hover_label    = paste0(
            "<b>", program, " (", year, ")</b>\n",
            "Valor: $", val_form, " M USD\n",
            "Proporción: ", percent(pct_programa, accuracy = 0.1), "\n",
            "Tasa Efectiva: ", percent(tasa_efectiva, accuracy = 0.1), "\n",
            "Tasa Implícita: ", percent(tasa_implicita, accuracy = 0.1), "\n",
            "Valor Gravable: ", percent(pct_gravable, accuracy = 0.1)
          )
        )
      
      titulo_txt <- paste0(
        "<b>Importaciones anuales de EE. UU. provenientes de ", pais_txt, " en ", años_txt, " por programa</b><br>",
                           "<span style='font-size:15px; color:#666666; font-family:Roboto;'>", subtitulo_hts, "</span>")
      
      p <- ggplot() +
        geom_col(
          data = df, 
          aes(x = year_lbl, y = value, fill = program_shrt, label = hover_label), 
          position = "stack", 
          width = 0.5
        ) +
        scale_fill_manual(values = paleta_programas, limits = force) +
        scale_y_continuous(labels = label_dollar(), n.breaks = 6) +
        labs(x = NULL, y = "Millones de USD", fill = "") +
        theme_minimal() +
        theme(
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top",
          axis.title.y = element_text(family = "Roboto-medium", color = "gray35", size = 14)
        )
      
      angulo_eje_x <- 0
      
    } else {
      df <- df_raw %>% 
        group_by(year, month, program_shrt, program) %>% 
        summarise(
          customs   = sum(customs_value, na.rm = TRUE),
          dutiable  = sum(dutiable_value, na.rm = TRUE),
          duties    = sum(calculated_duties, na.rm = TRUE),
          .groups   = "drop"
        ) %>% 
        group_by(year, month) %>% 
        mutate(
          total_month_customs = sum(customs, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        mutate(
          month_num  = match(as.character(month), meses_nombre_full),
          year_month = sprintf("%d-%02d", year, month_num),
          period_lbl = paste0(month, " ", str_sub(as.character(year), 3, 4))
        ) %>% 
        arrange(year_month) %>% 
        mutate(
          month_lbl      = factor(period_lbl, levels = unique(period_lbl)),
          value          = customs / 1e6,
          val_form       = formatC(value, format = "f", digits = 1, big.mark = ","),
          pct_programa   = if_else(total_month_customs > 0, customs / total_month_customs, 0),
          tasa_efectiva  = if_else(dutiable > 0, duties / dutiable, 0),
          tasa_implicita = if_else(customs > 0, duties / customs, 0),
          pct_gravable   = if_else(customs > 0, dutiable / customs, 0),
          hover_label    = paste0(
            "<b>", program, " (", period_lbl, ")</b>\n",
            "Valor: $", val_form, " M USD\n",
            "Proporción: ", percent(pct_programa, accuracy = 0.1), "\n",
            "Tasa Efectiva: ", percent(tasa_efectiva, accuracy = 0.1), "\n",
            "Tasa Implícita: ", percent(tasa_implicita, accuracy = 0.1), "\n",
            "Valor Gravable: ", percent(pct_gravable, accuracy = 0.1)
          )
        )
      
      total_periodos <- n_distinct(df$period_lbl)
      angulo_eje_x   <- if_else(total_periodos > 12, -90, 0)
      
      # Creamos una línea negra para total mensual consolidado por periodo
      df_totals <- df %>% 
        group_by(month_lbl, period_lbl) %>% 
        summarise(
          customs  = sum(customs),
          dutiable = sum(dutiable),
          duties   = sum(duties),
          .groups  = "drop"
        ) %>% 
        mutate(
          x_num          = as.numeric(month_lbl),
          total_val      = customs / 1e6,
          val_form       = formatC(total_val, format = "f", digits = 1, big.mark = ","),
          tasa_efectiva  = if_else(dutiable > 0, duties / dutiable, 0),
          tasa_implicita = if_else(customs > 0, duties / customs, 0),
          pct_gravable   = if_else(customs > 0, dutiable / customs, 0),
          hover_total    = paste0(
            "<b>Total (", period_lbl, ")</b>\n",
            "Valor: $", val_form, " M USD\n",
            "Tasa Efectiva: ", percent(tasa_efectiva, accuracy = 0.1), "\n",
            "Tasa Implícita: ", percent(tasa_implicita, accuracy = 0.1), "\n",
            "Valor Gravable: ", percent(pct_gravable, accuracy = 0.1)
          )
        )
      
      titulo_txt <- paste0(
        "<b>Importaciones mensuales de EE. UU. provenientes de ", pais_txt, " en ", años_txt, " por programa</b><br>",
                           "<span style='font-size:14px; color:#666666; font-family:Roboto-medium;'>", subtitulo_hts, "</span>")
      
      p <- ggplot() +
        geom_col(
          data = df, 
          aes(x = month_lbl, y = value, fill = program_shrt, label = hover_label), 
          position = position_dodge(width = 0.8, preserve = "single"), 
          width = 0.7
        ) +
        geom_segment(
          data = df_totals, 
          aes(
            x = x_num - 0.2, 
            xend = x_num + 0.2, 
            y = total_val, 
            yend = total_val, 
            color = "Total",
            label = hover_total
          ), 
          linewidth = 1.2,
          inherit.aes = FALSE
        ) +
        scale_fill_manual(values = paleta_programas, limits = force) +
        scale_color_manual(values = c("Total" = "black")) +
        scale_y_continuous(labels = label_dollar(), n.breaks = 6) +
        labs(x = NULL, y = "Millones de USD", fill = "", color = "") +
        theme_minimal() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "top",
          axis.title.y = element_text(family = "Roboto-medium", color = "gray45", size = 14)
        )
    }
    
    p_plotly <- ggplotly(p, tooltip = "label")
    
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$text)) {
        p_plotly$x$data[[i]]$text <- gsub("^[a-zA-Z_]+:\\s*", "", p_plotly$x$data[[i]]$text)
      }
      if (!is.null(p_plotly$x$data[[i]]$name)){
        p_plotly$x$data[[i]]$name <- gsub("^\\((.*),1\\)$", "\\1", p_plotly$x$data[[i]]$name)
      }
    }
    
    p_plotly <- p_plotly %>% 
      layout(
        margin = list(t = 90, l = 70, r = 15, b = 70),
        title = list(
          text = titulo_txt,
          x = 0.02,
          xanchor = "left",
          xref = "container",
          y = 0.95,
          yanchor = "top",
          font = list(size = 18, color = "#333333", family = "Roboto")
        ),
        legend = list(
          orientation = "h",
          x = 0,
          xanchor = "left",
          xref = "container",
          y = 0.88,
          yanchor = "top",
          yref = "container",
          font = list(size = 12, family = "Roboto")
        ),
        xaxis = list(
          tickangle = angulo_eje_x,
          automargin = T,
          tickfont = list(family = "Roboto", color = "gray45", size = 14)
        ),
        yaxis = list(
          title = "Millones de USD",
          automargin = TRUE,
          tickfont = list(family = "Roboto", color = "gray45", size = 14),
          titlefont = list(family = "Roboto", color = "gray45", size = 14)
        ),
        annotations = list(
          list(
            text      = "Fuente: CEPCAN con datos de USITC.",
            x         = 0,
            y         = 0,               
            yshift    = -54,
            xshift    = -45,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            xanchor   = "left",
            yanchor   = "top",
            font      = list(size = 14, color = "#666666", family = "Roboto-medium")
          )
        ),
        meta = list(
          custom_fonts = htmltools::tags$head(
            htmltools::tags$link(
              rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
            )
          )
        )
      )
    
    return(p_plotly)
  })
  
  output$plot_tmec_bar_chart <- renderPlotly({
    plot_tmec_bar_chart_rx()
  })
  
  output$download_tmec_plot_hd <- downloadHandler(
    filename = function() {
      paste0("serie_programa_", Sys.Date(), ".png")
    },
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(
        widget = plot_tmec_bar_chart_rx(), 
        file = temp_html, 
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1600,   
        vheight = 800,  
        zoom = 2        
      )
    } 
  )
# Gráfica de dona para proporción por programas
  plot_program_donut_rx <- reactive({
    df_raw <- datos_filtrados()
    req(nrow(df_raw) > 0)
    
    pais_txt <- paste(input$filter_country, collapse = ", ")
    yrs <- sort(as.numeric(input$filter_year))
    grupos <- cumsum(c(1, diff(yrs) != 1))
    rangos <- tapply(yrs, grupos, function(g) {
      if (length(g) == 1) {
        as.character(g)
      } else {
        paste0(min(g), "-", max(g))
      }
    })
    años_txt <- paste(rangos, collapse = ", ")
    subtitulo_hts <- if (!is.null(input$hts_search_input) && nchar(trimws(input$hts_search_input)) > 0) {
      paste0("Partida: ", input$hts_search_input)
    } else {
      "Comercio Total"
    }
    
    df <- df_raw %>% 
      group_by(program_shrt, program) %>% 
      summarise(value = sum(customs_value, na.rm = TRUE) / 1e6, .groups = "drop") %>%
      mutate(
        val_form = formatC(value, format = "f", digits = 1, big.mark = ","),
        hover_label = paste0(program, "\nValor: $", val_form, " M USD")
      )
    
    colores_ordenados <- unname(paleta_programas[as.character(df$program_shrt)])
    
    plot_ly(
      df, 
      labels = ~program_shrt, 
      values = ~value, 
      type = "pie", 
      hole = 0.5, 
      text = ~hover_label,
      hoverinfo = "text", 
      marker = list(colors = colores_ordenados), 
      textposition = "inside", 
      textinfo = "percent",
      insidetextfont = list(
        family = "Roboto",
        size   = 16)
    ) %>% 
      layout(
        title = list(
          text = paste0("<b>Proporción de importaciones por programa</b><br><span style='font-size: 14px; color: #666; font-family: Roboto-medium;'>", pais_txt, " (", años_txt, ") | ", subtitulo_hts, "</span>"), 
          x = 0.02, 
          y = 0.96,
          font = list(size = 18, family = "Roboto")
        ),
        annotations = list(
          list(
            text      = "Fuente: CEPCAN con datos de USITC.",
            x         = 0.02,
            y         = 0.02,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            xanchor   = "left",
            yanchor   = "top",
            font      = list(size = 14, color = "#666666", family = "Roboto-medium")
          )
        ),
        showlegend = TRUE,
        margin = list(l = 10, r = 10, t = 50, b = 10),
        meta = list(
          custom_fonts = htmltools::tags$head(
            htmltools::tags$link(
              rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
            )
          )
        )
      )
  })
  
  output$plot_program_donut <- renderPlotly({
    plot_program_donut_rx()
  })
  
  output$download_program_donut_hd <- downloadHandler(
    filename = function() {
      paste0("distribución_programa_", Sys.Date(), ".png")
    },
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(
        widget = plot_program_donut_rx(), 
        file = temp_html, 
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1600,   
        vheight = 800,  
        zoom = 2        
      )
    } 
  )

### Sección Top productos
  # Principales productos importados por EE.UU.
  plot_top_products_rx <- reactive({
    df_raw <- datos_filtrados()
    req(nrow(df_raw) > 0)
    
    pais_txt <- paste(input$filter_country, collapse = ", ")
    yrs <- sort(as.numeric(input$filter_year))
    grupos <- cumsum(c(1, diff(yrs) != 1))
    rangos <- tapply(yrs, grupos, function(g) {
      if (length(g) == 1) {
        as.character(g)
      } else {
        paste0(min(g), "-", max(g))
      }
    })
    años_txt <- paste(rangos, collapse = ", ")
    subtitulo_hts <- if (!is.null(input$hts_search_input) && nchar(trimws(input$hts_search_input)) > 0) {
      paste0("Partida: ", input$hts_search_input)
    } else {
      "Comercio Total"
    }
    
    # Obtenemos los códigos del Top 10 de productos principales
    top_10_codes <- df_raw %>% 
      group_by(hs_code) %>% 
      summarise(total_gen = sum(customs_value, na.rm = TRUE), .groups = "drop") %>% 
      slice_max(order_by = total_gen, n = 10) %>% 
      pull(hs_code)
    
    # Filtramos el df_raw para el Top 10 y agrupamos por producto y programa
    df <- df_raw %>% 
      filter(hs_code %in% top_10_codes) %>% 
      group_by(hs_code, description, program_shrt, program) %>% 
      summarise(
        customs   = sum(customs_value, na.rm = TRUE),
        dutiable  = sum(dutiable_value, na.rm = TRUE),
        duties    = sum(calculated_duties, na.rm = TRUE),
        .groups   = "drop"
      ) %>% 
      group_by(hs_code) %>% 
      mutate(total_hs_customs = sum(customs, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(
        value        = customs / 1e6,
        val_form     = formatC(value, format = "f", digits = 1, big.mark = ","),
        pct_programa = if_else(total_hs_customs > 0, customs / total_hs_customs, 0),
        hover_label  = paste0(
          "<b>", description, "</b>\n",
          "Programa: ", program, "\n",
          "Valor: $", val_form, " M USD\n",
          "Proporción: ", percent(pct_programa, accuracy = 0.1), "\n"
        )
      )
    
    # Reordenamos los códigos HS de menor a mayor valor total acumulado para el eje Y
    df <- df %>% 
      mutate(hs_code = reorder(hs_code, value, FUN = sum))
    
    titulo_txt <- paste0("<b>Principales productos importados por EE. UU. de ", pais_txt, " en ", años_txt, " por nivel HS y programa (anual)</b><br>",
                         "<span style='font-size:15px; color:#666666; font-family:Roboto-bold;'>", subtitulo_hts, "</span>")
    
    # Gráfica en barras apiladas para desagregar por programa de importación
    p <- ggplot(df, aes(x = value, y = hs_code)) +
      geom_col(
        aes(fill = program_shrt, label = hover_label), 
        position = "stack", 
        width = 0.7
      ) +
      scale_fill_manual(values = paleta_programas, limits = force) +
      scale_x_continuous(labels = label_dollar(), n.breaks = 7, expand = c(0.01,0)) +
      labs(x = "Millones de USD", y = paste("Códigos HS -", input$hs_level_select), fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(family = "Roboto-medium", color = "gray35", size = 14),
        axis.title.y = element_text(family = "Roboto-medium", color = "gray35", size = 12)
      )
    
    p_plotly <- ggplotly(p, tooltip = "label")
    
    # Limpiamos las etiquetas en el tooltip de Plotly
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$text)) {
        p_plotly$x$data[[i]]$text <- gsub("^[a-zA-Z_]+:\\s*", "", p_plotly$x$data[[i]]$text)
      }
    }
    
    p_plotly <- p_plotly %>% 
      layout(
        margin = list(t = 90, l = 70, r = 15, b = 60),
        title = list(
          text = titulo_txt,
          x = 0.02,
          xanchor = "left",
          xref = "container",
          y = 0.95,
          yanchor = "top",
          font = list(size = 18, color = "#333333", family = "Roboto")
        ),
        legend = list(
          orientation = "h",
          x = 0,
          xanchor = "left",
          xref = "paper",
          y = 0.89,
          yanchor = "top",
          yref = "container",
          font = list(size = 14, family = "Roboto")
        ),
        xaxis = list(
          title = "Millones de USD",
          automargin = TRUE,
          rangemode = "tozero",
          tickfont = list(family = "Roboto", color = "gray45", size = 14),
          titlefont = list(family = "Roboto-medium", color = "gray45", size = 14)
        ),
        yaxis = list(
          automargin = TRUE,
          standoff = 25, 
          tickfont = list(family = "Roboto", color = "gray45", size = 14),
          titlefont = list(family = "Roboto-medium", color = "gray35", size = 12, margin = margin(r = 10))
        ),
        annotations = list(
          list(
            text      = "Fuente: CEPCAN con datos de USITC.",
            x         = 0,
            y         = 0,               
            yshift    = -40,
            xshift    = -50,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            xanchor   = "left",
            yanchor   = "top",
            font      = list(size = 14, color = "#666666", family = "Roboto-medium")
          )
        ),
        meta = list(
          custom_fonts = htmltools::tags$head(
            htmltools::tags$link(
              rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
            )
          )
        )
      )
    
    return(p_plotly)
  })
  
  output$plot_top_products <- renderPlotly({
    plot_top_products_rx()
  })
  
  output$download_top_products_hd <- downloadHandler(
    filename = function() {
      paste0("principales_productos_", Sys.Date(), ".png")
    },
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(
        widget = plot_top_products_rx(), 
        file = temp_html, 
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1600,   
        vheight = 800,  
        zoom = 2        
      )
    } 
  )
  
  ### Top países: 
  plot_top_countries_rx <- reactive({
    req(exists("DATOS_USITC_BASE"), exists("DATOS_USITC_TOTALES"), input$filter_year, input$hs_level_select)
    
    df_raw <- DATOS_USITC_BASE %>% 
      filter(year %in% as.numeric(input$filter_year))
    
    df_world_raw <- DATOS_USITC_TOTALES %>% 
      filter(year %in% as.numeric(input$filter_year))
    
    if (input$hs_level_select == "2") {
      df_raw <- df_raw %>% mutate(hs_code = hts2)
      df_world_raw <- df_world_raw %>% mutate(hs_code = hts2)
    } else if (input$hs_level_select == "4") {
      df_raw <- df_raw %>% mutate(hs_code = hts4)
      df_world_raw <- df_world_raw %>% mutate(hs_code = hts4)
    } else if (input$hs_level_select == "6") {
      df_raw <- df_raw %>% mutate(hs_code = hts6)
      df_world_raw <- df_world_raw %>% mutate(hs_code = hts6)
    } else {
      df_raw <- df_raw %>% mutate(hs_code = hts8)
      df_world_raw <- df_world_raw %>% mutate(hs_code = hts8)
    }
    
    if (!is.null(input$hts_search_input) && nzchar(trimws(input$hts_search_input))) {
      q <- trimws(input$hts_search_input)
      df_raw <- df_raw %>% filter(str_starts(as.character(hs_code), fixed(q)))
      df_world_raw <- df_world_raw %>% filter(str_starts(as.character(hs_code), fixed(q)))
    }
    
    req(nrow(df_raw) > 0)
    
    yrs <- sort(as.numeric(input$filter_year))
    grupos <- cumsum(c(1, diff(yrs) != 1))
    rangos <- tapply(yrs, grupos, function(g) {
      if (length(g) == 1) {
        as.character(g)
      } else {
        paste0(min(g), "-", max(g))
      }
    })
    años_txt <- paste(rangos, collapse = ", ")
    
    df_top10 <- df_raw %>% 
      group_by(country) %>% 
      summarise(customs = sum(customs_value, na.rm = TRUE) / 1e6, .groups = "drop") %>% 
      slice_max(order_by = customs, n = 12)
    
    total_acumulado <- sum(df_world_raw$customs_value, na.rm = TRUE) / 1e6
    total_txt <- formatC(total_acumulado, format = "f", digits = 1, big.mark = ",")
    
    suma_top10 <- sum(df_top10$customs, na.rm = TRUE)
    resto_del_mundo_usd <- max(0, total_acumulado - suma_top10)
    
    df_tree <- bind_rows(
      df_top10,
      tibble(country = "Resto del mundo", customs = resto_del_mundo_usd)
    ) %>% 
      mutate(
        total_global = sum(customs),
        pct_share    = (customs / total_acumulado) * 100, 
        parents      = "",
        hover_text   = paste0(
          "<b>", country, "</b><br>",
          "Valor Importaciones: $", formatC(customs, format = "f", digits = 1, big.mark = ","), " M USD<br>",
          "Participación: ", formatC(pct_share, format = "f", digits = 1), "%"
        )
      )
    
    if (!is.null(input$hts_search_input) && nzchar(trimws(input$hts_search_input))) {
      producto_txt <- paste0("de la partida ", trimws(input$hts_search_input), " ")
    } else {
      producto_txt <- "totales " 
    }
    
    titulo_txt <- paste0("<b>Principales países de origen de las importaciones de EE. UU. ", producto_txt, "en ", años_txt, "</b>")
    
    plot_ly(
      data = df_tree,
      type = "treemap",
      labels = ~country,
      parents = ~parents,
      values = ~customs,
      customdata = ~pct_share,
      texttemplate = "<b>%{label}</b><br>$%{value:,.1f} M USD<br>(%{customdata:.1f}%)",
      hoverinfo = "text",
      text = ~hover_text,
      outsidetextfont = list(family = "Roboto"),
      insidetextfont = list(family = "Roboto-medium", size = 14),
      marker = list(colors = ~customs,
                    colorscale = "Portland",
                    showscale = FALSE)
    ) %>% 
      layout(
          title = list(
            text = paste0(
              titulo_txt, "<br>",
              "<span style='font-size: 16px; color: #333333; font-family: Roboto;'><b>Valor Total de Importaciones: $", total_txt, " M USD</b></span><br>",
              "<span style='color: #666666; font-size: 14px; font-family: Roboto;'>",
              '<b>Nota: Los países mostrados son los principales dentro de la muestra (12 países). "Resto del mundo" ajusta al total real, por lo que los porcentajes de participación son exactos.</b>',
              "</span>"
            ),
          x = 0.01, xanchor = "left", xref = "container",
          y = 0.97, yanchor = "top", 
          font = list(size = 18, family = "Roboto-medium")
        ),
        margin = list(t = 90, l = 10, r = 10, b = 60),
        annotations = list(
          list(
            text      = "Fuente: CEPCAN con datos de USITC.",
            x         = 0,
            y         = 0,               
            yshift    = -15,
            xshift    = 0,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            xanchor   = "left",
            yanchor   = "top",
            font      = list(size = 14, color = "#666666", family = "Roboto-medium")
          )
        ),
        meta = list(
          custom_fonts = htmltools::tags$head(
            htmltools::tags$link(
              rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
            )
          )
        )
      )
  })
  
  output$plot_top_countries <- renderPlotly({
    plot_top_countries_rx()
  })
  
  output$download_top_countries_hd <- downloadHandler(
    filename = function() {
      paste0("principales_países_", Sys.Date(), ".png")
    },
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(
        widget = plot_top_countries_rx(), 
        file = temp_html, 
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1600,   
        vheight = 800,  
        zoom = 2        
      )
    } 
  )
  
  #### Nueva sección: Laboratorio de Datos (Antes BALANCE COMERCIAL EE. UU.) 
  # Laboratorio de datos
  plot_trade_balance_rx <- reactive({
    df <- datos_balanza()
    req(nrow(df) > 0)
    req(input$tb_metrics)
    
    indicadores_sel <- input$tb_metrics
    paises_sel      <- unique(df$country)
    yrs             <- sort(as.numeric(input$filter_year))
    
    # 1. RANGOS DE AÑOS Y SUBTÍTULO
    grupos <- cumsum(c(1, diff(yrs) != 1))
    rangos <- tapply(yrs, grupos, function(g) {
      if (length(g) == 1) as.character(g) else paste0(min(g), "-", max(g))
    })
    años_txt <- paste(rangos, collapse = ", ")
    
    subtitulo_hts <- if (!is.null(input$hts_search_input) && nchar(trimws(input$hts_search_input)) > 0) {
      paste0("Partida: ", input$hts_search_input)
    } else {
      "Comercio Total"
    }
    
    # Mapa de Metadatos
    dict_meta <- list(
      "customs_value"     = list(label = "Importaciones", unit = "Millones de USD", fmt = "money", type = "money"),
      "dutiable_value"    = list(label = "Valor gravable", unit = "Millones de USD", fmt = "money", type = "money"),
      "calculated_duties" = list(label = "Aranceles pagados", unit = "Millones de USD", fmt = "money", type = "money"),
      "tasa_implicita"    = list(label = "Tasa implícita", unit = "Porcentaje (%)", fmt = "percent", type = "percent"),
      "tasa_efectiva"     = list(label = "Tasa efectiva", unit = "Porcentaje (%)", fmt = "percent", type = "percent"),
      "pct_gravable"      = list(label = "Base gravable", unit = "Porcentaje (%)", fmt = "percent", type = "percent")
    )
    
    tipos_sel <- sapply(indicadores_sel, function(x) dict_meta[[x]]$type)
    es_mixto  <- length(indicadores_sel) == 2 && length(unique(tipos_sel)) == 2
    
    # Paleta de Colores por País
    colores_base  <- c("#1E5A9C", "#D4AF37", "#b64bad", "#e07a5f")
    paleta_paises <- setNames(rep(NA, length(paises_sel)), paises_sel)
    idx_otros     <- 1
    
    for (p in paises_sel) {
      if (tolower(p) %in% c("mexico", "méxico")) {
        paleta_paises[p] <- "darkgreen" 
      } else if (tolower(p) %in% c("china")) {
        paleta_paises[p] <- "#B02418"
      } else {
        paleta_paises[p] <- colores_base[(idx_otros - 1) %% length(colores_base) + 1]
        idx_otros <- idx_otros + 1
      }
    }
    
    # 2. PROCESAMIENTO GENERAL DE DATOS
    if (input$filter_timeframe == "annual") {
      df_calc <- df %>% 
        group_by(year, country) %>% 
        summarise(
          customs_val  = sum(customs_value, na.rm = TRUE),
          dutiable_val = sum(dutiable_value, na.rm = TRUE),
          duties_val   = sum(calculated_duties, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        mutate(
          customs_value     = customs_val / 1e6,
          dutiable_value    = dutiable_val / 1e6,
          calculated_duties = duties_val / 1e6,
          tasa_implicita    = if_else(customs_val > 0, (duties_val / customs_val) * 100, 0),
          tasa_efectiva     = if_else(dutiable_val > 0, (duties_val / dutiable_val) * 100, 0),
          pct_gravable      = if_else(customs_val > 0, (dutiable_val / customs_val) * 100, 0),
          time_factor       = factor(year)
        )
      lbl_timeframe <- "Evolución anual"
    } else {
      df_calc <- df %>% 
        mutate(month_num = match(as.character(month), meses_nombre_full)) %>% 
        group_by(year, month_num, month, country) %>% 
        summarise(
          customs_val  = sum(customs_value, na.rm = TRUE),
          dutiable_val = sum(dutiable_value, na.rm = TRUE),
          duties_val   = sum(calculated_duties, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        arrange(year, month_num) %>% 
        mutate(
          period_lbl        = paste0(substr(as.character(month), 1, 3), " ", str_sub(as.character(year), 3, 4)),
          time_factor       = factor(period_lbl, levels = unique(period_lbl)),
          customs_value     = customs_val / 1e6,
          dutiable_value    = dutiable_val / 1e6,
          calculated_duties = duties_val / 1e6,
          tasa_implicita    = if_else(customs_val > 0, (duties_val / customs_val) * 100, 0),
          tasa_efectiva     = if_else(dutiable_val > 0, (duties_val / dutiable_val) * 100, 0),
          pct_gravable      = if_else(customs_val > 0, (dutiable_val / customs_val) * 100, 0)
        )
      lbl_timeframe <- "Evolución mensual"
    }
    
    total_periodos <- n_distinct(df_calc$time_factor)
    angulo_eje_x   <- if_else(total_periodos > 12, -90, 0)
    
    #CASO A: VISTA MIXTA ($ Y %)
    if (es_mixto) {
      ind_money   <- indicadores_sel[tipos_sel == "money"][1]
      ind_percent <- indicadores_sel[tipos_sel == "percent"][1]
      
      meta_money   <- dict_meta[[ind_money]]
      meta_percent <- dict_meta[[ind_percent]]
      
      df_plot <- df_calc %>% 
        mutate(
          val_money   = .data[[ind_money]],
          val_percent = .data[[ind_percent]],
          text_hover  = paste0(
            "<b>", country, "</b> (", time_factor, ")<br>",
            "<b>", meta_money$label, ":</b> $", scales::comma(val_money, accuracy = 0.1), " M<br>",
            "<b>", meta_percent$label, ":</b> ", scales::comma(val_percent, accuracy = 0.01), "%"
          )
        )
      
      p_money <- ggplot(df_plot, aes(x = time_factor, y = val_money, fill = country, text = text_hover)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
        scale_fill_manual(values = paleta_paises) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
        labs(x = "", y = meta_money$unit, fill = "") +
        theme_minimal() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_text(family = "Roboto", color = "gray35", margin = margin(r = 10), size = 16)
        )
      
      p_percent <- ggplot(df_plot, aes(x = time_factor, y = val_percent, color = country, group = country, text = text_hover)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = paleta_paises) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        labs(x = "", y = meta_percent$label, color = "") +
        theme_minimal()
      
      g_money   <- ggplotly(p_money, tooltip = "text")
      g_percent <- ggplotly(p_percent, tooltip = "text")
      
      for (i in seq_along(g_percent$x$data)) {
        g_percent$x$data[[i]]$yaxis <- "y2"
      }
      
      p_plotly <- g_money
      for (i in seq_along(g_percent$x$data)) {
        p_plotly$x$data[[length(p_plotly$x$data) + 1]] <- g_percent$x$data[[i]]
      }
      
      idx_bar  <- 1
      idx_line <- 1
      
      for (i in seq_along(p_plotly$x$data)) {
        if (!is.null(p_plotly$x$data[[i]]$name)) {
          p_name <- gsub("^\\((.*),.*\\)$", "\\1", p_plotly$x$data[[i]]$name)
          
          if (p_plotly$x$data[[i]]$type == "bar") {
            lbl_prefix <- if (idx_bar == 1) paste0("<b>", meta_money$label, ":</b> ") else ""
            p_plotly$x$data[[i]]$name <- paste0(lbl_prefix, p_name)
            idx_bar <- idx_bar + 1
          } else {
            lbl_prefix <- if (idx_line == 1) paste0("<b>", meta_percent$label, ":</b> ") else ""
            p_plotly$x$data[[i]]$name <- paste0(lbl_prefix, p_name)
            idx_line <- idx_line + 1
          }
        }
      }
      
      titulo_eje  <- paste0(meta_money$label, " ($) y ", meta_percent$label, " (%)")
      titulo_html <- paste0(
        "<b>", lbl_timeframe, " de ", titulo_eje, " (", años_txt, ")</b><br>",
        "<span style='font-size:15px; color:#666666; font-family:Roboto-bold;'>", subtitulo_hts, "</span>"
      )
      
      p_plotly <- p_plotly %>% 
        layout(
          margin = list(t = 70, l = 70, r = 70, b = 70), 
          title = list(
            text = titulo_html,
            y = 0.96,          
            x = 0.02,              
            xanchor = "left",
            xref = "container",
            font = list(size = 20, color = "#333333", family = "Roboto"),
            pad = list(b = 10)
          ),
          legend = list(
            orientation = "h",
            xanchor = "center",
            xref = "paper",
            x = 0.5,
            y = 0.9,          
            yanchor = "top",
            yref = "container",
            font = list(size = 14, family = "Roboto")
          ),
          xaxis = list(tickangle = angulo_eje_x, tickfont = list(family = "Roboto-medium", color = "gray35", size = 14)),
          yaxis = list(
            title = meta_money$unit,
            nticks = 5,
            automargin = TRUE, 
            tickfont = list(family = "Roboto", color = "gray35", size = 14), 
            titlefont = list(family = "Roboto-medium", color = "gray35", size = 14)
          ),
          yaxis2 = list(
            title = " ",
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            zeroline = FALSE,
            ticksuffix = "%",
            titlefont = list(family = "Roboto", color = "gray35", size = 14),
            tickfont = list(family = "Roboto", color = "gray35", size = 14)
          ),
          annotations = list(
            list(
              text = meta_percent$label,
              textangle = 90,
              x = 1.045,      
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(family = "Roboto-medium", color = "gray35", size = 14)
              ),
            list(
              text      = "Fuente: CEPCAN con datos de USITC.",
              x         = 0,
              y         = 0,               
              yshift    = -55,
              xshift    = -30,
              xref      = "paper",
              yref      = "paper",
              showarrow = FALSE,
              xanchor   = "left",
              yanchor   = "top",
              font      = list(size = 14, color = "#666666", family = "Roboto-medium")
            )
          ),
          meta = list(
            custom_fonts = htmltools::tags$head(
              htmltools::tags$link(
                rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
              )
            )
          )
        )
      
      return(p_plotly)
      
      #CASO B: INDICADORES HOMOGÉNEOS
    } else {
      if (length(indicadores_sel) == 2) {
        ind1  <- indicadores_sel[1]
        ind2  <- indicadores_sel[2]
        meta1 <- dict_meta[[ind1]]
        meta2 <- dict_meta[[ind2]]
        
        df_calc <- df_calc %>% 
          mutate(
            val1_txt = case_when(
              meta1$fmt == "money"   ~ paste0("$", scales::comma(.data[[ind1]], accuracy = 0.1), " M"),
              meta1$fmt == "percent" ~ paste0(scales::comma(.data[[ind1]], accuracy = 0.01), "%"),
              TRUE                   ~ scales::comma(.data[[ind1]], accuracy = 0.1)
            ),
            val2_txt = case_when(
              meta2$fmt == "money"   ~ paste0("$", scales::comma(.data[[ind2]], accuracy = 0.1), " M"),
              meta2$fmt == "percent" ~ paste0(scales::comma(.data[[ind2]], accuracy = 0.01), "%"),
              TRUE                   ~ scales::comma(.data[[ind2]], accuracy = 0.1)
            ),
            text_hover_shared = paste0(
              "<b>", country, "</b> (", time_factor, ")<br>",
              "<b>", meta1$label, ":</b> ", val1_txt, "<br>",
              "<b>", meta2$label, ":</b> ", val2_txt
            )
          )
      } else {
        ind1  <- indicadores_sel[1]
        meta1 <- dict_meta[[ind1]]
        df_calc <- df_calc %>% 
          mutate(
            val1_txt = case_when(
              meta1$fmt == "money"   ~ paste0("$", scales::comma(.data[[ind1]], accuracy = 0.1), " M"),
              meta1$fmt == "percent" ~ paste0(scales::comma(.data[[ind1]], accuracy = 0.01), "%"),
              TRUE                   ~ scales::comma(.data[[ind1]], accuracy = 0.1)
            ),
            text_hover_shared = paste0(
              "<b>", country, "</b> (", time_factor, ")<br>",
              "<b>", meta1$label, ":</b> ", val1_txt
            )
          )
      }
      
      df_long <- df_calc %>% 
        select(time_factor, country, text_hover_shared, all_of(indicadores_sel)) %>% 
        pivot_longer(
          cols = all_of(indicadores_sel),
          names_to = "indicador_key",
          values_to = "valor_plot"
        ) %>% 
        mutate(
          indicador_lbl = factor(
            sapply(indicador_key, function(k) dict_meta[[k]]$label),
            levels = sapply(indicadores_sel, function(k) dict_meta[[k]]$label)
          )
        )
      
      fmt_primer_ind <- dict_meta[[indicadores_sel[1]]]$fmt
      y_scale <- if (fmt_primer_ind == "money") {
        scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
      } else {
        scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      if (input$filter_timeframe == "annual") {
        p <- ggplot(df_long, aes(x = time_factor, y = valor_plot, fill = country, alpha = indicador_lbl, text = text_hover_shared)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7) +
          scale_fill_manual(values = paleta_paises, name = "") +
          scale_alpha_manual(values = c(1, 0.55), name = "") +
          y_scale
      } else {
        p <- ggplot(df_long, aes(x = time_factor, y = valor_plot, color = country, linetype = indicador_lbl, group = interaction(country, indicador_lbl), text = text_hover_shared)) +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
          scale_color_manual(values = paleta_paises, name = "") +
          scale_linetype_manual(values = c("solid", "dashed"), name = "") +
          y_scale
      }
      
      p <- p +
        labs(title = "", x = "", y = dict_meta[[indicadores_sel[1]]]$unit) +
        theme_minimal() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_text(family = "Roboto", color = "gray35", margin = margin(r = 10), size = 14),
          legend.position = "top"
        )
      
      titulo_eje  <- paste(sapply(indicadores_sel, function(k) dict_meta[[k]]$label), collapse = " y ")
      titulo_html <- paste0(
        "<b>", lbl_timeframe, " de ", titulo_eje, " por país de origen (", años_txt, ")</b><br>",
        "<span style='font-size:15px; color:#666666; font-family:Roboto-bold;'>", subtitulo_hts, "</span>"
      )
      
      p_plotly <- ggplotly(p, tooltip = "text")
      
      names_seen <- c()
      for (i in seq_along(p_plotly$x$data)) {
        if (!is.null(p_plotly$x$data[[i]]$name)) {
          raw_name     <- p_plotly$x$data[[i]]$name
          country_name <- gsub("^\\((.*),(.*)\\)$", "\\1", raw_name)
          ind_name     <- gsub("^\\((.*),(.*)\\)$", "\\2", raw_name)
          
          if (!ind_name %in% names_seen) {
            names_seen <- c(names_seen, ind_name)
            p_plotly$x$data[[i]]$name <- paste0("<b>", ind_name, ":</b> ", country_name)
          } else {
            p_plotly$x$data[[i]]$name <- country_name
          }
        }
      }
      
      p_plotly <- p_plotly %>% 
        layout(
          margin = list(t = 70, l = 70, r = 70, b = 70), 
          title = list(
            text = titulo_html,
            y = 0.96,          
            x = 0.02,              
            xanchor = "left",
            xref = "container",
            font = list(size = 20, color = "#333333", family = "Roboto"),
            pad = list(b = 10)
          ),
          legend = list(
            orientation = "h",
            xanchor = "center",
            xref = "paper",
            x = 0.5,
            y = 0.9,          
            yanchor = "top",
            yref = "container",
            font = list(size = 14, family = "Roboto")
          ),
          annotations = list(
            list(
              text      = "Fuente: CEPCAN con datos de USITC.",
              x         = 0,
              y         = 0,               
              yshift    = -55,
              xshift    = -30,
              xref      = "paper",
              yref      = "paper",
              showarrow = FALSE,
              xanchor   = "left",
              yanchor   = "top",
              font      = list(size = 14, color = "#666666", family = "Roboto-medium")
            )
          ),
          xaxis = list(tickangle = angulo_eje_x, tickfont = list(family = "Roboto-medium", color = "gray35", size = 14)),
          yaxis = list(
            title = dict_meta[[indicadores_sel[1]]]$unit,
            nticks = 5,
            automargin = TRUE, 
            tickfont = list(family = "Roboto", color = "gray35", size = 14), 
            titlefont = list(family = "Roboto-medium", color = "gray35", size = 14)
          )
        )
      
      return(p_plotly)
    }
  })
  
  output$plot_trade_balance_series <- renderPlotly({
    plot_trade_balance_rx()
  })
  
  output$download_trade_plot_hd <- downloadHandler(
    filename = function() {
      paste0("laboratorio_datos_", Sys.Date(), ".png")
    },
    content = function(file) {
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(
        widget = plot_trade_balance_rx(), 
        file = temp_html, 
        selfcontained = TRUE
      )
      
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1600,   
        vheight = 800,  
        zoom = 2        
      )
    }
  )
  
  # TABLA RESUMEN PARA LA CONSULTA RANCELARIA Y DESCARGA DE CSV
  tabla_resumen <- reactive({
    df <- datos_filtrados()
    
    group_cols <- c("hs_code", "description", "country", "year")
    if (input$filter_timeframe == "monthly") {
      group_cols <- c(group_cols, "month")
    }
    
    res <- df %>% 
      group_by(across(all_of(group_cols))) %>% 
      summarise(
        Valor_Aduanero    = sum(customs_value, na.rm = TRUE),
        Valor_Gravable   = sum(dutiable_value, na.rm = TRUE),
        Aranceles_Pagados = sum(calculated_duties, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      mutate(
        Tasa_Implicita = if_else(Valor_Aduanero > 0, Aranceles_Pagados / Valor_Aduanero, 0),
        Tasa_Efectiva  = if_else(Valor_Gravable > 0, Aranceles_Pagados / Valor_Gravable, 0),
        Pct_Gravable   = if_else(Valor_Aduanero > 0, Valor_Gravable / Valor_Aduanero, 0)
      )
    
    if ("month" %in% names(res)) {
      res <- res %>% 
        mutate(month = factor(month, levels = meses_nombre_full, ordered = TRUE)) %>% 
        arrange(year, month) 
    } else {
      res <- res %>% arrange(year)
    }
    return(res)
  })
  

  output$table_hs_query <- DT::renderDataTable({
    DT::datatable(
      tabla_resumen(),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Código HS", "Descripción", "País", "Año", if ("month" %in% names(tabla_resumen())) "Mes" else NULL,
                   "Valor Aduanero ($)", "Valor Gravable ($)", "Derechos ($)", "Tasa Implícita (%)", "Tasa Efectiva (%)", "Valor gravable (%)")
    ) %>% 
      formatCurrency(c("Valor_Aduanero", "Valor_Gravable", "Aranceles_Pagados"), currency = "$", digits = 0) %>% 
      formatPercentage(c("Tasa_Implicita", "Tasa_Efectiva","Pct_Gravable"), digits = 2)
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("USITC_Consulta_HS", input$hs_level_select, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_download <- tabla_resumen() %>% 
        mutate(hs_code = paste0('="', hs_code, '"'))
      write.csv(df_download, file, row.names = FALSE)
    }
  )
}

# Ejecución de la APP -----------------------------------------------------

shinyApp(ui = ui, server = server)

