library(shiny)
library(shinycssloaders)
library(leaflet)
library(tidyverse)
library(rgdal)
library(stringi)
library(mapview)
library(lubridate)
library(geoloc)
# library(plotly)
# library(Cairo)

# options(shiny.usecairo=T)

# dades <- read_csv("https://analisi.transparenciacatalunya.cat/api/views/xuwf-dxjd/rows.csv?accessType=DOWNLOAD&sorting=true", na = "")
# save(dades, file = 'covid_cases.RData')

load('covid_cases.RData')

# Canviem colnames
colnames(dades) <- stri_trans_general(sapply(colnames(dades), function(x) trimws(strsplit(x, "/")[[1]][1])), "Latin-ASCII")

dades[, "TipusCasData"] <- parse_date(dades$TipusCasData, "%d/%m/%Y")

# mapa <- download.file('http://salutweb.gencat.cat/web/.content/_departament/estadistiques-sanitaries/cartografia/ABS_2018.zip', destfile = 'ABS_2018.zip')

# shapeData <- readOGR("~/mapaABS",'ABS_2018', use_iconv=TRUE, encoding = 'UTF-8')
# shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))

min_data <- min(dades$TipusCasData)
max_data <- max(dades$TipusCasData)

arees_salut <- sort(unique(dades$ABSDescripcio))
tipus_deteccio <- c('Tots', unique(dades$TipusCasDescripcio))


load("mapa_ABS.RData")
#shapeData
load("mapaCP_catalunya.RData")
# mapa
cp_valids <- unique(as.character(mapa@data$COD_POSTAL))

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  geoloc::onload_geoloc(),
  
  titlePanel("Número de casos de COVID19 a Catalunya per ABS (Àrea Bàsica de Salut)"),
  
  tags$div(class="h3", checked=NA,
           "Font: ",
           tags$a(href="https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/xuwf-dxjd",
                  "Departament de Salut de la Generalitat de Catalunya")
  )
  ,
  leafletOutput("lf")
  ,
  br(),
  p("Si la localització no és correcta o bé vols buscar una altra zona, pots informar el codi postal."),
  textInput("cp", "Codi postal:"),
  selectInput("detec", "Escull una tipus de deteccio:",
              tipus_deteccio),
  checkboxInput("checkbox", label = "Agregació setmanal", value = TRUE),
  # column(4, 
  #        # selectInput("abs", "Escull una Àrea Sanitària Bàsica:",
  #        #             arees_salut),
  # 
  #        # plotlyOutput('plot_casos_plotly')
  # ),
  plotOutput('plot_casos'
             # width = 720,
             # height = 480
             )
  
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # mapa amb la localitzacio segons navegador
  output$lf <- renderLeaflet({
    req(input$geoloc_lon)
    req(input$geoloc_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$geoloc_lon), as.numeric(input$geoloc_lat), zoom = 17) %>%
      addMarkers(as.numeric(input$geoloc_lon), as.numeric(input$geoloc_lat), label = "Estàs aquí!")
  })
  
  output$plot_casos <- renderPlot({
    # Comprovem que estiguis a Catalunya
    loc_catalunya <- FALSE
    if (!is.null(input$geoloc_lon)) {
      coords <- data.frame(x = as.numeric(input$geoloc_lon), y = as.numeric(input$geoloc_lat))
      coordinates(coords) <- coords
      proj4string(coords) <- proj4string(shapeData)
      codi_abs <- as.character(over(coords, shapeData)[, 'CODIABS'])
      if (!is.na(codi_abs)) {
        loc_catalunya <- TRUE
      }
    }
    
    # Si estas a catalunya i no has informat cp, trobem ABS associada a la geoloc
    
    if (input$cp == "" & loc_catalunya) {
      abs <- dades[match(codi_abs, dades$ABSCodi), ]$ABSDescripcio
    } else {
    # Validem que el cp sigui correcte
      validate(
        need(input$cp %in% cp_valids, 'El codi postal no és correcte.')
      )
      
      # Trobem l'ABS del CP (aprox)
      qui <- which(mapa@data$COD_POSTAL == input$cp)
      
      # posem punts a l'atzar en el cp
      n <- 100
      points_cp <- spsample(mapa[qui, ], n, 'regular')
      
      # centr_cp <- as.data.frame(coordinates(mapa[qui, ]))
      # coordinates(centr_cp) <- centr_cp
      proj4string(points_cp) <- proj4string(shapeData)
      
      codis_abs <- as.character(over(points_cp, shapeData)[, 'CODIABS'])
      codi_abs <- names(which.max(table(codis_abs)))
      abs <- dades[match(codi_abs, dades$ABSCodi), ]$ABSDescripcio
    }
    
    data_agg <- dades %>% 
      group_by(ABSCodi, ABSDescripcio, TipusCasData, TipusCasDescripcio) %>% 
      summarise(casos = sum(NumCasos))
    
    
    if (input$detec == 'Tots') {
      aa_week <- data_agg %>%
        filter(ABSDescripcio == abs) %>% 
        group_by(TipusCasData = week(TipusCasData)) %>% 
        summarise(casos = sum(casos))
      aa <- data_agg %>%
        filter(ABSDescripcio == abs) %>% 
        group_by(TipusCasData) %>% 
        summarise(casos = sum(casos))
    } else {
      aa_week <- data_agg %>%
        filter(ABSDescripcio == abs & TipusCasDescripcio == input$detec) %>% 
        group_by(TipusCasData = week(TipusCasData)) %>% 
        summarise(casos = sum(casos))
      aa <- data_agg %>%
        filter(ABSDescripcio == abs & TipusCasDescripcio == input$detec) %>% 
        group_by(TipusCasData) %>% 
        summarise(casos = sum(casos))
    }
    
    # Calculem contagis en els ultims n dies contra el promig en els últims k periodes 
    # de n dies cadascun
    
    n <- 7
    k <- 4
    cont_ara <- sum(aa$casos[(nrow(aa)-n + 1):nrow(aa)])
    cont_promig <- sum(aa$casos[(nrow(aa) - k*n + 1):(nrow(aa) - n)]) / k
    perc <- round((cont_ara/cont_promig - 1) * 100, 1)
    
    if (input$checkbox) {
      aa <- aa_week
    }
    
    ggplot(aa, aes(x=TipusCasData, y=casos)) +
      geom_line(color = 'red') +
      ggtitle(paste0("Casos detectats a ", abs), subtitle = paste0('Els contagiats han augmentat un ', perc, "% en l'últim periode de ", n, ' dies repecte els anteriors ', k, ' periodes.')) +
      # labs(y=paste0("Número de casos detectats (", input$detec, ")"), x = "Data") +
      geom_smooth(method = "loess")
  })
  
  
  
}

shinyApp(ui, server)
