

library(shiny)
library(shinycssloaders)
library(leaflet)
library(tidyverse)
library(rgdal)
library(stringi)

data <- read_csv("https://analisi.transparenciacatalunya.cat/api/views/xuwf-dxjd/rows.csv?accessType=DOWNLOAD&sorting=true", na = "")

# Canviem colnames
colnames(data) <- stri_trans_general(sapply(colnames(data), function(x) trimws(strsplit(x, "/")[[1]][1])), "Latin-ASCII")

data[, "TipusCasData"] <- parse_date(data$TipusCasData, "%d/%m/%Y")

# # Agrupem
# data_agg <- data %>%
#   group_by(TipusCasData, ABSCodi) %>%
#   summarise(casos = sum(NumCasos))

# mapa <- download.file('http://salutweb.gencat.cat/web/.content/_departament/estadistiques-sanitaries/cartografia/ABS_2018.zip', destfile = 'ABS_2018.zip')

# shapeData <- readOGR("~/mapaABS",'ABS_2018', use_iconv=TRUE, encoding = 'UTF-8')
# shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))

min_data <- min(data$TipusCasData)
max_data <- max(data$TipusCasData)

load("mapa_ABS.RData")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Número de casos de COVID19 a Catalunya per ABS (Àrea Bàsica de Salut)"),
  
  tags$div(class="h3", checked=NA,
           "Font: ",
           tags$a(href="https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/xuwf-dxjd",
                  "Departament de Salut de la Generalitat de Catalunya")
  )
  ,
  dateRangeInput('dateRange',
                 label = 'Rang de dates (yyyy-mm-dd)',
                 start = min_data,
                 end = max_data,
                 language = 'ca',
                 separator = 'fins a',
                 weekstart = 1,
                 format = "dd/mm/yyyy")
  ,
  
  sliderInput("time", "date", min_data,
              max_data,
              value = min_data,
              step=1,
              animate=T),
  
  leafletOutput("distPlot", height = "95vh") %>% withSpinner(),
  
  p()
  # ,
  
  # dataTableOutput('table')
  
)


# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # # Creem dades reactives
  # data_agg <- react
  #   data_agg <- 
  
  aux <- reactive({
    shapeData@data %>%
      left_join((data %>% 
                  filter(TipusCasData >= input$dateRange[1] & TipusCasData <= input$dateRange[2]) %>% 
                  group_by(ABSCodi) %>% 
                  summarise(casos = sum(NumCasos))), by = c("CODIABS" = "ABSCodi")) %>% 
      select(casos) %>% 
      pull('casos')
  })
  print(aux)
  
  colorpal <- reactive({
    colorNumeric("YlOrRd", domain = aux())
  })
  
  
  output$distPlot <- renderLeaflet({
    
    shapeData %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                                  opacity = 1.0, fillOpacity = 0.5)
    
  })
  
  
  observe({
    pal <- colorpal()
    shapeData@data$casos <- aux()
    
    leafletProxy("distPlot", data = shapeData) %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(shapeData@data$casos),
                  popup = paste0("Número de casos: ", shapeData@data$casos)) %>% 
      addLegend(pal = pal, values = ~shapeData@data$casos, opacity = 1)


  })
  
  # output$table <- renderDataTable({
  #   qui <- which(data$TipusCasData >= input$dateRange[1] & data$TipusCasData <= input$dateRange[2])
  #                                     
  #   data_agg <- data[qui, ] %>% 
  #   group_by(ABSCodi, ABSDescripcio) %>% 
  #   summarise(casos = sum(NumCasos))})
}

shinyApp(ui, server)
