library(shiny)
library(shinycssloaders)
library(leaflet)
library(tidyverse)
library(rgdal)
library(stringi)
library(mapview)
# library(plotly)
# library(Cairo)

# options(shiny.usecairo=T)

dades <- read_csv("https://analisi.transparenciacatalunya.cat/api/views/xuwf-dxjd/rows.csv?accessType=DOWNLOAD&sorting=true", na = "")
save(dades, file = 'covid_cases.RData')

# load('covid_cases.RData')

# Canviem colnames
colnames(dades) <- stri_trans_general(sapply(colnames(dades), function(x) trimws(strsplit(x, "/")[[1]][1])), "Latin-ASCII")

dades[, "TipusCasData"] <- parse_date(dades$TipusCasData, "%d/%m/%Y")

# # Agrupem
# data_agg <- dades %>%
#   group_by(TipusCasData, ABSCodi) %>%
#   summarise(casos = sum(NumCasos))

# mapa <- download.file('http://salutweb.gencat.cat/web/.content/_departament/estadistiques-sanitaries/cartografia/ABS_2018.zip', destfile = 'ABS_2018.zip')

# shapeData <- readOGR("~/mapaABS",'ABS_2018', use_iconv=TRUE, encoding = 'UTF-8')
# shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))

min_data <- min(dades$TipusCasData)
max_data <- max(dades$TipusCasData)

arees_salut <- unique(dades$RegioSanitariaDescripcio)

# load("mapa_ABS.RData")
load("mapa_regio.RData")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Número de casos de COVID19 a Catalunya per Regió Sanitària"),
  
  tags$div(class="h3", checked=NA,
           "Font: ",
           tags$a(href="https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/xuwf-dxjd",
                  "Departament de Salut de la Generalitat de Catalunya")
  )
  ,
  column(6,
  dateRangeInput('dateRange',
                 label = 'Rang de dates (yyyy-mm-dd)',
                 start = min_data,
                 end = max_data,
                 language = 'ca',
                 separator = 'fins a',
                 weekstart = 1,
                 format = "dd/mm/yyyy")
  ,
  
  leafletOutput("distPlot", height = "95vh") %>% withSpinner()
  ),
  
  
  column(4, 
  selectInput("abs", "Escull una Regió Sanitària:",
              arees_salut),
  # plotlyOutput('plot_casos_plotly')
  plotOutput('plot_casos',
             width = 720,
             height = 480)
  )
  # ,
  
  # dataTableOutput('table')
  
)


# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  output$distPlot <- renderLeaflet({
    
    qui <- which(dades$TipusCasData >= input$dateRange[1] & dades$TipusCasData <= input$dateRange[2])
    
    # qui <- which(dades$TipusCasData >= input$time & dades$TipusCasData <= input$time + 5)
    
    data_agg <- dades[qui, ] %>% 
      group_by(RegioSanitariaCodi, RegioSanitariaDescripcio) %>% 
      summarise(casos = sum(NumCasos))
    
    # Fem match amb dades de data
    m <- match(mapa_regio_df@data$id_regio, data_agg$RegioSanitariaCodi)
    i <- which(!is.na(m))
    
    # mapa_regio_df@data[, c('casos', 'ABSDescripcio')] <- NA
    mapa_regio_df@data[i, 'casos'] <- data_agg$casos[na.omit(m)]
    # mapa_regio_df@data[i, 'ABSDescripcio'] <- data_agg$ABSDescripcio[na.omit(m)]
    # 

    mapa_regio_df %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorNumeric("YlOrRd", domain = mapa_regio_df@data$casos)(casos),
                  popup = paste0("Regió Sanitària: ", mapa_regio_df@data$nom_regio, "<br>Número de casos: ", mapa_regio_df@data$casos)) %>%
                  # popup = popupGraph(ggplot(shapeData@data) + geom_l)
      addLegend(pal = colorNumeric("YlOrRd", domain = mapa_regio_df@data$casos), values = ~casos, opacity = 1)
  })
  
  output$plot_casos<-renderPlot({
    data_agg <- dades %>% 
      group_by(RegioSanitariaCodi, RegioSanitariaDescripcio, TipusCasData) %>% 
      summarise(casos = sum(NumCasos))
    
    aa <- data_agg %>%
      filter(RegioSanitariaDescripcio == input$abs)
    
    ggplot(aa, aes(x=TipusCasData, y=casos)) +
      geom_line(color = 'red') +
      ggtitle(paste0("Casos detectats a ", input$abs)) +
      labs(y="Número de casos detectats", x = "Data") +
      geom_smooth(method = "loess")
  })
  # 
  # output$plot_casos_plotly<-renderPlotly({
  #   data_agg <- dades %>% 
  #     group_by(ABSCodi, ABSDescripcio, TipusCasData, SexeDescripcio) %>% 
  #     summarise(casos = sum(NumCasos))
  #   
  #   aa <- data_agg %>%
  #     filter(ABSDescripcio == input$abs)
  #   
  #   p <- ggplot(aa, aes(x=TipusCasData, y=casos, color = SexeDescripcio)) +
  #     geom_line() +
  #     ggtitle(paste0("Casos detectats a ", input$abs)) +
  #     labs(y="Número de casos detectats", x = "Data")
  #   
  #   ggplotly(p)
  # 
  # })
  # 
  
  # fig <- plot_ly(tg, x = ~dose, y = ~length, type = 'scatter', mode = 'lines', linetype = ~supp, color = I('black')) 
  
}

shinyApp(ui, server)
