library(shiny)
library(tidyverse)
library(lubridate)
library(rmarkdown)
library(leaflet)


ui <- navbarPage(
  title="Temperaturas mínimas del Uruguay",
  
  tabPanel("Base de datos" , type= "tabset",
           fluidRow(
             column(12,
                    dataTableOutput('table')
             )
           )
           
           
           ),
  
  tabPanel("Visualizaciones",type="tabset",
           sidebarLayout(position = "right",
                         sidebarPanel(
                           
                           sliderInput(inputId = "est",
                                       label = "Número de estación",
                                       min = 1,
                                       max = 26,
                                       value=2),
                           sliderInput(inputId = "year",
                                       label = "Año",
                                       min =2004,
                                       max = 2015,
                                       value =c( 2002:2004)
                           ),
                           sliderInput(inputId ="m",
                                       label = "superior",
                                       min = -10,
                                       max=30,
                                       value=12,
                                       step= 0.2 )),
                         
                         
                         mainPanel(plotOutput("serie"))
                         
           )),
  
  tabPanel("Introducción",type="tabset",
           fluidRow(
             column(6,
                    includeMarkdown("introduccion.Rmd")
             ),
             column(3,
                    img(class="img-polaroid",
                        src=paste0("https://media.elobservador.com.uy/adjuntos/181/imagenes/018/126/0018126280.jpeg"))))),
  
  
  tabPanel("Mapa",type="tabset",
           sidebarLayout(position = "right",
                         sidebarPanel(
                           
                           sliderInput(inputId = "esta",
                                       label = "Número de estación",
                                       min = 1,
                                       max = 26,
                                       value=2)         ),
           mainPanel(leafletOutput("mymap")))
           
  )
)



server <- function(input,output,session){
  output$serie <- renderPlot({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas %>% filter(nroEstacion==input$est) %>% filter(between(anio,min(input$year),max(input$year))) %>%
      ggplot(aes(x=fecha,y=tmin,colour=as.factor(anio))) + geom_point()+geom_hline(yintercept = input$m, colour="red",size=2)+
      
      ggtitle(paste("Temperaturas de la estación N°:",input$est))
    
  })
  
  output$table <- renderDataTable({ temperaturas <- read.csv("temperaturas.csv",sep="\t")
  temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
  temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
  
    
    })
  
  
  
  
  output$mymap <- renderLeaflet({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    
    temperaturas %>% filter(nroEstacion==input$esta) %>% 
    leaflet() %>%  addTiles() %>% addCircles(lng = ~lon, lat = ~lat, color = "yellow")
    
  })
}


shinyApp(ui, server)


