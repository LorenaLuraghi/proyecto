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
                        src="Puerto-vallarta.jpg")))),
  
  
  tabPanel("Distancias de estaciones al aeropuerto de Carrasco",type="tabset",
           sidebarLayout(position = "right",
                         sidebarPanel(
                           
                           radioButtons(inputId = "esta",
                                       label = "Estaciones con distancia:",
                                       c("Cerca","Media","Lejos"))         ),
           mainPanel(leafletOutput("mymap",height = 1000)))
           
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
    
    if(input$esta=="Cerca") { temperaturas %>% filter(nroEstacion==c(5,24,18,25,2,1,13,8,16,7)) %>% 
        leaflet() %>%  addTiles() %>% addCircles(lng = ~lon, lat = ~lat,label=~nroEstacion, color = "green")  }
    else if (input$esta=="Media") { temperaturas %>% filter(nroEstacion==c(11,10,21,22,6,20,23)) %>% 
        leaflet() %>%  addTiles() %>% addCircles(lng = ~lon, lat = ~lat,color = "yellow") }
    else {temperaturas %>% filter(nroEstacion==c(14,12,19,9,17,26,3,15,4)) %>% 
        leaflet() %>%  addTiles() %>% addCircles(lng = ~lon, lat = ~lat, color = "red")
      
    }
    
    
    
  })
}


shinyApp(ui, server)


