library(shiny)
library(tidyverse)
library(lubridate)
library(interval)

ui <- fluidPage(
  titlePanel("Temperaturas del Uru"),
  sliderInput(inputId = "est",
              label = "Número de estación",
              min = 1,
              max = 26,
              value=2),
  sliderInput(inputId = "year",
              label = "Año",
              min =2004,
              max = 2015,
              value =c( 2002,2004)
  ),
  plotOutput("serie")
)


server <- function(input,output){
  output$serie <- renderPlot({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas %>% filter(nroEstacion==input$est) %>% filter(between(anio,min(input$year),max(input$year))) %>%
      ggplot(aes(x=fecha,y=tmin,colour=as.factor(anio))) + geom_point()
    
    
    
  })
}


shinyApp(ui, server)



