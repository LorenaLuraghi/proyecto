library(shiny)
library(tidyverse)
library(lubridate)


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
              value =c( 2002:2004)
  ),
  sliderInput(inputId ="m",
              label = "superior",
              min = -10,
              max=30,
              value=12,
              step= 0.2 ),
  plotOutput("serie")
  
)


server <- function(input,output){
  output$serie <- renderPlot({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas %>% filter(nroEstacion==input$est) %>% filter(between(anio,min(input$year),max(input$year))) %>%
      ggplot(aes(x=fecha,y=tmin,colour=as.factor(anio))) + geom_point()+geom_hline(yintercept = input$m, colour="red",size=2)+
      
      ggtitle(paste("Temperaturas de la estación N°",input$est))
    
    ggtitle(paste("Temperaturas de la estación N°:",input$est))
    
  })
}
    
    
shinyApp(ui, server)
