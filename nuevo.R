library(shiny)
library(tidyverse)
library(lubridate)
library(rmarkdown)
library(leaflet)
library(ggplot2)
library(shiny)
library(ggvis)
library(raster)
library(rgeos)
library(rgdal)



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
           
  ),
  
  tabPanel("Mapa temperaturas", type="tabset",
           
           
           
           
          sidebarLayout(
            sidebarPanel(
              dateInput(inputId = "dia",
                        label ="Seleccionar dia",
                        value="2012-05-24")
            ),
            mainPanel = (ggvisOutput("mapauy"))
          ) )
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
        leaflet() %>%  addTiles() %>% addCircles(lng = ~lon, lat = ~lat, color = "red
                                                 ")
      
    }
    
    
    
  })
  
  
  output$mapauy <- reactive({
    
    
    
    
  
  
  
  temperaturas <- read.csv("temperaturas.csv",sep="\t")
  temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
  temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
  
  
  #conseguimos datos de uruguay 
  uruguay <- getData("GADM", country = "UY", level = 0)
  
  
  #datos de uruguay con los departamentos
  uruguay_states <- getData("GADM", country = "UY", level = 1)
  
  #transformamos los datos con la función spTransform
  uystates_UTM <-spTransform(uruguay_states, CRS("+init=EPSG:5383")) 
  
  
  #extrae del objeto uystates_UTM los departamentos (si ponemos view en el elemento nos muestra)
  NAME_1 <- uystates_UTM@data$NAME_1
  
  #la idea ahora es crear un data.frame que asigne a cada departamento un valor de interes.
  #para esto tendriamos que elegir que estación va a representar cada departamento, o crear una variable
  #en la base de datos que sea departamento, para asignarle valores de interés...
  
  temperaturas <- mutate(temperaturas,departamento=ifelse(nroEstacion %in% c(1,2,25),"Canelones",ifelse(nroEstacion %in% c(13),"Montevideo",ifelse(nroEstacion %in% c(16,26),"Rocha",ifelse(nroEstacion %in% c(3,4),"Artigas",ifelse(nroEstacion %in% c(15),"Rivera",ifelse(nroEstacion %in% c(9),"Cerro Largo",ifelse(nroEstacion %in% c(5,24),"Colonia",ifelse(nroEstacion %in% c(6),"Durazno",ifelse(nroEstacion %in% c(7),"Florida",ifelse(nroEstacion %in% c(8,14),"Maldonado",ifelse(nroEstacion %in% c(10),"Soriano",ifelse(nroEstacion %in% c(11,19),"Tacuarembó",ifelse(nroEstacion %in% c(12),"Paysandú",ifelse(nroEstacion %in% c(17,26),"Salto",ifelse(nroEstacion %in% c(18),"San José",ifelse(nroEstacion %in% c(20),"Treinta y Tres",ifelse(nroEstacion %in% c(21),"Flores","Río Negro"))))))))))))))))))
  
  #para empezar eligiremos una estación representativa para cada depto, despues podemos ver de fusionar
  #varias, pero no sabemos como se hace je 
  
  vectorestaciones <- c(3,1,9,5,6,21,7,0,8,13,12,22,15,16,17,18,10,11,20)
  
  temperaturas <-temperaturas %>% filter(nroEstacion %in% vectorestaciones)
  
  #aca es donde deberiamos poner un input$fecha cuando lo pasemos al shiny.
  
  
  count_df <-data.frame(filter(temperaturas,fecha==input$dia)$departamento,filter(temperaturas,fecha==input$dia)$tmin)
  names(count_df) <- c("NAME_1", "tmin")
  
  #no hay estacion en lavalleja, la agregamos como un NA
  lav <- data.frame(c("Lavalleja"), NA)
  names(lav) <- c("NAME_1", "tmin")
  
  #unimos los dos data frame y los ordeno alfabeticamente
  
  count_df <- rbind(count_df,lav)
  count_df <- count_df[order(count_df$NAME_1),]
  
  
  uystates_UTM@data$id <- rownames(uystates_UTM@data)
  uystates_UTM@data <- plyr::join(uystates_UTM@data, count_df, by="NAME_1")
  uystates_df <- fortify(uystates_UTM)
  uystates_df <- plyr::join(uystates_df,uystates_UTM@data, by="id")
  
  #Mapa basico usando ggvis
  
  uystates_df %>%
    ggvis(~long, ~lat) %>%
    group_by(group, id,tmin) %>%
    layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f",fill=~tmin)  %>%  
    hide_axis("x") %>% hide_axis("y") %>% bind_shiny("mapauy")
  })
}


shinyApp(ui, server)
