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
library(dplyr)
library(plotly)
library(scales)
library(shinythemes)

temperaturas <- read.csv("temperaturas.csv",sep="\t")
temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
temperaturas <- subset(temperaturas, select = c(1,2,3,10,11) )

ui <- navbarPage(theme = shinytheme("cerulean"),
  title="Temperaturas mínimas del Uruguay",
  
  
  tabPanel("Base de datos" , type= "tabset",
           fluidRow(
             h4("Se  cuenta  con  una  base  de  datos compuesta  por  registros  diarios  de  temperaturas  mínimas  de  26  estaciones  meteorológicas  de  Uruguay  para  el  período  2002-2014.  Los  datos  están  comprendidos  entre  el  1º  de  enero  de  2002  y  el  
                31  de  diciembre  de  2014,  lo  cual  implica  un  total  de  4.526  observaciones  por  estación."),
             fluidRow(
               column(4,
                      selectInput("nroest",
                                  "Número de estación:",
                                  c("Todas",
                                    unique(as.character(temperaturas$nroEstacion))))
               )
             ),
             column(12,
                    dataTableOutput('table')
             )
             
             
             )),
  
  
  tabPanel("Visualización",type="tabset",
           
           fluidRow(column(8,leafletOutput("mymap",height = 500)),
                    
                    br(),
                    
                    column( 3,selectizeInput(inputId = "estac",
                                             label = "Número de estación",
                                             choices=list("E1"=1, "Las Brujas"=2, "E3"=3, "E4"=4, "E5"=5,"E6"=6,"e7"=7,
                                                          "e8"=8,"e9"=9, "e10"=10,"e11"=11,"e12"=12,"e13"=13,"e14"=14,"e15"=15,"e16"=16,"e17"=17,"e18"=18,"e19"=19,"e20"=20,"e21"=21,
                                                          "e22"=22,"e23"=23, "e24"=24,"e25"=25,"e26"=26),
                                             selected = 2 ,
                                             options = list(maxItems = 4L),
                                             multiple = TRUE ),
                            
                            
                            sliderInput(inputId = "year",
                                        label = "Año",
                                        min =2004,
                                        max = 2015,
                                        value =c( 2002:2004) ),
                            
                            dateInput(inputId = "fe",
                                      label = "Seleccionar Fecha",
                                      value= "2012-05-01")
                    )
           ),
           hr(),
           
           fluidRow( column(1),
                     column(4,h2("Temperaturas minimas mensuales para las estaciones seleccionadas")
                     ),
                     column (1), column(4, h2("Temperaturas mínimas mensuales por Departamento"))),
           
           hr(),
           
           fluidRow(
             
             column(6, plotOutput("serie")),
             
             column(6, plotlyOutput("Mapita"))
           )),
  
  
  
  tabPanel ("Cambiar nombre", type="tabset",
            
            radioButtons("bloque", label = h3("Tamaño del bloque"), 
                         choices = list("Un año" = 1, "Un mes" = 2),
                         selected = 1),
            
            fluidRow( 
              column(12,
                     dataTableOutput('tablabloque')
              )
              
              
            )))



server <- function(input,output,session){
  output$serie <- renderPlot({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas <- mutate(temperaturas,Estacion= as.factor(temperaturas$nroEstacion))
    
    temperaturas %>% filter(Estacion %in% c(input$estac)) %>% filter(between(anio,min(input$year),max(input$year))) %>% group_by(anio,mes,Estacion) %>%
      slice(which.min(tmin)) %>%
      ggplot(aes(x=fecha,y=tmin, color=Estacion)) + geom_line()+labs(x="Fecha", y="Temperatura mínima mensual")+
      geom_vline(xintercept = 2005,colour="black", size=2)
    
  })
  
  output$table <- renderDataTable({ 
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas <- subset(temperaturas, select = c(1,2,3,10,11) )
    
    if (input$nroest != "Todas") {
      temperaturas <- temperaturas[temperaturas$nroEstacion == input$nroest,]
    } 
    temperaturas
    
    
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    
    
    
    
    temperaturas %>% filter(nroEstacion%in% c(input$estac)) %>% leaflet() %>% addTiles() %>% addCircles(lng = ~lon, lat = ~lat,label=~nroEstacion, color = "green") 
    
  })
  
  
  output$Mapita <- renderPlotly({
    
    temperaturas <- read.csv("temperaturas.csv", sep = "\t")
    temperaturas <- mutate(temperaturas, fecha = paste(dia, mes, 
                                                       anio, sep = "-"))
    temperaturas <- mutate(temperaturas, fecha = dmy(temperaturas$fecha))
    # conseguimos datos de Uruguay
    uruguay <- getData("GADM", country = "UY", level = 0)
    library(shiny)
    # datos de Uruguay con los departamentos
    uruguay_states <- getData("GADM", country = "UY", level = 1)
    # transformamos los datos con la función spTransform
    uystates_UTM <- spTransform(uruguay_states, CRS("+init=EPSG:5383"))
    # extrae del objeto uystates_UTM los departamentos (si
    # ponemos view en el elemento nos muestra)
    NAME_1 <- uystates_UTM@data$NAME_1
    temperaturas <- mutate(temperaturas, departamento = ifelse(nroEstacion %in% 
                                                                 c(1, 2, 25), "Canelones", ifelse(nroEstacion %in% c(13), 
                                                                                                  "Montevideo", ifelse(nroEstacion %in% c(16, 26), "Rocha", 
                                                                                                                       ifelse(nroEstacion %in% c(3, 4), "Artigas", ifelse(nroEstacion %in% 
                                                                                                                                                                            c(15), "Rivera", ifelse(nroEstacion %in% c(9), "Cerro Largo", 
                                                                                                                                                                                                    ifelse(nroEstacion %in% c(5, 24), "Colonia", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                          c(6), "Durazno", ifelse(nroEstacion %in% c(7), 
                                                                                                                                                                                                                                                                                  "Florida", ifelse(nroEstacion %in% c(8, 14), 
                                                                                                                                                                                                                                                                                                    "Maldonado", ifelse(nroEstacion %in% c(10), 
                                                                                                                                                                                                                                                                                                                        "Soriano", ifelse(nroEstacion %in% c(11, 
                                                                                                                                                                                                                                                                                                                                                             19), "Tacuarembó", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                                                                                                                                                         c(12), "Paysandú", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                                                                                                                                                                                     c(17, 26), "Salto", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                                                                                                                                                                                                                  c(18), "San José", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              c(20), "Treinta y Tres", ifelse(nroEstacion %in% 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                c(21), "Flores", "Río Negro"))))))))))))))))))
    vectorestaciones <- c(3, 1, 9, 5, 6, 21, 7, 0, 8, 13, 12, 22, 
                          15, 16, 17, 18, 10, 11, 20)
    temperaturas <- temperaturas %>% filter(nroEstacion %in% vectorestaciones)
    temperaturas <- mutate(temperaturas, Estacion = as.factor(temperaturas$nroEstacion))
    temperaturas <- temperaturas %>% group_by(anio, mes, Estacion) %>% 
      slice(which.min(tmin))
    temperaturas <- mutate(temperaturas,fecha2=as.Date((paste(as.character(anio),as.character( mes),"01", sep='-'))))
    
    count_df <- data.frame(filter(temperaturas, fecha2 == input$fe)$departamento, 
                           filter(temperaturas, fecha2 == input$fe)$tmin)
    
    names(count_df) <- c("NAME_1", "tmin")
    # no hay estacion en Lavalleja, la agregamos como un NA
    lav <- data.frame(c("Lavalleja"), NA)
    names(lav) <- c("NAME_1", "tmin")
    count_df <- rbind(count_df, lav)
    uystates_UTM@data$id <- rownames(uystates_UTM@data)
    uystates_UTM@data <- plyr::join(uystates_UTM@data, count_df, 
                                    by = "NAME_1")
    uystates_df <- fortify(uystates_UTM)
    
    uystates_df <- plyr::join(uystates_df, uystates_UTM@data, by = "id")
    
    uystates_df <- uystates_df %>% filter(!(NAME_1 == "Rivera" & 
                                              lat < 6400000))  #un error en el mapa que hay que sacar
    
    theme_opts <- list(theme(panel.grid.minor = element_blank(), 
                             panel.grid.major = element_blank(), panel.background = element_blank(), 
                             plot.background = element_blank(), axis.line = element_blank(), 
                             axis.text.x = element_blank(), axis.text.y = element_blank(), 
                             axis.ticks = element_blank(), axis.title.x = element_blank(), 
                             axis.title.y = element_blank(), plot.title = element_blank()))
    countunique <- uystates_df %>% group_by(NAME_1) %>% summarise(mlong = mean(long), 
                                                                  mlat = mean(lat))
    
    
    
    ### countunique[8,] <- c('Rivera', 129381.96, 6441658)
    b <- left_join(countunique, count_df)
    
    tmin <- b$tmin
    library(scales)
    #  MAPITA <- ggplot() + geom_polygon(data = uystates_df, aes(x = long, 
    #   y = lat, group = group, fill = tmin), color = "black", size = 0.25) + 
    # geom_text(data = countunique, aes(label = round(tmin, 2), 
    # x = mlong, y = mlat)) + theme(aspect.ratio = 1) + labs(fill = "Temperatura mínima") + 
    # scale_fill_gradient2(midpoint = mean(b$tmin, na.rm = TRUE), 
    # low = muted("blue"), high = muted("red"), mid = "white", 
    #  na.value = "green") + theme_opts
    
    
    # MAPITA
    
    ################################################### 
    
    countunique <- left_join(countunique, count_df)
    
    countunique$hover <- with(countunique, paste("Departamento", 
                                                 NAME_1, "<br>", "Temperatura mínima", tmin))
    
    uystates_df <-uystates_df %>% mutate(Departamento = NAME_1)
    
    library(plotly)
    MAPITA2 <- ggplot(data = uystates_df, aes(dpto = Departamento)) + geom_polygon(data = uystates_df, aes(x = long, 
                                                                                                           y = lat, group = group, fill = tmin), color = "black",
                                                                                   size = 0.25, show.legend = TRUE) + 
      theme(aspect.ratio = 1) + labs(fill = "Temperatura mínima") + 
      #geom_text(aes(label=NAME_1, inherit.aes=TRUE)) +
      # 
      # geom_text(data = countunique, 
      #           aes(label = NAME_1, x = mlong, y = mlat)) + 
      scale_fill_gradient2(midpoint = mean(b$tmin, na.rm = TRUE),
                           low = muted("blue"), high = muted("red"),
                           mid = "white", na.value = "grey") + 
      theme_opts
    
    countunique$hover
    ## quiero que en las etiquetas salga la info que esta en
    ## hover, departamento y temp mínima
    
    g <- ggplotly(MAPITA2)
    ## ggplotly(MAPITA2) %>% add_trace(countunique, text=
    ## ~countunique$hover)
    
    ggplotly(g)
    
  })
  
  output$tablabloque <- renderDataTable({ 
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas <- subset(temperaturas, select = c(1,2,3,5,6,10,11) )
    temperaturas <- temperaturas %>% mutate(tmin=-1*tmin) %>% filter(mes %in% 5:9) %>% filter(nroEstacion==2) %>% 
      filter(!is.na(tmin))
    
    if (input$bloque== 1) {
      temperaturas <- temperaturas %>% group_by(anio) %>% summarise(max=max(tmin))
    } 
    else {temperaturas <- temperaturas %>% group_by(anio,mes) %>% summarise(max=max(tmin))}
    
    
    
    
    
  })
  
  
}


shinyApp(ui, server)
