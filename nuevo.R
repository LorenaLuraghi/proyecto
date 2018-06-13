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

temperaturas <- read.csv("temperaturas.csv",sep="\t")
temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
temperaturas <- subset(temperaturas, select = c(1,2,3,10,11) )

ui <- navbarPage(
  title="Temperaturas mínimas del Uruguay",
  
  
  tabPanel("Introducción",type="tabset",
           fluidRow(
             column(6,
                    includeMarkdown("introduccion.Rmd")
             ),
             column(3,
                    img(class="img-polaroid",
                        src="Puerto-vallarta.jpg")))),
  
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
  
  
  tabPanel("Exploración de la base de datos",type="tabset",
           
           fluidRow(column(8,leafletOutput("mymap",height = 500)),
                    
                    br(),
                    
                    column( 3,selectizeInput(inputId = "estac",
                                             label = "Estación",
                                             choices=list("Carrasco,Canelones"=1, "Melilla,Canelones"=2, "Artigas, Artigas"=3, "Coronado, Artigas"=4, "Laguna de los Patos,Colonia"=5,"Santa Bernardina,Durazno"=6,"Florida,Florida"=7,
                                                          "Laguna del Sauce,Maldonado"=8,"Melo, Cerro Largo"=9, "Mercedes,Soriano"=10,"Paso de los Toros, Tacuarembó"=11,"Chacras de Paysandú, Paysandú"=12,"Prado,Montevideo"=13,"Punta del Este, Maldonado"=14,"Rivera,Rivera"=15,"Ciudad de Rocha,Rocha"=16,"Nueva Hesperides,Salto"=17,"San José, San José"=18,"Ciudad de Tacuarembó, Tacuarembó"=19,"Treinta y Tres,Treinta y Tres"=20,"Trinidad,Flores"=21,
                                                          "Young,Río Negro"=22,"Lascano,Rocha"=23, "El Semillero,Colonia"=24,"Las Brujas,Canelones"=25,"El Naranjal,Salto"=26),
                                             selected = 2 ,
                                             options = list(maxItems = 4L),
                                             multiple = TRUE
                    ),
                    
                    
                    
                    
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
                                step= 0.2 )
                    )),
           
           
           fluidRow(
             
             column(12, plotOutput("serie"))
           )),
  
  
  
  tabPanel("Mapa temperaturas", type="tabset",
           
           fluidRow( h4("Se muestra las temperaturas minímas registradas para cada depertamento según mes y año seleccionados")
             
           ),
           
           
           
           
           sidebarLayout(
             sidebarPanel(
               dateInput(inputId = "dia",
                         label ="Seleccionar dia",
                         value="2012-05-24")
             ),
             mainPanel = (ggvisOutput("mapauy"))
           ) ),
  tabPanel("Metodología", type="tabset",
           
           fluidRow(
             column(12,
                    includeMarkdown("metodologia.Rmd")
             ),
             column(12, 
                    h4("aca irian los boxplot y avisamos que usaremos solo algunos meses, 
                       eso se puede justificar porque no queremos estacionalidad"))
           )),
  tabPanel ("Método Block Máxima", type="tabset",
            
            radioButtons("bloque", label = h3("Tamaño del bloque"), 
                               choices = list("Un año" = 1, "Un mes" = 2),
                               selected = 1),
            
            fluidRow( 
              column(12,
                   dataTableOutput('tablabloque')
              )
            
            
            ),
            
            
            
            

  
  
  tabPanel("Método del Umbral")
  
 ))


server <- function(input,output,session){
  output$serie <- renderPlot({
    temperaturas <- read.csv("temperaturas.csv",sep="\t")
    temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
    temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
    temperaturas <- mutate(temperaturas,Estacion= as.factor(temperaturas$nroEstacion))
    
    temperaturas %>% filter(Estacion %in% c(input$estac)) %>% filter(between(anio,min(input$year),max(input$year))) %>% group_by(anio,mes,Estacion) %>%
      slice(which.min(tmin)) %>%
      ggplot(aes(x=fecha,y=tmin, color=Estacion)) + geom_line()+geom_hline(yintercept = input$m, colour="red",size=1)+
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
