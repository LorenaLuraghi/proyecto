library(ggplot2)
library(shiny)
library(ggvis)
library(tidyverse)
library(lubridate)
library(raster)
library(rgeos)

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


count_df <-data.frame(filter(temperaturas,fecha=="2012-05-24")$departamento,filter(temperaturas,fecha=="2012-05-24")$tmin)
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
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f",fill=~tmin) %>% 
  hide_axis("x") %>% hide_axis("y")



