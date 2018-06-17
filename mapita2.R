library(ggplot2)
 library(ggvis)
 library(tidyverse)
 library(lubridate)
 library(raster)
 library(rgeos)
 library(rgdal)
 temperaturas <- read.csv("temperaturas.csv",sep="\t")
 temperaturas <- mutate(temperaturas, fecha=paste(dia,mes,anio, sep="-"))
 temperaturas <- mutate(temperaturas,fecha=dmy(temperaturas$fecha))
 #conseguimos datos de uruguay 
 uruguay <- getData("GADM", country = "UY", level = 0)
library(shiny)
 #datos de uruguay con los departamentos
uruguay_states <- getData("GADM", country = "UY", level = 1)
#transformamos los datos con la función spTransform
uystates_UTM <-spTransform(uruguay_states, CRS("+init=EPSG:5383"))
#extrae del objeto uystates_UTM los departamentos (si ponemos view en el elemento nos muestra)
NAME_1 <- uystates_UTM@data$NAME_1
temperaturas <- mutate(temperaturas,departamento=ifelse(nroEstacion %in% c(1,2,25),"Canelones",ifelse(nroEstacion %in% c(13),"Montevideo",ifelse(nroEstacion %in% c(16,26),"Rocha",ifelse(nroEstacion %in% c(3,4),"Artigas",ifelse(nroEstacion %in% c(15),"Rivera",ifelse(nroEstacion %in% c(9),"Cerro Largo",ifelse(nroEstacion %in% c(5,24),"Colonia",ifelse(nroEstacion %in% c(6),"Durazno",ifelse(nroEstacion %in% c(7),"Florida",ifelse(nroEstacion %in% c(8,14),"Maldonado",ifelse(nroEstacion %in% c(10),"Soriano",ifelse(nroEstacion %in% c(11,19),"Tacuarembó",ifelse(nroEstacion %in% c(12),"Paysandú",ifelse(nroEstacion %in% c(17,26),"Salto",ifelse(nroEstacion %in% c(18),"San José",ifelse(nroEstacion %in% c(20),"Treinta y Tres",ifelse(nroEstacion %in% c(21),"Flores","Río Negro"))))))))))))))))))
vectorestaciones <- c(3,1,9,5,6,21,7,0,8,13,12,22,15,16,17,18,10,11,20)
temperaturas <-temperaturas %>% filter(nroEstacion %in% vectorestaciones)
temperaturas <- mutate(temperaturas,Estacion= as.factor(temperaturas$nroEstacion))
temperaturas <- temperaturas %>% group_by(anio,mes,Estacion) %>% slice(which.min(tmin))
temperaturas <- mutate(temperaturas, fecha2=paste(mes,anio, sep="-"))
count_df <-data.frame(filter(temperaturas,fecha2=="1-2012")$departamento,filter(temperaturas,fecha2=="1-2012")$tmin)
     
names(count_df) <- c("NAME_1", "tmin")
#no hay estacion en lavalleja, la agregamos como un NA
lav <- data.frame(c("Lavalleja"), NA)
names(lav) <- c("NAME_1", "tmin")
count_df <- rbind(count_df,lav)
uystates_UTM@data$id <- rownames(uystates_UTM@data)
uystates_UTM@data <- plyr::join(uystates_UTM@data, count_df, by="NAME_1")
uystates_df <- fortify(uystates_UTM)

uystates_df <- plyr::join(uystates_df,uystates_UTM@data, by="id")

uystates_df <-uystates_df %>% filter(!(NAME_1=="Rivera"& lat<6400000)) #un error en el mapa que hay que sacar

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.background = element_blank(),
                             plot.background = element_blank(),
                             axis.line = element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             plot.title = element_blank()))
countunique <-uystates_df %>% group_by(NAME_1) %>% summarise(mlong = mean(long), mlat = mean(lat))



###countunique[8,] <- c("Rivera", 129381.96, 6441658)
b <- left_join(countunique,count_df)
 
tmin <- b$tmin
library(scales)
MAPITA <- ggplot() + geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill= tmin), color = "black", size = 0.25) +
    geom_text(data =countunique ,aes(label = round(tmin,2), x = mlong, y = mlat))+
    theme(aspect.ratio = 1) + labs(fill = "Temperatura mínima")+
    scale_fill_gradient2( midpoint = mean(b$tmin, na.rm=TRUE), low = muted ("blue"), high = muted("red"), mid="white",na.value = "green") +
    theme_opts 


MAPITA

###################################################

countunique <- left_join(countunique,count_df)

countunique$hover <- with(countunique, paste("Departamento", NAME_1, "<br>",
                           "Temperatura mínima", tmin))

library(plotly)
 MAPITA2 <- ggplot() + geom_polygon(data = uystates_df, aes(x = long, y = lat, group = group, fill= tmin), color = "black", size = 0.25) +
theme(aspect.ratio = 1) + labs(fill = "Temperatura mínima")+geom_text(data =countunique ,aes(label = NAME_1, x = mlong, y = mlat))+
scale_fill_gradient2( midpoint = mean(b$tmin, na.rm=TRUE), low = muted ("blue"), high = muted("red"), mid="white",na.value = "grey") +
theme_opts
  
 countunique$hover
 ## quiero que en las etiquetas salga la info que esta en hover, departamento y temp mínima
 
ggplotly(MAPITA2) 
##ggplotly(MAPITA2) %>% add_trace(countunique, text= ~countunique$hover)

