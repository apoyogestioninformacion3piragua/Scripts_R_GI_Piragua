# Script para obtención de datos de precipitación
# de las estaciones automáticas de Piragua-Corantioquia

# Cargamos librerías

library(tidyverse)
library(urltools)
library(lubridate)
library(dplyr)
options(timeout=20000)
options(scipen=999)

# Limpiamos el global enviroment

rm(list = ls())

# Definimos temporalidad para extracción de datos

comenzar = "2021-04-14"
finalizar = "2021-04-21"

# Leemos estaciones

estaciones = read.csv("http://www.piraguacorantioquia.com.co/redes_nuevo/images/Estaciones_Piragua.csv")
estaciones$muniCodi = as.character(paste(estaciones$municipio, estaciones$codigo))

# Filtramos por territoriales (cambiar territorial en filter):
# territorial == "XXXX"
estaciones_pluvio = estaciones %>%
  filter(tipo == "Pluviógrafo")


# Definimos función de lectura de datos

get_data = function(estacion, inicio, final){
  url="https://www.piraguacorantioquia.com.co/api/v1/precipitacion/40?date_estacion__gte=2020-11-17&date_estacion__lt=2020-11-23&downloadfile"
  url2 = param_set(url, "date_estacion__gte", inicio)
  url3 = param_set(url2, "date_estacion__lt", final)
  data = read.csv(gsub("40", estacion, url3))
  data = na.omit(data)
  data = data[!(data$fecha == ""), ] # Para borrar campos de fechas vacíos
  data$fecha = as.POSIXct(data$fecha, "%Y-%m-%d %H-%M")
  data2 = data
}


# Función de extracción y conversión de datos

get_lluvia = function() {
  
  lluvia = list()
  
  # Guardamos en lista de listas todos los datasets de las estaciones 
  # para una territorial en particular
  for (i in 1:nrow(estaciones_pluvio)) {
    
    estacion = estaciones_pluvio$codigo[i]
    lluvia[[i]] = get_data(estacion, comenzar, finalizar)
    
  }
  # Agregamos una columna de municipios a cada dataframe de la lista
  for (i in 1:length(lluvia)) {
    
    if (nrow(lluvia[[i]]) > 0) {
      lluvia[[i]] = data.frame(lluvia[[i]],
                               territorial = estaciones_pluvio[i,5],
                               municipios = estaciones_pluvio[i,4],
                               ubicacion = estaciones_pluvio[i,11],
                               muniCodi = estaciones_pluvio[i,17])
      
    }
  }
  #Para eliminar listas con 0 filas
  lluvia = lluvia[sapply(lluvia, nrow) > 0]
  #Creamos un dataframe global con la lista de estaciones que tienen datos
  lluvia = do.call(rbind, lluvia)
  
}

# Descargamos y almacenamos
lluvia = get_lluvia()

#Agrupamos por municipios y hallamos su acumulado para la temporalidad dada
lluvia2 = lluvia %>%
  group_by(muniCodi, territorial, ubicacion) %>%
  dplyr::summarise(muestra = sum(muestra))


# Para exportar los datos
write.table(lluvia2,"lluvia.txt", sep = ";", 
            col.names = c("municipios", "territorial", "ubicacion", "muestra"),
            row.names = F)

lluvia3 = lluvia

# Convertimos a formato AAAA-MM-DD
lluvia3$fechas = as.Date(lluvia$fechas)

lluvia4 = aggregate(lluvia3["muestra"], by = lluvia3["fechas"], sum)

# Si queremos hallar promedio semanal en una temporalidad (semana) #

lluvia_w = lluvia3 %>% group_by(municipios, ubicacion, week = week(fecha)) %>% 
  dplyr::summarise(value = sum(muestra))

prom_lluvia_w = lluvia_w %>% 
  group_by(municipios, ubicacion) %>% 
  dplyr::summarise(value = mean(value))

# Sumamos valores por día y municipio
lluvia4 = lluvia3 %>%
  group_by(muniCodi, day = day(fecha)) %>%
  summarise(value = sum(muestra))

# Contar días que no ha llovido

lluvia5 = lluvia4 %>%
  filter(value == 0) %>%
  group_by(muniCodi) %>%
  count(muniCodi)

lluvia_final <- merge(lluvia2[c(1)], lluvia5, by = "muniCodi",
                      all.x = T)

lluvia_final$n <-  ifelse(is.na(lluvia_final$n),
                    0,
                    lluvia_final$n)

write.table(lluvia_final,"no_lluvia.txt", sep = ";", 
            col.names = c("municipios", "dias"),
            row.names = F)

##############################################################################


