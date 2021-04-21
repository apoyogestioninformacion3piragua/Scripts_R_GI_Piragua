library(tidyverse)
library(urltools)
library(lubridate)
library(dplyr)
library(ggplot2)

# Limpiamos el global enviroment
rm(list = ls())

# Definimos temporalidad para extracción de datos

comenzar = "2021-04-04"
finalizar = "2021-04-11"

# Leemos estaciones

estaciones = read.csv("http://www.piraguacorantioquia.com.co/redes_nuevo/images/Estaciones_Piragua.csv")
estaciones$muniCodi = as.character(paste(estaciones$municipio, estaciones$codigo))

# Filtramos por tipo de estación 

estaciones_limni = estaciones %>%
  filter(tipo == "Limnígrafo", municipio %in% c("Sabaneta", "Caracolí",
                                                "Zaragoza"))
#estaciones_limni$municipio = make.names(estaciones_limni[,4], unique = TRUE)

# Funcion para obtener dataset de estación limnigráfica

get_data_limni = function(estacion, inicio, final){
  url="https://www.piraguacorantioquia.com.co/api/v1/nivel/1017?date_estacion__gte=2020-01-01&date_estacion__lt=2020-01-02&downloadfile"
  url2 = param_set(url, "date_estacion__gte", inicio)
  url3 = param_set(url2, "date_estacion__lt", final)
  data = read.csv(gsub("1017", estacion, url3))
  data = na.omit(data)
  data = data[!(data$fecha == ""), ] # Para borrar campos de fechas vacíos
  data$fecha = as.POSIXct(data$fecha, "%Y-%m-%d %H-%M")
  data2 = data
  #data$fechas = floor_date(data$fechas, "5 mins")
  #limnidata2 = data %>%
    #group_by(fechas) %>%
    #dplyr::summarise(nivel = mean(nivel)) #Cambiar muestra, nivel o caudal
}


# Iteramos sobre las estaciones limnigráficas
  
limni = list() # Creamos una lista vacía
  
# Guardamos en lista de listas todos los datasets de las estaciones 
# para una territorial en particular
  
for (i in 1:nrow(estaciones_limni)) {
    
  estacion = estaciones_limni$codigo[i]
  limni[[i]] = get_data_limni(estacion, comenzar, finalizar)
    
  }
  
# Agregamos una columna de municipios a cada dataframe de la lista
  
for (i in 1:length(limni)) {
    
  if (nrow(limni[[i]]) > 0) {
    
      limni[[i]] = data.frame(limni[[i]], 
                              territorial = estaciones_limni[i,5],
                              municipios = estaciones_limni[i,4],
                              muniCodi = estaciones_limni[i,17],
                              fuente = estaciones_limni[i,7])
  }
}

#Para eliminar listas con 0 filas
limni = limni[sapply(limni, nrow) > 0]

#Creamos un dataframe global con la lista de estaciones que tienen datos
limni = do.call(rbind, limni)

limni2 <- limni
limni2$calidad[limni2$calidad == "No data"] <- NA
limni2$nivel[limni2$nivel == -999] <- NA
limni2$nivel[limni2$nivel <= 0] <- NA
limni2 <- na.omit(limni2)

#Agrupamos por municipios y hallamos su promedio
limni_prom = limni2 %>%
  group_by(territorial, muniCodi) %>%
  dplyr::summarise(medio = mean(nivel)) # Nivel, caudal o muestra

#Agrupamos por municipios y hallamos su máximo
limni_max = limni2 %>%
  group_by(territorial, muniCodi) %>%
  dplyr::summarise(máximo = max(nivel)) # Nivel, caudal o muestra

#Agrupamos por municipios y hallamos su mínimo
limni_min = limni2 %>%
  group_by(territorial, muniCodi) %>%
  dplyr::summarise(mínimo = min(nivel)) # Nivel, caudal o muestra

# Creamos dataset con resumen
limni_resumen = data.frame(limni_prom, limni_max$máximo,
                           limni_min$mínimo)
colnames(limni_resumen) = c("Territorial", "Estación", "Promedio",
                            "Máximo", "Mínimo")



# Exportamos datos
write.table(limni_resumen,"limni.txt", sep = ";",
            row.names = F)

##################################################################################

save(limni, file = "limni.Rdata", compress = "xz")
