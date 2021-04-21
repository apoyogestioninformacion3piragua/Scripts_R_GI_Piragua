library(tidyverse)
library(dplyr)
library(plyr)
library(ggthemes)
library(lubridate)
library(gridExtra)

#save(Vegachí_HB, file = "vega2.Rda")
load("vega2.Rda")
Vegachí_HB$FECHA <- as.Date(Vegachí_HB$FECHA)
Vegachí_HB$Fuente[Vegachí_HB$Fuente == "Q.El Placer(La Julia)"] <- "Q. El Placer"
Vegachí_HB$Fuente[Vegachí_HB$Fuente == "Q.El Horizonte"] <- "Q. El Horizonte"
Vegachí_HB$Fuente <- as.factor(Vegachí_HB$Fuente)
levels(Vegachí_HB$Fuente)

vega_bmwp <-  Vegachí_HB %>% 
  group_by(FECHA,AÑO, Campaña, Fuente) %>% 
  dplyr::summarise(BMWP_fuente = sum(BMWP))

max_mbwp <-  round_any(max(vega_bmwp$BMWP_fuente), 10)

p <- ggplot(data = vega_bmwp, aes(x = FECHA, y = BMWP_fuente)) +
  geom_rect(aes(ymin = 0, ymax = 20, 
                xmin = as.Date(min(FECHA),"%Y-%m-%d"), 
                xmax = as.Date(max(FECHA),"%Y-%m-%d"), fill = 'Muy crítica'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 20, ymax = 45, 
                xmin = as.Date(min(FECHA),"%Y-%m-%d"), 
                xmax = as.Date(max(FECHA),"%Y-%m-%d"), fill = 'Crítica',
                ),
            alpha = 0.5) +
  geom_rect(aes(ymin = 45, ymax = 70, 
                xmin = as.Date(min(FECHA),"%Y-%m-%d"), 
                xmax = as.Date(max(FECHA),"%Y-%m-%d"), fill = 'Dudosa'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 70, ymax = 122, 
                xmin = as.Date(min(FECHA),"%Y-%m-%d"), 
                xmax = as.Date(max(FECHA),"%Y-%m-%d"), fill = 'Aceptable'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 122, ymax = max(vega_bmwp$BMWP_fuente), 
                xmin = as.Date(min(FECHA),"%Y-%m-%d"), 
                xmax = as.Date(max(FECHA),"%Y-%m-%d"), fill = 'Buena'),
            alpha = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("2019-01-01")),
             linetype = 4, colour = "black") +
  geom_hline(yintercept=20, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=45, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=70, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=122, linetype="dashed", 
             color = "black", size=0.3) +
  scale_fill_manual(name="BMWP/Col",
                    breaks = c("Muy crítica", "Crítica", 
                               "Dudosa", "Aceptable",
                               "Buena"),
                    values= c("red", "orange",
                              "yellow","green",
                              "blue")) +
  coord_cartesian(ylim = c(0, max_mbwp)) +
  labs(x = "Fechas de registro", y = "BMWP/Col", 
       color = "Legend",
       caption = "Gráficos construidos con datos de Piragua-Corantioquia*") +
  ggtitle(label = "Calidad hidrobiológica: Fuentes abastecedoras Vegachí (Zenufaná)",
          subtitle = "Registro histórico del Índice BMWP/Col para diferentes fuentes abastecedoras monitoreadas*") +
  geom_line(linetype = "dotdash", size = 1) +
  geom_point(size = 4) +
  theme_stata() +
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5,
                                                                  color = "#0174a5"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "#0174a5"),
        plot.caption = element_text(hjust = 0.5,
                                    color = "#0174a5"),
        panel.grid.major = element_line(colour = "gray",
                                        linetype = "dashed",
                                        size = 0.3),
        axis.title = element_text(), 
        axis.title.x = element_blank(),
        strip.text = element_text(),
        strip.background =element_rect(fill="#addff4"),
        legend.text = element_text(color = "#0174a5"),
        legend.title = element_text(color = "#0174a5")) +
  scale_x_date(limits = c(min(vega_bmwp$FECHA), max(vega_bmwp$FECHA)),
               date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(~ Fuente, nrow = 2, scales = "free")
  

p 

#####################################################################################################

lluvia2 = lluvia
lluvia_d <-  lluvia2 %>% 
  group_by(day = floor_date(fechas, "day")) %>% 
  dplyr::summarise(acum = sum(muestra))

lluvia2$fechas <- ymd_hms(lluvia2$fechas, tz = "America/Bogota")

lluvia_m <- lluvia2 %>% 
  group_by(month = floor_date(fechas, "month")) %>% 
  dplyr::summarise(acum = sum(muestra))

lluvia_d$day <- as.Date(lluvia_d$day)
lluvia_m$month <- as.Date(lluvia_m$month)

lluvia_d$year <- as.factor(format(lluvia_d$day, "%Y"))
lluvia_m$year <- as.factor(format(lluvia_m$month, "%Y"))

p1 <- ggplot(data = lluvia_d, aes(x = day, y = acum)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_stata() +
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "gray",
                                        linetype = "dashed"))
p2 <- ggplot(data = lluvia_m, aes(x = month, y = acum)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_stata()
grid.arrange(p1, p2, nrow = 2)

#################################################################################

library(readxl)
vega_fq <- read_excel("BD_FQ_2.xlsx", col_types = c("numeric", 
                                                    "text", "text", "text", "text", "text", 
                                                    "date", "skip", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "text"))

vega_fq$Fecha <- as.Date(vega_fq$Fecha)

vega_fq$Fuente <- as.factor(vega_fq$Fuente)

vega_fq2 <- vega_fq %>% 
  filter(Fuente %in% c("Q. La Gallinera", "Q.El Horizonte",
                       "Q.El Placer(La Julia)",
                       "Q.Alto de la Puerta"))

p <- ggplot(data = vega_fq2, aes(x = Fecha, y = ICA)) +
  geom_rect(aes(ymin = 0, ymax = 25, 
                xmin = as.Date(min(Fecha),"%Y-%m-%d"), 
                xmax = as.Date(max(Fecha),"%Y-%m-%d"), fill = 'Muy mala'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 25, ymax = 50, 
                xmin = as.Date(min(Fecha),"%Y-%m-%d"), 
                xmax = as.Date(max(Fecha),"%Y-%m-%d"), fill = 'Mala'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 50, ymax = 70, 
                xmin = as.Date(min(Fecha),"%Y-%m-%d"), 
                xmax = as.Date(max(Fecha),"%Y-%m-%d"), fill = 'Medio'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 70, ymax = 90, 
                xmin = as.Date(min(Fecha),"%Y-%m-%d"), 
                xmax = as.Date(max(Fecha),"%Y-%m-%d"), fill = 'Buena'),
            alpha = 0.5) +
  geom_rect(aes(ymin = 90, ymax = 100, 
                xmin = as.Date(min(Fecha),"%Y-%m-%d"), 
                xmax = as.Date(max(Fecha),"%Y-%m-%d"), fill = 'Excelente'),
            alpha = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("2019-01-01")),
             linetype = 4, colour = "black") +
  geom_hline(yintercept=25, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=90, linetype="dashed", 
             color = "black", size=0.3) +
  geom_hline(yintercept=100, linetype="dashed", 
             color = "black", size=0.3) +
  scale_fill_manual(name="ICA",
                    breaks = c("Muy mala", "Mala", 
                               "Medio", "Buena",
                               "Excelente"),
                    values= c("red", "orange",
                              "yellow","green",
                              "blue")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Fechas de registro", y = "ICA-NFS QWI", 
       color = "Legend",
       caption = "Gráficos construidos con datos de Piragua-Corantioquia*") +
  ggtitle(label = "Calidad fisicoquímica: Fuentes abastecedoras Vegachí (Zenufaná)",
          subtitle = "Registro histórico del ICA-NFA QWI para diferentes fuentes abastecedoras monitoreadas*") +
  geom_line(linetype = "dotdash", size = 0.5) +
  geom_point(size = 1) +
  theme_stata() +
  theme(text = element_text(size = 9), 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "gray",
                                        linetype = "dashed",
                                        size = 0.3),
        axis.title = element_text(), 
        axis.title.x = element_blank(),
        strip.text = element_text(),
        strip.background =element_rect(fill="#addff4"),
        legend.text = element_text(),
        legend.title = element_text()) +
  scale_x_date(limits = c(min(vega_fq2$Fecha), max(vega_fq2$Fecha)),
               date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(~ Fuente, nrow = 2, scales = "free")

p
