# Load libraries #####

library(dplyr)
library(ggplot2)
library(tibble)
library(ggpmisc)
library(gt)
library(mapproj)
library(sf)
library(ggspatial)
library(forcats)
library(tidyr)
library(scatterpie)
library(stringr)
library(ggpmisc)
library(gridExtra)
library(ggpubr)

# Load data and variables #####

var_names <- readxl::read_excel("../Encuesta_de_Movilidad_en_Anaga2024-12-09_02_53_24.xlsx", sheet = "vars")

data <- readxl::read_excel("../Encuesta_de_Movilidad_en_Anaga2024-12-09_02_53_24.xlsx", sheet = "Sheet1")
names(data) <- var_names$var

# elimina cuestionarios sin respuestas
data<-data %>% filter( completa=="No, no he realizado esta encuesta previamente." )

# núcleos de Anaga y equivalencias para datos geográficos
lista_nucleos<-readxl::read_excel("../nucleos.xlsx")

# Crear variables derivadas del lugar de residencia ######

order_nucleos<-c( "Tegueste", "Pedro Álvarez", "Bajamar", 
                  "Jardina","Las Mercedes",
                  "Punta del Hidalgo","Bejía","El Batán","Chinamada", "Las Carboneras","Afur","Taborno","Catalanes","Roque Negro","Lomo de Las Bodegas","Casas de La Cumbre","Chamorga", 
                  "Taganana","Almáciga","Benijo","Igueste de San Andrés",
                  "Valle Tahodio","Cueva Bermeja","María Jiménez","Barrio de la Alegría","Valleseco","San Andrés","El Suculum",
                  "Núcleo desc. de Anaga",
                  "San Cristóbal de La Laguna",
                  "Santa Cruz de Tenerife",
                  "Tacoronte","El Sauzal" ,"La Matanza de Acentejo","La Victoria de Acentejo","Santa Úrsula","La Orotava","Puerto de la Cruz","Los Realejos","Icod de los Vinos","La Guancha","Garachico","San Juan de la Rambla","Buenavista del Norte","Los Silos", 
                  "Candelaria","El Rosario","Güímar","Arafo","Fasnia","Arico","San Miguel de Abona","Granadilla de Abona","Arona","Adeje","Guía de Isora","Santiago del Teide" ,
                  "Otra isla de Canarias",
                  "Península /Baleares" ,
                  "Reino Unido","Irlanda","Francia","Alemania","Italia",
                  "Otro" ,"NS/NC (ya hizo la encuesta, sólo sorteo, etc.)")


data<-data %>% 
  #select(REp01.01,REp01.02,REp01.03,REp01.03a,REp01.03b,REp01.03c,EXp01.04) %>% 
  mutate(
    REp01.situa_resid=case_when(
      REp01.01=="Sí" & REp01.03=="Sí"~"Reside en Anaga",
      REp01.01=="Sí" & (is.na(REp01.03) | REp01.03=="No")~"Reside en otras zonas de Tenerife", 
      #REp01.01=="No" ~ replace(EXp01.04, !( EXp01.04 %in% c("Otra isla de Canarias","Península /Baleares")), "Extranjero/Otro"),
      REp01.01=="No" ~ "No reside en Tenerife",
      .default="NS/NC (ya hizo la encuesta, sólo sorteo, etc.)"
    ),
    REp01.nucleo_resid=case_when(
      REp01.01=="Sí" & REp01.03=="Sí"~ ifelse(!is.na(REp01.03a),REp01.03a,ifelse(!is.na(REp01.03b),REp01.03b,ifelse(!is.na(REp01.03c),REp01.03c,"Núcleo desc. de Anaga"))),
      REp01.01=="Sí" & (is.na(REp01.03) | REp01.03=="No")~REp01.02, 
      REp01.01=="No" ~EXp01.04,
      .default="NS/NC (ya hizo la encuesta, sólo sorteo, etc.)"                
      
    )
  ) %>%
  mutate(REp01.nucleo_resid=ifelse(REp01.nucleo_resid %in% c("Tegueste Casco"),"Tegueste",REp01.nucleo_resid)) %>%
  mutate(REp01.lugar_resid=case_when(
    REp01.situa_resid=="Reside en Anaga" & REp01.nucleo_resid %in% c( "Tegueste", "Pedro Álvarez", "Bajamar",
                                                                      "Jardina","Las Mercedes",
                                                                      "Valle Tahodio","Cueva Bermeja","María Jiménez","Barrio de la Alegría","Valleseco","San Andrés","El Suculum"
    )~"Reside en núcleos periféricos de Anaga",
    REp01.situa_resid=="Reside en Anaga" & REp01.nucleo_resid %in% c( "Punta del Hidalgo","Bejía","El Batán","Chinamada", "Las Carboneras","Afur","Taborno","Catalanes","Roque Negro","Lomo de Las Bodegas","Casas de La Cumbre","Chamorga", 
                                                                      "Taganana","Almáciga","Benijo","Igueste de San Andrés"
    )~"Reside en núcleos interiores de Anaga",
    #REp01.situa_resid=="Reside en Anaga" & REp01.nucleo_resid=="Núcleo desc. de Anaga"~"Reside en Anaga",
    #REp01.situa_resid=="No reside en Tenerife"~replace(EXp01.04, !( EXp01.04 %in% c("Otra isla de Canarias","Península /Baleares")), "Extranjero/Otro"),
    .default=REp01.situa_resid)
  ) %>%
  mutate(REp01.situa_resid=factor(REp01.situa_resid,levels=c("Reside en Anaga","Reside en otras zonas de Tenerife",
                                                             "No reside en Tenerife",
                                                             "NS/NC (ya hizo la encuesta, sólo sorteo, etc.)")), 
         REp01.lugar_resid=factor(REp01.lugar_resid,levels=c("Reside en núcleos interiores de Anaga",
                                                             "Reside en núcleos periféricos de Anaga",
                                                             "Reside en Anaga",
                                                             "Reside en otras zonas de Tenerife",
                                                             "No reside en Tenerife",
                                                             "NS/NC (ya hizo la encuesta, sólo sorteo, etc.)")),
         REp01.nucleo_resid=factor(REp01.nucleo_resid,levels=order_nucleos)
  ) 

sfdf <- read_sf("../anaga_nucleos.geojson")

# Crear filas de sub-totales y totales para tablas #####

# Define a named list of aggregation
# functions and summary row labels
row_fun_labels <- 
  list(
    SubTotal = ~sum(., na.rm = TRUE)
  )
grand_fun_labels <- 
  list(
    TOTAL = ~sum(., na.rm = TRUE)
  )   