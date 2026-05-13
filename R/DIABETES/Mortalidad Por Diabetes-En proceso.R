#Importamos librerías
rm(list=ls())
#install.packages("sf")
#install.packages("geodata")

library(sf)
library(geodata)
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)

library(tidyr)
DB2020 <- read_csv("data/BD2020.csv")
DB2021 <- read_csv("data/BD2021.csv")
DB2022 <- read_csv("data/BD2022.csv")
DB2023 <- read_csv("data/BD2023.csv")
DB2024 <- read_csv("data/BD2024.csv")

POB <- read_excel("data/POB.xlsx", sheet = "POB")



#--------------------------LIMPIEZA DE LAS BASES DE DATOS---------------------------------#
#Revisamos las variables colnames(DB2022)...
#Variables de interés
#"COD_DPTO" "COD_MUNIC"  "SEXO"  "EST_CIVIL" "GRU_ED2" "NIVEL_EDU" "OCUPACION"   C_BAS1(según la CIE10)
#Filtramos por causa de muerte (diabetes Tipo I (E10..), diabetes tipo II (E11..)), Diabetes no especificada (E14) y seleccionamos por variables de interés.

DB2020f <- DB2020%>%filter(grepl("^E10|^E11|^E14", C_BAS1))%>%#^indica que quiero filtar por codigos que comiencen con E1, E11, E14 segun el caso. 
  
  select(COD_DPTO, COD_MUNIC, SEXO, EST_CIVIL,GRU_ED2,NIVEL_EDU,C_BAS1)%>%
  mutate(ANIO=as.character(2020))

DB2021f <- DB2021%>%filter(grepl("^E10|^E11|^E14", C_BAS1))%>%#^indica que quiero filtar por codigos que comiencen con E1, E11, E14 segun el caso. 
  
  select(COD_DPTO, COD_MUNIC, SEXO, EST_CIVIL,GRU_ED2,NIVEL_EDU,C_BAS1)%>%
  mutate(ANIO=as.character(2021))

DB2022f <- DB2022%>%filter(grepl("^E10|^E11|^E14", C_BAS1))%>%#^indica que quiero filtar por codigos que comiencen con E1, E11, E14 segun el caso. 
  
  select(COD_DPTO, COD_MUNIC, SEXO, EST_CIVIL,GRU_ED2,NIVEL_EDU,C_BAS1)%>%
  mutate(ANIO=as.character(2022))#USAMOS MUTATE PARA AGREGAR LA COLUMNA AÑO y nos aseguramos que esté en formato caractér
DB2023f <- DB2023%>%filter(grepl("^E10|^E11|^E14", C_BAS1))%>%
  select(COD_DPTO, COD_MUNIC, SEXO, EST_CIVIL,GRU_ED2,NIVEL_EDU,C_BAS1)%>%
  mutate(ANIO=as.character(2023))
DB2024f <- DB2024%>%filter(grepl("^E10|^E11|^E14", C_BAS1))%>%
  select(COD_DPTO, COD_MUNIC, SEXO, EST_CIVIL,GRU_ED2,NIVEL_EDU,C_BAS1)%>%
mutate(ANIO=as.character(2024))

database <- rbind(DB2020f,DB2021f,DB2022f,DB2023f,DB2024f)#Unimos los datos que tenemos para  cada año
#Verificamos la integridad de los registros
#head(database)
#str(database)
#summary(database)

#Podemos observar que las variables sexo y estado civil estan en formato numerico, pero estan son variables categoricas
#Entonces, debemos cambiar el formato de dichas variables para análisis posteriores.Asi mismo, otras variables como 
#nivel educativo y edad estan en formato caracter, pero conviene transformarlas como  factor. 

database <- database %>%
  mutate(
    SEXO = factor(SEXO, levels = c(1, 2, 3), labels = c("Masculino", "Femenino", "Indeterminado")),
    EST_CIVIL = factor(EST_CIVIL, levels = c(1, 2, 3, 4, 5, 6, 9),
                       labels = c("Unión Libre más de dos", "Unión Libre menos de dos", 
                                  "Separado/Divorciado", 
                                  "Viudo", "Soltero", "Casado", "Sin información")),
    
    NIVEL_EDU = factor(NIVEL_EDU, 
                       levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "99"), 
                       labels = c("Preescolar", "Básica primaria", "Básica secundaria", 
                                  "Media académica o clásica", "Media técnica", "Normalista", 
                                  "Técnica profesional", "Tecnológica", "Profesional", 
                                  "Especialización", "Maestría", "Doctorado", "Ninguno", "Sin información")),
    GRU_ED2 = factor(GRU_ED2, 
                     levels = c("01", "02", "03", "04", "05", "06", "07"), 
                     labels = c("Menor de 1 año", 
                                "De 1 a 4 años", 
                                "De 5 a 14 años", 
                                "De 15 a 44 años", 
                                "De 45 a 64 años", 
                                "De 65 y más años", 
                                "Edad desconocida"))
  )

#Ahora, podemos ver que hay variables con muchas categorías. En ese caso convierne agrupar ciertas categorías.

database <- database%>%
  mutate(
    
    EST_CIVIL_AGRUPADO=case_when(
      EST_CIVIL%in%c( "Unión Libre más de dos", "Unión Libre menos de dos")~"Unión Libre",
      EST_CIVIL==  "Separado/Divorciado"~"Separado(a)/Divorciado(a)", 
      
      EST_CIVIL=="Viudo"~"Viudo(a)",
      EST_CIVIL=="Soltero"~"Soltero(a)",
      EST_CIVIL=="Casado"~ "Casado(a)", 
      EST_CIVIL=="Sin información"~"Sin información"
    ),
      NIVEL_EDU_AGRUPADO = case_when(
      NIVEL_EDU %in% c("Preescolar", "Básica primaria", "Básica secundaria") ~ "Básico",   # Agrupar niveles básicos
      NIVEL_EDU %in% c("Media académica o clásica", "Media técnica", "Normalista") ~ "Medio", # Agrupar niveles medios
      NIVEL_EDU %in% c("Técnica profesional", "Tecnológica", "Profesional", "Especialización", "Maestría", "Doctorado") ~ "Superior", # Agrupar niveles superiores
      NIVEL_EDU == "Ninguno" ~ "Sin educación",   # Sin educación
      NIVEL_EDU == "Sin información" ~ "Sin información"  # Sin información
    ),
    GRU_ED2_AGRUPADO = case_when(
      GRU_ED2 %in% c("Menor de 1 año", "De 1 a 4 años", "De 5 a 14 años") ~ "Menor de 15 años",  # Niños y adolescentes
      GRU_ED2 %in% c("De 15 a 44 años") ~ "De 15 a 44 años",  # Adultos jóvenes
      GRU_ED2 %in% c("De 45 a 64 años") ~ "De 45 a 64 años",  # Adultos mayores
      GRU_ED2 %in% c("De 65 y más años") ~ "De 65 y más años",  # Adultos mayores mayores
      GRU_ED2 == "Edad desconocida" ~ "Edad desconocida"  # Sin información de edad
    ),
    TIPO_DIABETES = case_when(
      grepl("^E10", C_BAS1) ~ "Diabetes tipo 1",
      grepl("^E11", C_BAS1) ~ "Diabetes tipo 2",
      grepl("^E14", C_BAS1) ~ "Diabetes no especificada",
      TRUE ~ "Otra"
    )
    
    
  )

#head(database) Verificamos registros

#--------------------------ANALISIS DESCRIPTIVO---------------------------------#

#Evolución de número de muertes por anio a causa de diabetes en Colombia

muertes_por_anio <- database%>%
  group_by(ANIO)%>%
  summarise(NUM_MUERTES =n())
#Creamos un gráfico para ver la evolucion

graf_muertes_anio<-ggplot(muertes_por_anio,aes(x=ANIO,y=NUM_MUERTES ,group=1))+
  geom_line(color="steelblue",linewidth=1.2)+
  geom_point(color="red",size=3)+
  labs(title="Evolucion de muertes por diabetes por año en Colombia",
       x="Año",
       y="Número de muertes")+
  theme_minimal()

#En anterior gráfico es un primer acercamiento al número de muerte por diabetes según el año. Podemos ver que hubo 
#un descenso evidente del numero de muerte por dabete en el año 2023, con respecto a los años tres años anteriores

muertes_anio_tipo <-  database %>%
  group_by(ANIO, TIPO_DIABETES) %>%
  summarise(CASOS = n(), .groups = "drop")#porcentaje

muertes_anio_tipo$TIPO_DIABETES <- factor(
  muertes_anio_tipo$TIPO_DIABETES,
  levels = c("Diabetes no especificada", "Diabetes tipo 2", "Diabetes tipo 1")
)


graf_muertes_anio_tipo <- ggplot(muertes_anio_tipo, aes(x = ANIO, y = CASOS, fill = TIPO_DIABETES)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de muertes por diabetes según tipo",
       x = "Año",
       y = "Número de casos",
       fill = "Tipo de diabetes") +
  scale_fill_manual(values = c(
    "Diabetes tipo 1" = "#800000",
    "Diabetes tipo 2" = "#FF6666",
    "Diabetes no especificada" = "#FFC0CB"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


defunciones_clean <- database %>%
  filter(
    TIPO_DIABETES != "Otra",
    GRU_ED2_AGRUPADO != "Edad desconocida",
    SEXO != "Indeterminado"
  )


#Calculo de Tasa de Mortalidad por año , sexo y tipo

poblacion_clean <-POB%>%
  rename(
    
  #Renombramos ciertas coloumnas de POB
    POB_H   = `TOTAL H`,
    POB_M   = `TOTAL M`
  ) %>%
  filter(AREA == "Total") %>%          # solo fila Total
  pivot_longer(
    cols      = c(POB_H, POB_M), #Pasamos el dataset PB a formato largo 
    names_to  = "SEXO",
    values_to = "POBLACION"
  ) %>%
  mutate(
    SEXO = recode(SEXO,
                  "POB_H" = "Masculino",
                  "POB_M" = "Femenino")
  ) %>%
  select( COD_DPTO ,DEPNOM  , ANIO, SEXO, POBLACION)
#sleccionamso columnas de interes


# Población nacional por año y sexo
poblacion_nacional <- poblacion_clean %>%
  group_by(ANIO, SEXO) %>%
  summarise(POBLACION = sum(POBLACION, na.rm = TRUE), .groups = "drop")

# Población DPTO por año y sexo
poblacion_dpto <- poblacion_clean %>%
  group_by(COD_DPTO, ANIO, SEXO) %>%
  summarise(POBLACION = sum(POBLACION, na.rm = TRUE), .groups = "drop")


# Elegimos una POBLACIÓN ESTÁNDAR
# En este caso la del año más reciente 
# =============================================================================

ANIO_ESTANDAR <- max(poblacion_nacional$ANIO)   # cambia a un año fijo si prefieres: 2022

poblacion_estandar <- poblacion_nacional %>%
  filter(ANIO == ANIO_ESTANDAR) %>%
  select(SEXO, POB_ESTANDAR = POBLACION)

# TASAS BRUTAS
# Fórmula: (Defunciones / Población) × 100.000
# Informe  < 5 defunciones 
# =============================================================================

calcular_tasa_bruta <- function(df_def, df_pob, vars_grupo) {
  df_def %>%
    group_by(across(all_of(vars_grupo))) %>%#vars_grupo son un vector de variables de agupamiento
    summarise(DEFUNCIONES = n(), .groups = "drop") %>%
    left_join(df_pob, by = intersect(vars_grupo, names(df_pob))) %>%#Junto dataset de defunciones y poblacion segun variables de agrupamiento
    mutate(
      TASA_BRUTA = round((DEFUNCIONES / POBLACION) * 100000, 2),
      CONFIABLE  = if_else(DEFUNCIONES >= 5, "Sí", "No (<5 def.)"),
      NOTA       = if_else(DEFUNCIONES < 5,
                           "Tasa basada en <5 defunciones — interpretar con precaución",
                           NA_character_)
    )
}

#me aseguro que ANIO ESTE EN FORMATO ENTERO PARA EVITAR PROBLEMAS DE UNION

defunciones_clean <- defunciones_clean %>%
  mutate(ANIO = as.integer(ANIO))

poblacion_clean <- poblacion_clean %>%
  mutate(ANIO = as.integer(ANIO))

#  Tasa Nacional por año + sexo + tipo 
tb_nac_tipo  <- calcular_tasa_bruta(defunciones_clean, poblacion_nacional,
                                    c("ANIO", "SEXO", "TIPO_DIABETES"))

tb_nac_total <- calcular_tasa_bruta(
  defunciones_clean %>% mutate(TIPO_DIABETES = "Diabetes total"),
  poblacion_nacional, c("ANIO", "SEXO", "TIPO_DIABETES")
)

tasa_bruta_nacional <- bind_rows(tb_nac_tipo, tb_nac_total)


# Tasa Departamental + año + sexo (diabetes total) ---
tb_dpto_total <- calcular_tasa_bruta(
  defunciones_clean %>% mutate(TIPO_DIABETES = "Diabetes total"),
  poblacion_dpto, c("COD_DPTO", "ANIO", "SEXO", "TIPO_DIABETES")
)

# Deapartamento por tipo 
tb_dpto_tipo  <- calcular_tasa_bruta(defunciones_clean, poblacion_dpto,
                                    c("COD_DPTO", "ANIO", "SEXO", "TIPO_DIABETES"))

tasa_bruta_dpto <- bind_rows(tb_dpto_total, tb_dpto_tipo)


#Usamos ahora el mpetodo ditecto apra etandariza las tasas de mortalida por diabtes

# Fórmula: Tasa_std = Σ(tasa_edad_i × pob_estándar_i) / Σ(pob_estándar_i)
# Estrato de estandarización: GRU_ED2_AGRUPADO × SEXO
# =============================================================================

estandarizar_directa <- function(df_def, df_pob, vars_grupo) {
  df_def %>%
    group_by(across(all_of(c(vars_grupo, "SEXO", "GRU_ED2_AGRUPADO")))) %>%
    summarise(DEFUNCIONES = n(), .groups = "drop") %>%
    left_join(df_pob, by = c(setdiff(vars_grupo, "TIPO_DIABETES"), "SEXO")) %>%
    mutate(TASA_EDAD = DEFUNCIONES / POBLACION) %>%
    left_join(poblacion_estandar, by = "SEXO") %>%
    group_by(across(all_of(c(vars_grupo, "SEXO")))) %>%
    summarise(
      TASA_ESTAND = round(
        sum(TASA_EDAD * POB_ESTANDAR, na.rm = TRUE) /
          sum(POB_ESTANDAR, na.rm = TRUE) * 100000, 2),
      DEFUNCIONES = sum(DEFUNCIONES, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      CONFIABLE = if_else(DEFUNCIONES >= 5, "Sí", "No (<5 def.)"),
      NOTA      = if_else(DEFUNCIONES < 5,
                          "Tasa basada en <5 defunciones — interpretar con precaución",
                          NA_character_)
    )
}

# Nacional estandarizada por año + sexo + tipo 
std_nac_tipo <- estandarizar_directa(defunciones_clean, poblacion_nacional,
                                     c("ANIO", "TIPO_DIABETES"))

std_nac_total <- estandarizar_directa(
  defunciones_clean %>% mutate(TIPO_DIABETES = "Diabetes total"),
  poblacion_nacional, c("ANIO", "TIPO_DIABETES")
)

tasa_std_nacional <- bind_rows(std_nac_tipo, std_nac_total)

# DPTO estandarizada por región + año + sexo (diabetes total)
tasa_std_dpto <- estandarizar_directa(defunciones_clean, poblacion_dpto,
                                          c("COD_DPTO", "ANIO"))

#TABLAS FINALES

# Nacional bruta + estandarizada
resumen_nacional <- tasa_bruta_nacional %>%
  select(ANIO, SEXO, TIPO_DIABETES, DEFUNCIONES, POBLACION, TASA_BRUTA, CONFIABLE) %>%
  left_join(
    tasa_std_nacional %>% select(ANIO, SEXO, TIPO_DIABETES, TASA_ESTAND),
    by = c("ANIO", "SEXO", "TIPO_DIABETES")
  ) %>%
  arrange(ANIO, SEXO, TIPO_DIABETES)

# Departamental  bruta + estandarizada (diabetes total)
resumen_dpto <- tasa_bruta_dpto %>%
  filter(TIPO_DIABETES == "Diabetes total") %>%
  select(COD_DPTO, ANIO, SEXO, DEFUNCIONES, POBLACION, TASA_BRUTA, CONFIABLE) %>%
  left_join(
    tasa_std_dpto %>% select(COD_DPTO, ANIO, SEXO, TASA_ESTAND),
    by = c("COD_DPTO", "ANIO", "SEXO")
  ) %>%
  arrange(ANIO, COD_DPTO, SEXO)

# Regional por tipo (solo tasa bruta)
resumen_dpto_tipo <- tasa_bruta_dpto %>%
  filter(TIPO_DIABETES != "Diabetes total") %>%
  select(COD_DPTO, ANIO, SEXO, TIPO_DIABETES, DEFUNCIONES, TASA_BRUTA, CONFIABLE) %>%
  arrange(ANIO, COD_DPTO, SEXO, TIPO_DIABETES)

resumen_nacional %>%
  filter(TIPO_DIABETES != "Diabetes total", CONFIABLE == "Sí") %>%
  ggplot(aes(x = ANIO, y = TASA_ESTAND, color = SEXO, linetype = TIPO_DIABETES)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Masculino" = "#2196F3", "Femenino" = "#E91E63")) +
  labs(
    title    = "Tasa de mortalidad estandarizada — diabetes mellitus",
    subtitle = paste("Colombia nacional | Estándar:", ANIO_ESTANDAR),
    x = "Año", y = "Tasa × 100.000 hab.",
    color = "Sexo", linetype = "Tipo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


resumen_dpto %>%
  filter(!is.na(TASA_ESTAND)) %>%
  ggplot(aes(x = ANIO, y = reorder(COD_DPTO, TASA_ESTAND), fill = TASA_ESTAND)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#FFF9C4", high = "#B71C1C") +
  facet_wrap(~SEXO) +
  labs(
    title = "Tasa estandarizada de mortalidad por diabetes — por región y año",
    x = "Año", y = "Región", fill = "Tasa × 100K"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8))


# Descarga el shapefile de Colombia directo desde GADM
colombia <- gadm(country = "COL", level = 1, path = tempdir()) %>%
  st_as_sf()


# Extraer las 2 letras después de "CO." y mapear a código DANE
colombia <- colombia %>%
  mutate(
    HASC_CODE = str_extract(HASC_1, "(?<=CO\\.).*"),  # extrae "AM", "AN", etc.
    COD_DPTO = case_when(
      HASC_CODE == "AM" ~ "91",  # Amazonas
      HASC_CODE == "AN" ~ "05",  # Antioquia
      HASC_CODE == "AR" ~ "81",  # Arauca
      HASC_CODE == "AT" ~ "08",  # Atlántico
      HASC_CODE == "DC" ~ "11",  # Bogotá
      HASC_CODE == "BL" ~ "13",  # Bolívar
      HASC_CODE == "BY" ~ "15",  # Boyacá
      HASC_CODE == "CL" ~ "17",  # Caldas
      HASC_CODE == "CQ" ~ "18",  # Caquetá
      HASC_CODE == "CS" ~ "85",  # Casanare
      HASC_CODE == "CA" ~ "19",  # Cauca
      HASC_CODE == "CE" ~ "20",  # Cesar
      HASC_CODE == "CH" ~ "27",  # Chocó
      HASC_CODE == "CO" ~ "23",  # Córdoba
      HASC_CODE == "CU" ~ "25",  # Cundinamarca
      HASC_CODE == "GN" ~ "94",  # Guainía
      HASC_CODE == "GV" ~ "95",  # Guaviare
      HASC_CODE == "HU" ~ "41",  # Huila
      HASC_CODE == "LG" ~ "44",  # La Guajira
      HASC_CODE == "MA" ~ "47",  # Magdalena
      HASC_CODE == "ME" ~ "50",  # Meta
      HASC_CODE == "NA" ~ "52",  # Nariño
      HASC_CODE == "NS" ~ "54",  # Norte de Santander
      HASC_CODE == "PU" ~ "86",  # Putumayo
      HASC_CODE == "QD" ~ "63",  # Quindío
      HASC_CODE == "RI" ~ "66",  # Risaralda
      HASC_CODE == "SA" ~ "88",  # San Andrés
      HASC_CODE == "ST" ~ "68",  # Santander
      HASC_CODE == "SU" ~ "70",  # Sucre
      HASC_CODE == "TO" ~ "73",  # Tolima
      HASC_CODE == "VC" ~ "76",  # Valle del Cauca
      HASC_CODE == "VP" ~ "97",  # Vaupés
      HASC_CODE == "VD" ~ "99"   # Vichada
    )
  )

# Verificar que quedó bien
colombia %>% st_drop_geometry() %>% select(NAME_1, HASC_CODE, COD_DPTO)


# Función para generar el mapa
mapa_diabetes <- function(anio, sexo) {
  
  colombia %>%
    left_join(
      resumen_dpto %>% filter(ANIO == anio, SEXO == sexo),
      by = "COD_DPTO"
    ) %>%
    ggplot() +
    geom_sf(aes(fill = TASA_ESTAND), color = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low      = "#FFF9C4",
      high     = "#B71C1C",
      na.value = "grey80",
      name     = "Tasa × 100K"
    ) +
    labs(
      title    = "Tasa estandarizada de mortalidad por diabetes mellitus",
      subtitle = paste0(sexo, " — ", anio),
      caption  = "Fuente: DANE. Estandarización directa. Gris = dato no disponible."
    ) +
    theme_void(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

# Ejemplos de uso
mapa_diabetes(2020, "Femenino")
mapa_diabetes(2020, "Masculino")
mapa_diabetes(2021, "Femenino")

