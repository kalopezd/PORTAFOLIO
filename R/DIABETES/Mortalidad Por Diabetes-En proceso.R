#Importamos librerías
rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
DB2020 <- read_csv("data/BD2020.csv")
DB2021 <- read_csv("data/BD2021.csv")
DB2022 <- read_csv("data/BD2022.csv")
DB2023 <- read_csv("data/BD2023.csv")
DB2024 <- read_csv("data/BD2024.csv")

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
#un descenso evidente del numero de muerte por dabete en el año 2023, co reepcto a los años tres años anteriores

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
