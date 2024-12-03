##### Exploración y Consolidación de una base
library(pacman)

pacman:: p_load(tidyverse, ggplot2, dplyr, corrplot, skimr)

Votos<- read.csv("//Users/Abel/Documents/Abel/Colmex/9no/Tesis/Tesis/Datos/VotosCompletoLXV.csv", header = TRUE)

summary(Votos)
str(Votos)

#Crear una base con sólo las sumas de indisciplina, inasistencia y licencia (variables dependientes de los modelos)

VotosCont<- Votos %>% 
  mutate(indisciplina = rowSums(Votos == 1), .keep = "all") %>% 
  mutate(inasistencia = rowSums(Votos == 3), .keep = "all") %>% 
  mutate(licencia = rowSums(Votos == 33), .keep = "all")


## Valores de las variables dependoentes
valores_asis<- unique(VotosCont$inasistencia)
valores_ind<- unique(VotosCont$indisciplina)
valores_lic<- unique(VotosCont$licencia)

frecuencia_asis<- table(VotosCont$inasistencia)
frecuencia_asis

frecuencia_ind<- table(VotosCont$indisciplina)
frecuencia_ind

frecuencia_lic<- table(VotosCont$licencia)
frecuencia_lic

# Base de datos final de variables independientes
Votos_Final<- VotosCont %>% 
  select(code, indisciplina, inasistencia, licencia)

## Unir la base de datos de los diputados con la de los votos

Diputados<- read.csv("/Users/Abel/Documents/Abel/Colmex/9no/Tesis/Tesis/Datos/DipCompletoLXV.csv", header = TRUE)

DiputadosFin<- full_join(Diputados, Votos_Final, by = "code")

##Por alguna razón la primera columna de las independientes se pone en NA, porque se ponen en una fila al final
sum(is.na(DiputadosFin))

DiputadosFin <- head(DiputadosFin, -1)

#Como solo en mor1 hay NA, podemos sustituirlos todo por su valor real que es 0
DiputadosFin[is.na(DiputadosFin)] <- 0

#También sustituyo un error que tuve de codificación
DiputadosFin[150, 7] <- 1
DiputadosFin[36, 8] <- 3

#Otros errores
DiputadosFin <- DiputadosFin %>%
  mutate(exp_admin = if_else(exp_admin == 9, 0, exp_admin),
         asoc = if_else(asoc == 9, 0, asoc),
         dirigencia = if_else(dirigencia == 9, 0, asoc),
         directiva = if_else(directiva == 9,0, directiva))

unique(DiputadosFin$directiva)
unique(DiputadosFin$asoc)
unique(DiputadosFin$dirigencia)
unique(DiputadosFin$exp_admin)
### Ahora guardamos la base de datos
write_csv(DiputadosFin, "/Users/Abel/Documents/Abel/Colmex/9no/Tesis/Tesis/Datos/Diputados.csv")
help("write_csv")

##Tenemos que también hacer una base de datos de las iniciativas que utilizamos

Iniciativas<- read.csv("/Users/Abel/Documents/Abel/Colmex/9no/Tesis/Tesis/Datos/LXV_Iniciativa.csv")

Iniciativas<- Iniciativas %>% 
  slice(-(16:22))

#Ahora lo que tengo que hacer es contar no por diputado, sino por iniciativa, cuántas indisciplinas, asistencias y licencias hubo

Voto_Inic<- as.data.frame(t(Votos))
Voto_Inic[12, 543] <- 1
Voto_Inic <- Voto_Inic %>% slice(-1)




#Ya transformamos, ahora hacemos la suma
Voto_Inic <- Voto_Inic %>% 
  mutate(across(everything(), as.numeric))
         
         
str(Voto_Inic)

Voto_Inic <- Voto_Inic %>% 
  mutate(
    indisciplina = rowSums(select(., everything()) == 1, na.rm = TRUE),
    inasistencia = rowSums(select(., everything()) == 3, na.rm = TRUE),
    licencia = rowSums(select(., everything()) == 33, na.rm = TRUE)
  )

#Hacemos una base sólo con las tres variables y la ponemos con la de iniciativas

Votos_Inic<- Voto_Inic %>% 
  select(indisciplina, inasistencia, licencia)

Iniciativas <- Iniciativas %>% 
  bind_cols(Votos_Inic)

#Limpiémosla un poco
Iniciativas <- Iniciativas %>% select(-Num)
Iniciativas[4, 2] <- "Morena"

Iniciativas<- Iniciativas %>% 
  mutate(Pronunciamiento.AMLO = recode(Pronunciamiento.AMLO, "No" = 0,
                          "Si" = 1,
                          "Sí" = 1)) 

Iniciativas <- Iniciativas %>% 
  mutate(Pronunciamiento.AMLO = if_else(is.na(Pronunciamiento.AMLO), 1, Pronunciamiento.AMLO))

write_csv(Iniciativas, "/Users/Abel/Documents/Abel/Colmex/9no/Tesis/Tesis/Datos/Iniciativas.csv")
  




