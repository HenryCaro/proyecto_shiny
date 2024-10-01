# Cargar Librerías --------------------------------------------------------

library(tidyverse)
library(httr)
library(rvest)
library(readxl)
library(googledrive)

# Vector 1 ----------------------------------------------------------------

archivos_POSIXct <- paste0(c("afiliados.xls",
                             "afiliados_region_edad.xls", 
                             "afiliados_region_afp.xls",
                             "afiliados_edad_afp.xls", 
                             "afiliados_tipo_sexo_afp.xls",
                             "afiliados_densidad.xls", 
                             "cotizaciones_totales.xls",
                             "cotizantes_ingreso_imponible_promedio.xls", 
                             "cotizantes_region_tramos_ingreso_imponible.xls",
                             "cotizantes_region_afp.xls" ,"cotizantes_edad_afp.xls",
                             "ingreso_imponible_promedio_region.xls", 
                             "numero_monto_promedio_uf_pensiones_pagadas.xls",
                             "numero_monto_promedio_uf_pensiones_pagadas_retiros_programados.xls",
                             "numero_monto_promedio_uf_pensiones_pagadas_rentas_vitalicias.xls",
                             "numero_monto_promedio_uf_pensiones_pagadas_rentas_temporales.xls", 
                             "numero_monto_promedio_uf_pensiones_pagadas_cubiertas_seguro.xls", 
                             "activos_fondos_pensiones.xls", 
                             "pasivos_fondos_pensiones.xls", 
                             "rentabilidad_real_mensual_fondos_deflactada_uf.xls", 
                             "serie_pilar1.xls",
                             "serie_pilar2.xls", 
                             "serie_pilar3.xls",
                             "serie_pilar4.xls", 
                             "serie_pilar5.xls",
                             "serie_pilar6.xls"), collapse = "|")

# Función 1 ---------------------------------------------------------------

descargar_archivo <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    nombre_archivo <- basename(url)
    archivo_temporal <- tempfile(fileext = ".xls")
    writeBin(content(response, "raw"), archivo_temporal)
    data <- read_excel(archivo_temporal)
    unlink(archivo_temporal)
    cat("Archivo descargado y cargado exitosamente:", nombre_archivo, "\n")
    return(data)
  } else {
    print(paste("Error al descargar el archivo:", url))
    return(NULL)
  }
}

# Función 2 ---------------------------------------------------------------

descargar_archivo_fecha <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    nombre_archivo <- basename(url)
    archivo_temporal <- tempfile(fileext = ".xls")
    writeBin(content(response, "raw"), archivo_temporal)
    data <- read_excel(archivo_temporal, skip = 1, col_types = c("date", rep("guess", ncol(read_excel(archivo_temporal)) - 1)))
    unlink(archivo_temporal)
    cat("Archivo descargado y cargado exitosamente:", nombre_archivo, "\n")
    return(data)
  } else {
    print(paste("Error al descargar el archivo:", url))
    return(NULL)
  }
}

# Función 3 ---------------------------------------------------------------

descargar_archivo_ind <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    nombre_archivo <- basename(url)
    archivo_temporal <- tempfile(fileext = ".xls")
    writeBin(content(response, "raw"), archivo_temporal)
    data <- read_excel(archivo_temporal, skip = 1, col_types = c("date", "text", rep("numeric", ncol(read_excel(archivo_temporal)) - 2)))
    unlink(archivo_temporal)
    cat("Archivo descargado y cargado exitosamente:", nombre_archivo, "\n")
    return(data)
  } else {
    print(paste("Error al descargar el archivo:", url))
    return(NULL)
  }
}

# Función 4 ---------------------------------------------------------------

reordenar_variables <- function(df) {
  nombres_n <- str_subset(names(df), "^N") %>% sort()
  nombres_ing <- str_subset(names(df), "^Ing.") %>% sort()
  nombres <- c(rbind(nombres_n, nombres_ing))
  variables_cot <- df %>% select(-starts_with("N"), -starts_with("Ing"))
  df <- bind_cols(variables_cot, df[, nombres])
  return(df)
}

# Función 4 ---------------------------------------------------------------

transformar_variable <- function(df) {
  df <- df %>% mutate(REGIÓN = case_when(REGIÓN == "I"              | REGIÓN == "01" | REGIÓN == "I\n Tarapacá"                                  |  REGIÓN == "I Tarapacá" ~ "I. Región de Tarapacá",
                                         REGIÓN == "II"             | REGIÓN == "02" | REGIÓN == "II\nAntofagasta"                               |  REGIÓN == "II Antofagasta" ~ "II. Región de Antofagasta",
                                         REGIÓN == "III"            | REGIÓN == "03" | REGIÓN == "III\nAtacama"                                  |  REGIÓN == "III Atacama" ~ "III. Región de Atacama",
                                         REGIÓN == "IV"             | REGIÓN == "04" | REGIÓN == "IV\nCoquimbo"                                  |  REGIÓN == "IV Coquimbo" ~ "IV. Región de Coquimbo",
                                         REGIÓN == "V"              | REGIÓN == "05" | REGIÓN == "V\nValparíaso"                                 |  REGIÓN == "V Valparíaso" ~ "V. Región de Valparaíso",
                                         REGIÓN == "VI"             | REGIÓN == "06" | REGIÓN == "VI\nLibertador General Bernardo O'Higgins"     |  REGIÓN == "VI Libertador General Bernardo O'Higgins" ~ "VI. Región del Libertador General Bernardo O'Higgins",
                                         REGIÓN == "VII"            | REGIÓN == "07" | REGIÓN == "VII\nMaule"                                    |  REGIÓN == "VII Maule" ~ "VII. Región deL Maule",
                                         REGIÓN == "VIII"           | REGIÓN == "08" | REGIÓN == "VIII\nBiobío"                                  |  REGIÓN == "VIII Biobío" ~ "VIII. Región deL Biobío",
                                         REGIÓN == "IX"             | REGIÓN == "09" | REGIÓN == "IX\nLa Araucanía"                              |  REGIÓN == "IX La Araucanía" ~ "IX. Región de La Araucanía",
                                         REGIÓN == "X"              | REGIÓN == "10" | REGIÓN == "X\nLos Lagos"                                  |  REGIÓN == "X Los Lagos" ~ "X. Región de Los Lagos",
                                         REGIÓN == "XI"             | REGIÓN == "11" | REGIÓN == "XI\nAysén del General Carlos Ibañez del Campo" |  REGIÓN == "XI Aysén del General Carlos Ibañez del Campo" ~ "XI. Región de Aysén del General Carlos Ibañez del Campo",
                                         REGIÓN == "XII"            | REGIÓN == "12" | REGIÓN == "XII\nMagallanes y de la Antártica Chilena"     |  REGIÓN == "XII Magallanes y de la Antártica Chilena" ~ "XII. Región de Magallanes y de la Antártica Chilena",
                                         REGIÓN == "XIII"           | REGIÓN == "13" | REGIÓN == "XIII\nMetropolitana de Santiago"               |  REGIÓN == "XIII Metropolitana de Santiago" | REGIÓN == "R. Metrop." | REGIÓN == "R.M." | REGIÓN == "R. Metropolitana" ~ "XIII. Región Metropolitana de Santiago",
                                         REGIÓN == "XIV"            | REGIÓN == "14" | REGIÓN == "XIV\nLos Ríos"                                 |  REGIÓN == "XIV Los Ríos" ~ "XIV. Región de Los Ríos",
                                         REGIÓN == "XV"             | REGIÓN == "15" | REGIÓN == "XV\nArica y Parinacota"                        |  REGIÓN == "XV Arica y Parinacota" ~ "XV. Región de Arica y Parinacota",
                                         REGIÓN == "XVI"            | REGIÓN == "16" | REGIÓN == "XVI\nÑuble"                                    |  REGIÓN == "XVI Ñuble" ~ "XVI. Región de Ñuble",
                                         REGIÓN == "S/I"            | REGIÓN == "Sin información"                                                | REGIÓN  == "Sin clasificar"                                         |  REGIÓN == "Sin Ubicación" ~ "Sin información",  TRUE ~ REGIÓN))
  return(df)
}

# Función 5 ---------------------------------------------------------------

nombrar_variables1 <- function(df) {
  colnames(df) <- c(replace(as.vector(unlist(df[1,])), 1, "FECHA")[1], 
                    paste("Femenino", as.vector(unlist(df[1,]))[2:8]),
                    paste("Masculino", as.vector(unlist(df[1,]))[9:15]), 
                    paste("Total", as.vector(unlist(df[1,]))[16:22]))
  return(df)
}

# Función 6 ---------------------------------------------------------------

nombrar_variables2 <- function(df) {
  colnames(df) <- c(replace(as.vector(unlist(df[1,])), 1, "FECHA")[1],
                    replace(as.vector(unlist(df[1,])), 2, "SEXO")[2],
                    paste("I. Región de Tarapacá", as.vector(unlist(df[1,]))[3:8], sep = "_"),
                    paste("II. Región de Antofagasta", as.vector(unlist(df[1,]))[9:14], sep = "_"), 
                    paste("III. Región de Atacama", as.vector(unlist(df[1,]))[15:20], sep = "_"),
                    paste("IV. Región de Coquimbo", as.vector(unlist(df[1,]))[21:26], sep = "_"),
                    paste("V. Región de Valparaíso", as.vector(unlist(df[1,]))[27:32], sep = "_"),
                    paste("VI. Región del Libertador General Bernardo O'Higgins", as.vector(unlist(df[1,]))[33:38], sep = "_"),
                    paste("VII. Región deL Maule", as.vector(unlist(df[1,]))[39:44], sep = "_"),
                    paste("VIII. Región deL Biobío", as.vector(unlist(df[1,]))[45:50], sep = "_"),
                    paste("IX. Región de La Araucanía", as.vector(unlist(df[1,]))[51:56], sep = "_"),
                    paste("X. Región de Los Lagos", as.vector(unlist(df[1,]))[57:62], sep = "_"),
                    paste("XI. Región de Aysén del General Carlos Ibañez del Campo", as.vector(unlist(df[1,]))[63:68], sep = "_"),
                    paste("XII. Región de Magallanes y de la Antártica Chilena", as.vector(unlist(df[1,]))[69:74], sep = "_"),
                    paste("XIII. Región Metropolitana de Santiago", as.vector(unlist(df[1,]))[75:80], sep = "_"),
                    paste("XIV. Región de Los Ríos", as.vector(unlist(df[1,]))[81:86], sep = "_"),
                    paste("XV. Región de Arica y Parinacota", as.vector(unlist(df[1,]))[87:92], sep = "_"),
                    paste("XVI. Región de Ñuble", as.vector(unlist(df[1,]))[93:98], sep = "_"),
                    paste("Sin información", as.vector(unlist(df[1,]))[99:104], sep = "_"),
                    paste("Totales por tipo de beneficio", as.vector(unlist(df[1,]))[105:111], sep = "_"))
  return(df)
}

# Función 7 ---------------------------------------------------------------

nombrar_variables3 <- function(df) {
  colnames(df) <- c(replace(as.vector(unlist(df[1, ])), 1, "FECHA")[1],
                    replace(as.vector(unlist(df[1, ])), 2, "TIPO DE ENTIDAD PRINCIPAL")[2],
                    paste("Mujeres", as.vector(unlist(df[1, ]))[3:12], sep = "_"),
                    paste("Hombres", as.vector(unlist(df[1, ]))[13:22], sep = "_"),
                    paste("Total", as.vector(unlist(df[1, ]))[3:12], sep = "_"))
  return(df)
}

# Vector 2 ----------------------------------------------------------------

nombres_variables <- c("FECHA",
                       "NúmeroVEJEZ EDAD",
                       "Monto U.F.VEJEZ EDAD",
                       "NúmeroVEJEZ ANTICIPADA",
                       "Monto U.F.VEJEZ ANTICIPADA",
                       "NúmeroINVALIDEZ TOTAL",
                       "Monto U.F.INVALIDEZ TOTAL",
                       "NúmeroINVALIDEZ PARCIAL",
                       "Monto U.F.INVALIDEZ PARCIAL",
                       "NúmeroVIUDEZ",
                       "Monto U.F.VIUDEZ",
                       "NúmeroORFANDAD", 
                       "Monto U.F.ORFANDAD",
                       "NúmeroOTRAS",
                       "Monto U.F.OTRAS",
                       "NúmeroTOTAL",
                       "Monto U.F.TOTAL")

# Función 8 ---------------------------------------------------------------

#nombrar_variables4 <- function(df) {
#  colnames(df) <- c(replace(as.vector(unlist(df[1, ])), 1, "AFP")[1],
#                    as.vector(unlist(df[1, ]))[2:45])
#  return(df)
#}


# Función 9 ---------------------------------------------------------------

nombrar_variables5 <- function(df) {
  colnames(df) <- c(replace(unlist(df[1, ]), 1, "AÑO")[1],
                    paste("VEJEZ EDAD", unlist(df[1, ])[2:4], sep = "_"),
                    paste("VEJEZ ANTICIPADA", unlist(df[1, ])[5:7], sep = "_"),
                    paste("TOTAL VEJEZ", as.vector(unlist(df[1, ]))[8:10], sep = "_"))
  return(df)
}

# Función 10 --------------------------------------------------------------

obtener_datos_PIB <- function(urls) {
  
  lista_dataframes <- list()
  
  for (i in seq_along(urls)) {
    json_data <- rjson::fromJSON(file = urls[[i]])
    dataframe <- as.data.frame(do.call(rbind, map(json_data$Series$Obs, as.vector)))
    lista_dataframes[[i]] <- dataframe
  }
  lista_dataframes <- lista_dataframes %>% set_names("PIB_precios_corrientes", "PIB_volumen_desestacionalizado", 
                                                     "PIB_volumen_encadenado", "PIB_per_cápita_PPP", "PIB_per_cápita")
  return(lista_dataframes)
}

# Función 11 --------------------------------------------------------------

descargar_archivo_ind2 <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    nombre_archivo <- basename(url)
    archivo_temporal <- tempfile(fileext = ".xlsx")
    writeBin(content(response, "raw"), archivo_temporal)
    data <- read_excel(archivo_temporal, sheet = 3)
    unlink(archivo_temporal)
    cat("Archivo descargado y cargado exitosamente:", nombre_archivo, "\n")
    return(data)
  } else {
    print(paste("Error al descargar el archivo:", url))
    return(NULL)
  }
}

# Función 12 --------------------------------------------------------------

descargar_archivo_ind3 <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    nombre_archivo <- basename(url)
    archivo_temporal <- tempfile(fileext = ".xlsx")
    writeBin(content(response, "raw"), archivo_temporal)
    data <- read_excel(archivo_temporal, sheet = 2)
    unlink(archivo_temporal)
    cat("Archivo descargado y cargado exitosamente:", nombre_archivo, "\n")
    return(data)
  } else {
    print(paste("Error al descargar el archivo:", url))
    return(NULL)
  }
}

# Función 13 --------------------------------------------------------------

crear_dataframe_pib <- function(pib) {
  data.frame(
    AÑO = lubridate::dmy(pib$indexDateString), 
    PIB = as.double(pib$value)
  )
}

# Función 14 --------------------------------------------------------------

rellenar_variables <- function(df) {
  for (i in 2:ncol(df)) {
    df[5, i] <- ifelse(is.na(df[5, i]), df[5, i - 1], df[5, i])
  }
  return(df)
}

# Función 15 --------------------------------------------------------------

rellenar_variables1 <- function(df) {
  for (i in 2:ncol(df)) {
    df[4, i] <- ifelse(is.na(df[4, i]), df[4, i - 1], df[4, i])
  }
  return(df)
}

# Función 16 --------------------------------------------------------------

rellenar_variables2 <- function(df) {
   for (i in 2:nrow(df)) {
         df[i, 1] <- ifelse(is.na(df[i, 1]), df[i - 1, 1], df[i, 1])
       }
   return(df)
}
