source("scripts/00-download-data.R")

# Procesamiento de Datos --------------------------------------------------

drive_auth()

# Bases de Datos PIB ------------------------------------------------------

lista_pib <- lista_pib %>% map(~.x %>% crear_dataframe_pib() %>% mutate(PIB = round(PIB, digits = 2)))

lista_pib <- lista_pib %>% map2(c("PRECIOS CORRIENTES", "PRECIOS ENCADENADOS", "PRECIOS ENCADENADOS DESESTACIONALIZADOS", 
                                  "DOLARES PPA", "DOLARES (REFERENCIA 2018)"), ~.x %>% mutate(TIPO = .y))

saveRDS(lista_pib$PIB_precios_corrientes, "data/macroeconomia/grafico_pib/PIB_precios_corrientes.rds")
saveRDS(lista_pib$PIB_volumen_desestacionalizado, "data/macroeconomia/grafico_pib/PIB_volumen_desestacionalizado.rds")
saveRDS(lista_pib$PIB_volumen_encadenado, "data/macroeconomia/grafico_pib/PIB_volumen_encadenado.rds")
saveRDS(lista_pib$PIB_per_cápita_PPP, "data/macroeconomia/grafico_pib/PIB_percapita_ppp.rds")
saveRDS(lista_pib$PIB_per_cápita, "data/macroeconomia/grafico_pib/PIB_per_capita.rds")

rm(lista_pib, api_pib)

# Bases de Datos Sociodemográficos ----------------------------------------

esperanza_de_vida <- esperanza_de_vida %>% filter(row_number() == 3 | row_number() >= which(
  str_detect(`INDICADORES DEMOGRÁFICOS DE LA POBLACION DE CHILE ESTIMADOS Y PROYECTADOS`,  "Esperanza de vida al nacer"))[1])

esperanza_de_vida <- esperanza_de_vida %>% setNames(replace(unlist(esperanza_de_vida[1, ]), 1, "GENERO")) %>% slice(-c(1,2)) %>% 
  mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "EDAD")

esperanza_de_vida <- esperanza_de_vida %>% mutate(AÑO = as.numeric(AÑO), GENERO = toupper(ifelse(GENERO == "Ambos sexos", "TOTAL", GENERO)))

saveRDS(esperanza_de_vida, "data/macroeconomia/grafico_poblacion/esperanza_de_vida.rds")

proyeccion_poblacion_total <- proyeccion_poblacion %>% setNames(replace(unlist(proyeccion_poblacion[1, ]), 1, "GENERO")) %>% slice(-c(1,2)) %>% 
  mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "POBLACIÓN")

proyeccion_poblacion_total <- proyeccion_poblacion_total %>% mutate(AÑO = as.numeric(AÑO), GENERO = toupper(ifelse(GENERO == "Ambos sexos", "TOTAL", GENERO)))

saveRDS(proyeccion_poblacion_total, "data/macroeconomia/grafico_poblacion/proyeccion_poblacion.rds")
rm(esperanza_de_vida, proyeccion_poblacion_total, proyeccion_poblacion)

ocup <- ocup %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, OCUPADOS = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN),
         AÑO = as.Date(str_remove(AÑO, "/P3M")), OCUPADOS = round(OCUPADOS))

ocup_inf <- ocup_inf %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, OCUPADOS_INFORMALES = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN),
         AÑO = as.Date(str_remove(AÑO, "/P3M")), OCUPADOS_INFORMALES = round(OCUPADOS_INFORMALES))

ocupados <- left_join(ocup, ocup_inf) %>% mutate(OCUPADOS_FORMALES = round(OCUPADOS - OCUPADOS_INFORMALES)) %>% filter(!is.na(OCUPADOS_FORMALES))

ocupados <- ocupados %>% mutate(TASA_OCUPADOS_FORMALES = round((OCUPADOS_FORMALES/OCUPADOS) * 100, digits = 2),
                                TASA_OCUPADOS_INFORMALES = round((OCUPADOS_INFORMALES/OCUPADOS) * 100, digits = 2))

tasa_ocup <- tasa_ocup %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, TASA_OCUPADOS = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN),
         AÑO = as.Date(str_remove(AÑO, "/P3M")), TASA_OCUPADOS = round(TASA_OCUPADOS, digits = 2))

tasa_desocup <- tasa_desocup %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, TASA_DESOCUPADOS = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN),
         AÑO = as.Date(str_remove(AÑO, "/P3M")), TASA_DESOCUPADOS = round(TASA_DESOCUPADOS, digits = 2))

ocupados <- left_join(left_join(ocupados, tasa_ocup), tasa_desocup) %>% filter(!is.na(TASA_OCUPADOS_INFORMALES))

saveRDS(ocupados, "data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds")
rm(ocup, ocup_inf, ocupados, tasa_ocup, tasa_desocup, df_ine)

# Bases de Datos de Ingresos ----------------------------------------------

ingreso_medio <- ingreso_medio %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, INGRESO_MEDIO = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN), 
         INGRESO_MEDIO = round(INGRESO_MEDIO, digits = 2))

ingreso_mediano <- ingreso_mediano %>% rename(REGIÓN = AREA_REF, AÑO = TIME_PERIOD, INGRESO_MEDIANO = OBS_VALUE, GENERO = SEXO) %>% transformar_variable() %>% 
  mutate(GENERO = case_when(GENERO == "F" ~ "Femenino", GENERO == "M" ~ "Masculino", GENERO == "AS" ~ "Total", TRUE ~ NA), REGIÓN = ifelse(REGIÓN == "_T", "Total", REGIÓN), 
         INGRESO_MEDIANO = round(INGRESO_MEDIANO, digits = 2))

ingreso_medio_mediano <- left_join(ingreso_medio, ingreso_mediano)

saveRDS(ingreso_medio_mediano, "data/macroeconomia/grafico_ingreso/ingreso_medio_mediano.rds")
saveRDS(ingreso_medio_mediano, "data/pensiones/grafico_3/ingreso_medio_mediano.rds")
saveRDS(ingreso_medio_mediano, "data/pensiones/grafico_4/ingreso_medio_mediano.rds")
saveRDS(ingreso_medio_mediano, "data/salud/grafico_3/ingreso_medio_mediano.rds")
saveRDS(ingreso_medio_mediano, "data/salud/grafico_4/ingreso_medio_mediano.rds")
rm(ingreso_medio, ingreso_mediano, ingreso_medio_mediano)

# Bases de Datos Gasto ----------------------------------------------------

gasto_salud_pib <- gasto_salud$`Gasto en Salud como % del PIB. 2003 - 2023.xlsx` %>% rename("SECTOR" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PORCENTAJE") %>% mutate(AÑO = as.numeric(AÑO), PORCENTAJE = round(PORCENTAJE, digits = 2))

saveRDS(gasto_salud_pib, "data/salud/grafico_gasto/gasto_salud_pib.rds")
rm(gasto_salud_pib)

gasto_salud <- gasto_salud %>% discard_at("Gasto en Salud como % del PIB. 2003 - 2023.xlsx")

gasto_salud_pesos <- gasto_salud$`Gasto Total en Salud, público y privado. Millones de pesos, 2003 - 2023.xlsx` %>% rename("SECTOR" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PESOS") %>% mutate(AÑO = as.numeric(AÑO), PESOS = round(PESOS, digits = 2))

saveRDS(gasto_salud_pesos, "data/salud/grafico_gasto/gasto_salud_pesos.rds")
rm(gasto_salud_pesos)

gasto_salud <- gasto_salud %>% discard_at("Gasto Total en Salud, público y privado. Millones de pesos, 2003 - 2023.xlsx")

gasto_salud_pesos_constante <- gasto_salud$`Gasto Total en Salud, público y privado. Millones de pesos constantes del año 2022, 2003 - 2023.xlsx` %>% rename("SECTOR" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PESOS") %>% mutate(AÑO = as.numeric(AÑO), PESOS = round(PESOS, digits = 2))

saveRDS(gasto_salud_pesos_constante, "data/salud/grafico_gasto/gasto_salud_pesos_constante.rds")
rm(gasto_salud_pesos_constante)

gasto_salud <- gasto_salud %>% discard_at("Gasto Total en Salud, público y privado. Millones de pesos constantes del año 2022, 2003 - 2023.xlsx")

gasto_salud_instituciones_t <- gasto_salud$`Gasto en Salud Totales. Millones de pesos, Instituciones 2003 - 2023.xlsx` %>% rename("AÑO" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "INSTITUCIONES", values_to = "PESOS CORRIENTES") %>% filter(!is.na(`PESOS CORRIENTES`)) %>% 
  mutate(`PESOS CORRIENTES` = round(`PESOS CORRIENTES`))

saveRDS(gasto_salud_instituciones_t, "data/salud/grafico_gasto/gasto_salud_instituciones_t.rds")
rm(gasto_salud_instituciones_t)

gasto_salud <- gasto_salud %>% discard_at("Gasto en Salud Totales. Millones de pesos, Instituciones 2003 - 2023.xlsx")

gasto_salud_percapita <- gasto_salud$`Gasto en Salud per cápita Sector Salud. Pesos corrientes.xlsx` %>% rename("SECTOR" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PESOS CORRIENTES") %>% mutate(AÑO = as.numeric(AÑO), 
                                                                                                    `PESOS CORRIENTES` = round(`PESOS CORRIENTES`, digits = 2))

saveRDS(gasto_salud_percapita, "data/salud/grafico_gasto/gasto_salud_percapita.rds")

gasto_salud_percapita_ppa <- gasto_salud$`Gasto en Salud. Per cápita PPA, por sector 2003 - 2023.xlsx` %>% rename("SECTOR" = "-") %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PPA") %>% mutate(AÑO = as.numeric(AÑO), PPA = round(PPA, digits = 2))

saveRDS(gasto_salud_percapita_ppa, "data/salud/grafico_gasto/gasto_salud_percapita_ppa.rds")
rm(gasto_salud_percapita, gasto_salud_percapita_ppa)

gasto_salud <- gasto_salud %>% discard_at(c("Gasto en Salud per cápita Sector Salud. Pesos corrientes.xlsx", "Gasto en Salud. Per cápita PPA, por sector 2003 - 2023.xlsx"))

gasto_salud_instituciones <- gasto_salud$`Gasto en Salud. Millones de pesos, Instituciones 2003 - 2023 pesos constante año 2023.xlsx`
rm(gasto_salud)

gasto_salud_instituciones <- gasto_salud_instituciones %>% rename("INSTITUCIONES" = "-") %>% mutate(SECCIONES = case_when(row_number() >= 1  & row_number() <= 10 ~ "Ministerio de Salud", 
  row_number() >= 11  & row_number() <= 12 ~ "Municipalidades",           row_number() >= 13  & row_number() <= 18 ~ "Otras Públicas", 
  row_number() >= 19  & row_number() <= 25 ~ "Otros Ministerios",         row_number() >= 26  & row_number() <= 28 ~ "Instituciones Privadas de Salud", 
  row_number() >= 29  & row_number() <= 31 ~ "Seguros Privados de Salud", row_number() >= 32  & row_number() <= 34 ~ "Hogares", TRUE ~ "Total"), .after = INSTITUCIONES) %>%
  filter(INSTITUCIONES != SECCIONES) %>% pivot_longer(cols = c(3:last_col()), names_to = "AÑO", values_to = "PESOS CORRIENTES") %>% 
  mutate(AÑO = as.integer(AÑO), `PESOS CORRIENTES` = round(`PESOS CORRIENTES`)) %>% filter(!is.na(`PESOS CORRIENTES`))

saveRDS(gasto_salud_instituciones, "data/salud/grafico_gasto/gasto_salud_instituciones.rds")
rm(gasto_salud_instituciones)