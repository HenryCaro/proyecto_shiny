source("scripts/01-process-data-macro.R")

# Procesamiento de Datos --------------------------------------------------

# Bases de Datos Gráficos 1 - 4  ------------------------------------------

afiliados_activos_afp.xls <- lista_archivos_general$afiliados_activos_afp.xls 

lista_archivos_general <- lista_archivos_general %>% discard_at("afiliados_activos_afp.xls")

afiliados_activos_totales.xls <- afiliados_activos_afp.xls %>% setNames(replace(unlist(afiliados_activos_afp.xls[4, ]), 1, "AFP")) %>% tail(-5) %>% 
  filter(row_number() <= which(str_detect(AFP, "TOTAL"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE AFILIADOS ACTIVOS") %>% 
  mutate(AFP = str_trim(str_remove_all(AFP, "\\(\\d+\\)|")), AÑO = str_remove(AÑO, "\\(\\d+\\)"), `NÚMERO DE AFILIADOS ACTIVOS` = as.integer(`NÚMERO DE AFILIADOS ACTIVOS`),
         `NÚMERO DE AFILIADOS ACTIVOS` = as.double(str_remove_all(`NÚMERO DE AFILIADOS ACTIVOS`, "-$"))) %>% 
  filter(!is.na(`NÚMERO DE AFILIADOS ACTIVOS`) & AFP == "TOTAL")

saveRDS(afiliados_activos_totales.xls, "data/pensiones/grafico_1/afiliados_activos_totales.rds")

cotizantes_mes_afp.xls <- lista_archivos_general$cotizantes_mes_afp.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("cotizantes_mes_afp.xls")

cotizantes_mes_totales.xls <- cotizantes_mes_afp.xls %>% setNames(replace(unlist(cotizantes_mes_afp.xls[3, ]), 1, "AFP")) %>% tail(-4) %>% 
  filter(row_number() <= which(str_detect(AFP, "TOTAL"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE COTIZANTES ACTIVOS") %>% 
  mutate(AFP = str_trim(str_remove_all(AFP, "\\(\\d+\\)|")), AÑO = str_remove(AÑO, "\\(\\d+\\)"), `NÚMERO DE COTIZANTES ACTIVOS` = as.integer(`NÚMERO DE COTIZANTES ACTIVOS`),
         `NÚMERO DE COTIZANTES ACTIVOS` = as.double(str_remove_all(`NÚMERO DE COTIZANTES ACTIVOS`, "-$"))) %>% 
  filter(!is.na(`NÚMERO DE COTIZANTES ACTIVOS`) & AFP == "TOTAL")

saveRDS(cotizantes_mes_totales.xls, "data/pensiones/grafico_2/cotizantes_mes_totales.rds")

# Base de Datos Gráfico Género --------------------------------------------

afiliados_tipo_sexo.xls <- lista_archivos_general$afiliados_tipo_sexo.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("afiliados_tipo_sexo.xls")

afiliados_tipo_sexo.xls <- afiliados_tipo_sexo.xls %>% setNames(as.vector(unlist(afiliados_tipo_sexo.xls[5, ]))) %>% tail(-6) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  mutate(`TIPO - SEXO` = case_when(row_number() == 1 ~ "DEPENDIENTES TOTALES", row_number() == 2 ~ "DEPENDIENTES MASCULINOS", row_number() == 3 ~ "DEPENDIENTES FEMENINOS", row_number() == 4 ~ "DEPENDIENTES SIN INFORMACIÓN",
                                   row_number() == 5 ~ "INDEPENDIENTES TOTALES", row_number() == 6 ~ "INDEPENDIENTES MASCULINOS", row_number() == 7 ~ "INDEPENDIENTES FEMENINOS", row_number() == 8 ~ "INDEPENDIENTES SIN INFORMACIÓN",
                                   row_number() == 9 ~ "VOLUNTARIOS TOTALES", row_number() == 10 ~ "VOLUNTARIOS MASCULINOS", row_number() == 11 ~ "VOLUNTARIOS FEMENINOS", row_number() == 12 ~ "VOLUNTARIOS SIN INFORMACIÓN",
                                   row_number() == 13 ~ "TOTALES AFILIADOS", row_number() == 14 ~ "TOTALES MASCULINOS", row_number() == 15 ~ "TOTALES FEMENINOS", row_number() == 16 ~ "TOTALES SIN INFORMACIÓN", TRUE ~ `TIPO - SEXO`)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% mutate(`TIPO - SEXO` = toupper(`TIPO - SEXO`))

afiliados_tipo_sexo_dependientes.xls <- afiliados_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^DEPENDIENTES") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "DEPENDIENTES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(afiliados_tipo_sexo_dependientes.xls, "data/pensiones/grafico_genero/afiliados_tipo_sexo_dependientes.rds")
rm(afiliados_tipo_sexo_dependientes.xls)

afiliados_tipo_sexo_independientes.xls <- afiliados_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^INDEPENDIENTES") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "INDEPENDIENTES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(afiliados_tipo_sexo_independientes.xls, "data/pensiones/grafico_genero/afiliados_tipo_sexo_independientes.rds")
rm(afiliados_tipo_sexo_independientes.xls)

afiliados_tipo_sexo_voluntarios.xls <- afiliados_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^VOLUNTARIOS") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "VOLUNTARIOS")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(afiliados_tipo_sexo_voluntarios.xls, "data/pensiones/grafico_genero/afiliados_tipo_sexo_voluntarios.rds")
rm(afiliados_tipo_sexo_voluntarios.xls)

afiliados_tipo_sexo_totales.xls <- afiliados_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^TOTALES") & !str_detect(`TIPO - SEXO`, "AFILIADOS")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "TOTALES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(afiliados_tipo_sexo_totales.xls, "data/pensiones/grafico_genero/afiliados_tipo_sexo_totales.rds")
rm(afiliados_tipo_sexo_totales.xls)

cotizantes_tipo_sexo.xls <- lista_archivos_general$cotizantes_tipo_sexo.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("cotizantes_tipo_sexo.xls")

cotizantes_tipo_sexo.xls <- cotizantes_tipo_sexo.xls %>% setNames(as.vector(unlist(cotizantes_tipo_sexo.xls[5, ]))) %>% tail(-6) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  mutate(`TIPO - SEXO` = case_when(row_number() == 1 ~ "DEPENDIENTES TOTALES", row_number() == 2 ~ "DEPENDIENTES MASCULINOS", row_number() == 3 ~ "DEPENDIENTES FEMENINOS", row_number() == 4 ~ "DEPENDIENTES SIN INFORMACIÓN",
                                   row_number() == 5 ~ "INDEPENDIENTES TOTALES", row_number() == 6 ~ "INDEPENDIENTES MASCULINOS", row_number() == 7 ~ "INDEPENDIENTES FEMENINOS", row_number() == 8 ~ "INDEPENDIENTES SIN INFORMACIÓN",
                                   row_number() == 9 ~ "VOLUNTARIOS TOTALES", row_number() == 10 ~ "VOLUNTARIOS MASCULINOS", row_number() == 11 ~ "VOLUNTARIOS FEMENINOS", row_number() == 12 ~ "VOLUNTARIOS SIN INFORMACIÓN",
                                   row_number() == 13 ~ "TOTALES COTIZANTES", row_number() == 14 ~ "TOTALES MASCULINOS", row_number() == 15 ~ "TOTALES FEMENINOS", row_number() == 16 ~ "TOTALES SIN INFORMACIÓN", TRUE ~ `TIPO - SEXO`)) %>% 
  filter(row_number() <= which(str_detect(`TIPO - SEXO`, "TOTALES SIN INFORMACIÓN"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD")

cotizantes_tipo_sexo_dependientes.xls <- cotizantes_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^DEPENDIENTES") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "DEPENDIENTES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_tipo_sexo_dependientes.xls, "data/pensiones/grafico_genero/cotizantes_tipo_sexo_dependientes.rds")
rm(cotizantes_tipo_sexo_dependientes.xls)

cotizantes_tipo_sexo_independientes.xls <- cotizantes_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^INDEPENDIENTES") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "INDEPENDIENTES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_tipo_sexo_independientes.xls, "data/pensiones/grafico_genero/cotizantes_tipo_sexo_independientes.rds")
rm(cotizantes_tipo_sexo_independientes.xls)

cotizantes_tipo_sexo_voluntarios.xls <- cotizantes_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^VOLUNTARIOS") & !str_detect(`TIPO - SEXO`, "TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "VOLUNTARIOS")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_tipo_sexo_voluntarios.xls, "data/pensiones/grafico_genero/cotizantes_tipo_sexo_voluntarios.rds")
rm(cotizantes_tipo_sexo_voluntarios.xls)

cotizantes_tipo_sexo_totales.xls <- cotizantes_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "^TOTALES") & !str_detect(`TIPO - SEXO`, "COTIZANTES")) %>%
  mutate(`TIPO - SEXO` = str_trim(str_remove(`TIPO - SEXO`, "TOTALES")), `TIPO - SEXO` = str_to_title(`TIPO - SEXO`)) %>% rename(GENERO = `TIPO - SEXO`) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_tipo_sexo_totales.xls, "data/pensiones/grafico_genero/cotizantes_tipo_sexo_totales.rds")
rm(cotizantes_tipo_sexo_totales.xls)

edad_prom_pens_vejez_historico.xls <- lista_archivos_general$edad_prom_pens_vejez_historico.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("edad_prom_pens_vejez_historico.xls")

edad_prom_pens_vejez_edad_sexo.xls <- edad_prom_pens_vejez_historico.xls %>% tail(-2) %>% nombrar_variables5() %>% tail(-1) %>% 
  pivot_longer(cols = starts_with("VEJEZ EDAD") | starts_with("VEJEZ ANTICIPADA") | starts_with("TOTAL VEJEZ"), names_to = c("TIPO DE BENEFICIO", "SEXO"), 
               names_pattern = "(VEJEZ EDAD|VEJEZ ANTICIPADA|TOTAL VEJEZ)_\\s*(.*)", values_to = "EDAD PROMEDIO") %>% 
  mutate(`EDAD PROMEDIO` = as.double(`EDAD PROMEDIO`), AÑO = as.numeric(AÑO)) %>% filter(between(AÑO, 1970, 2040) & !is.na(`EDAD PROMEDIO`) & `TIPO DE BENEFICIO` == "VEJEZ EDAD" & SEXO != "TOTAL")

saveRDS(edad_prom_pens_vejez_edad_sexo.xls, "data/pensiones/grafico_genero/edad_prom_pens_vejez_edad_sexo.rds")
rm(edad_prom_pens_vejez_edad_sexo.xls)

edad_prom_pens_anticipada_vejez_edad_sexo.xls <- edad_prom_pens_vejez_historico.xls %>% tail(-2) %>% nombrar_variables5() %>% tail(-1) %>% 
  pivot_longer(cols = starts_with("VEJEZ EDAD") | starts_with("VEJEZ ANTICIPADA") | starts_with("TOTAL VEJEZ"), names_to = c("TIPO DE BENEFICIO", "SEXO"), 
               names_pattern = "(VEJEZ EDAD|VEJEZ ANTICIPADA|TOTAL VEJEZ)_\\s*(.*)", values_to = "EDAD PROMEDIO") %>% 
  mutate(`EDAD PROMEDIO` = as.double(`EDAD PROMEDIO`), AÑO = as.numeric(AÑO)) %>% filter(between(AÑO, 1970, 2040) & !is.na(`EDAD PROMEDIO`) & `TIPO DE BENEFICIO` == "VEJEZ ANTICIPADA" & SEXO != "TOTAL")

saveRDS(edad_prom_pens_anticipada_vejez_edad_sexo.xls, "data/pensiones/grafico_genero/edad_prom_pens_anticipada_vejez_edad_sexo.rds")
rm(edad_prom_pens_anticipada_vejez_edad_sexo.xls, edad_prom_pens_vejez_historico.xls)

# Bases de Datos Gráfico General ------------------------------------------

afiliados_activos_afp_totales.xls <- afiliados_activos_afp.xls %>% setNames(replace(unlist(afiliados_activos_afp.xls[4, ]), 1, "AFP")) %>% tail(-5) %>% 
  filter(row_number() <= which(str_detect(AFP, "TOTAL"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE AFILIADOS ACTIVOS") %>% 
  mutate(AFP = case_when(row_number() == 1130 ~ "QUALITAS MAGISTER", row_number() == 1131 ~ "QUALITAS MAGISTER", row_number() == 1132 ~ "QUALITAS MAGISTER", TRUE ~ AFP), 
         AFP = str_trim(str_remove_all(AFP, "\\(\\d+\\)|")), AÑO = str_remove(AÑO, "\\(\\d+\\)"), AÑO = as.numeric(AÑO), 
         `NÚMERO DE AFILIADOS ACTIVOS`= as.integer(str_replace(`NÚMERO DE AFILIADOS ACTIVOS`, "-$", NA_character_))) %>% filter(!is.na(`NÚMERO DE AFILIADOS ACTIVOS`) & AFP != "TOTAL")

saveRDS(afiliados_activos_afp_totales.xls, "data/pensiones/grafico_totales_generales/afiliados_activos_afp_totales.rds")
rm(afiliados_activos_afp.xls, afiliados_activos_totales.xls, afiliados_activos_afp_totales.xls)

afiliados_tipo_totales.xls <- afiliados_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "(^DEPENDIENTES|^INDEPENDIENTES|^VOLUNTARIOS) TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_to_title(str_trim(str_remove(`TIPO - SEXO`, "TOTALES")))) %>% rename(`AFILIADOS POR TIPO DE RELACION LABORAL` = CANTIDAD) %>% 
  filter(!is.na(`AFILIADOS POR TIPO DE RELACION LABORAL`))

saveRDS(afiliados_tipo_totales.xls, "data/pensiones/grafico_totales_generales/afiliados_tipo_totales.rds")
rm(afiliados_tipo_totales.xls, afiliados_tipo_sexo.xls)

afiliados_region.xls <- lista_archivos_general$afiliados_region.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("afiliados_region.xls")

afiliados_region.xls <- afiliados_region.xls %>% setNames(replace(unlist(afiliados_region.xls[5, ]), 1, "REGIÓN")) %>% tail(-7) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE AFILIADOS POR REGIÓN") %>% 
  filter(!is.na(`NÚMERO DE AFILIADOS POR REGIÓN`)) %>% transformar_variable() %>% filter(REGIÓN != "Total")

saveRDS(afiliados_region.xls, "data/pensiones/grafico_totales_generales/afiliados_region.rds")
rm(afiliados_region.xls)

afiliados_edad.xls <- lista_archivos_general$afiliados_edad.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("afiliados_edad.xls")

afiliados_edad.xls <- afiliados_edad.xls %>% setNames(replace(unlist(afiliados_edad.xls[5, ]), 1, "RANGO EDAD")) %>% tail(-6) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE AFILIADOS POR EDAD") %>% filter(`RANGO EDAD` != "Total")

saveRDS(afiliados_edad.xls, "data/pensiones/grafico_totales_generales/afiliados_edad.rds")
rm(afiliados_edad.xls)

afiliados_region_edad.xls <- lista_archivos_POSIXct_afil_cot$afiliados_region_edad.xls
lista_archivos_POSIXct_afil_cot <- lista_archivos_POSIXct_afil_cot %>% discard_at("afiliados_region_edad.xls")

afiliados_region_edad_totales.xls <- afiliados_region_edad.xls %>% pivot_longer(cols = c(3:last_col()), names_to = "RANGO EDAD", values_to = "CANTIDAD") %>% 
  transformar_variable() %>% rename(AÑO = FECHA) %>% mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD), AÑO = as.Date(AÑO)) %>% 
  filter(REGIÓN != "TOTAL" & `RANGO EDAD` != "TOTAL" & month(AÑO) == 12)

saveRDS(afiliados_region_edad_totales.xls, "data/pensiones/grafico_totales_generales/afiliados_region_edad_totales.rds")
rm(afiliados_region_edad_totales.xls, afiliados_region_edad.xls)

afiliados_edad_afp.xls <- lista_archivos_POSIXct_afil_cot$afiliados_edad_afp.xls
lista_archivos_POSIXct_afil_cot <- lista_archivos_POSIXct_afil_cot %>% discard_at("afiliados_edad_afp.xls")

afiliados_edad_afp_totales.xls <- afiliados_edad_afp.xls %>% pivot_longer(cols = c(3:last_col()), names_to = "RANGO EDAD", values_to = "CANTIDAD") %>% 
  mutate(AFP = toupper(AFP), CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD), PERIODO = as.Date(PERIODO)) %>% rename(AÑO = PERIODO) %>% 
  filter(AFP != "SISTEMA" & `RANGO EDAD` != "TOTAL" & !is.na(CANTIDAD) & month(AÑO) == 12)

saveRDS(afiliados_edad_afp_totales.xls, "data/pensiones/grafico_totales_generales/afiliados_edad_afp_totales.rds")
rm(afiliados_edad_afp_totales.xls, afiliados_edad_afp.xls)

afiliados_region_afp.xls <- lista_archivos_POSIXct_afil_cot$afiliados_region_afp.xls
lista_archivos_POSIXct_afil_cot <- lista_archivos_POSIXct_afil_cot %>% discard_at("afiliados_region_afp.xls")

afiliados_region_afp_totales.xls <- afiliados_region_afp.xls %>% pivot_longer(cols = c(3:last_col()), names_to = "REGIÓN", values_to = "NUMERO DE AFILIADOS") %>% transformar_variable() %>% 
  mutate(`NUMERO DE AFILIADOS` = ifelse(`NUMERO DE AFILIADOS` == 0, NA, `NUMERO DE AFILIADOS`), AFP = toupper(AFP), PERIODO = as.Date(PERIODO)) %>% 
  rename(AÑO = PERIODO) %>% filter(!is.na(`NUMERO DE AFILIADOS`) & AFP != "SISTEMA" & month(AÑO) == 12)
  
saveRDS(afiliados_region_afp_totales.xls, "data/pensiones/grafico_totales_generales/afiliados_region_afp_totales.rds")
rm(afiliados_region_afp_totales.xls, afiliados_region_afp.xls)

cotizantes_mes_afp_totales.xls <- cotizantes_mes_afp.xls %>% setNames(replace(unlist(cotizantes_mes_afp.xls[3, ]), 1, "AFP")) %>% tail(-4) %>% 
  filter(row_number() <= which(str_detect(AFP, "TOTAL"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE COTIZANTES ACTIVOS") %>% 
  mutate(AFP = case_when(row_number() == 1022 ~ "QUALITAS MAGISTER", row_number() == 1023 ~ "QUALITAS MAGISTER", row_number() == 1024 ~ "QUALITAS MAGISTER", TRUE ~ AFP), 
         AFP = str_trim(str_remove_all(AFP, "\\(\\d+\\)|")), AÑO = str_remove(AÑO, "\\(\\d+\\)"), AÑO = as.numeric(AÑO), 
         `NÚMERO DE COTIZANTES ACTIVOS`= as.integer(str_replace(`NÚMERO DE COTIZANTES ACTIVOS`, "-$", NA_character_))) %>% filter(!is.na(`NÚMERO DE COTIZANTES ACTIVOS`) & AFP != "TOTAL")

saveRDS(cotizantes_mes_afp_totales.xls, "data/pensiones/grafico_totales_generales/cotizantes_mes_afp_totales.rds")
rm(cotizantes_mes_totales.xls, cotizantes_mes_afp_totales.xls, cotizantes_mes_afp.xls)

cotizantes_tipo_totales.xls <- cotizantes_tipo_sexo.xls %>% filter(str_detect(`TIPO - SEXO`, "(^DEPENDIENTES|^INDEPENDIENTES|^VOLUNTARIOS) TOTALES")) %>% 
  mutate(`TIPO - SEXO` = str_to_title(str_trim(str_remove(`TIPO - SEXO`, "TOTALES")))) %>% rename(`COTIZANTES POR TIPO DE RELACION LABORAL` = CANTIDAD) %>% 
  filter(!is.na(`COTIZANTES POR TIPO DE RELACION LABORAL`))

saveRDS(cotizantes_tipo_totales.xls, "data/pensiones/grafico_totales_generales/cotizantes_tipo_totales.rds")
rm(cotizantes_tipo_totales.xls, cotizantes_tipo_sexo.xls)

cotizantes_region.xls <- lista_archivos_general$cotizantes_region.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("cotizantes_region.xls")

cotizantes_region.xls <- cotizantes_region.xls %>% setNames(replace(unlist(cotizantes_region.xls[5, ]), 1, "REGIÓN")) %>% tail(-7) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE COTIZANTES POR REGIÓN") %>% 
  filter(!is.na(`NÚMERO DE COTIZANTES POR REGIÓN`)) %>% transformar_variable() %>% filter(REGIÓN != "Total")

saveRDS(cotizantes_region.xls, "data/pensiones/grafico_totales_generales/cotizantes_region.rds")
rm(cotizantes_region.xls)

cotizantes_edad.xls <- lista_archivos_general$cotizantes_edad.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("cotizantes_edad.xls")

cotizantes_edad.xls <- cotizantes_edad.xls %>% setNames(replace(unlist(cotizantes_edad.xls[5, ]), 1, "RANGO EDAD")) %>% tail(-6) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NÚMERO DE COTIZANTES POR EDAD") %>% filter(`RANGO EDAD` != "Total")

saveRDS(cotizantes_edad.xls, "data/pensiones/grafico_totales_generales/cotizantes_edad.rds")
rm(cotizantes_edad.xls)

cotizantes_edad_afp.xls <- lista_archivos_POSIXct_afil_cot$cotizantes_edad_afp.xls
lista_archivos_POSIXct_afil_cot <- lista_archivos_POSIXct_afil_cot %>% discard_at("cotizantes_edad_afp.xls")

cotizantes_edad_afp_totales.xls <- cotizantes_edad_afp.xls %>% pivot_longer(cols = c(3:last_col()), names_to = "RANGO EDAD", values_to = "CANTIDAD") %>% 
  mutate(AFP = toupper(AFP), CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD), PERIODO = as.Date(PERIODO)) %>% rename(AÑO = PERIODO) %>% 
  filter(AFP != "SISTEMA" & `RANGO EDAD` != "TOTAL" & !is.na(CANTIDAD) & month(AÑO) == 12)

saveRDS(cotizantes_edad_afp_totales.xls, "data/pensiones/grafico_totales_generales/cotizantes_edad_afp_totales.rds")
rm(cotizantes_edad_afp_totales.xls, cotizantes_edad_afp.xls)

cotizantes_region_afp.xls <- lista_archivos_POSIXct_afil_cot$cotizantes_region_afp.xls
lista_archivos_POSIXct_afil_cot <- lista_archivos_POSIXct_afil_cot %>% discard_at("cotizantes_region_afp.xls")

cotizantes_region_afp_totales.xls <- cotizantes_region_afp.xls %>% pivot_longer(cols = c(3:last_col()), names_to = "REGIÓN", values_to = "NUMERO DE COTIZANTES") %>% 
  transformar_variable() %>%  mutate(`NUMERO DE COTIZANTES` = ifelse(`NUMERO DE COTIZANTES` == 0, NA, `NUMERO DE COTIZANTES`), AFP = toupper(AFP), PERIODO = as.Date(PERIODO)) %>% 
  rename(AÑO = PERIODO) %>% filter(!is.na(`NUMERO DE COTIZANTES`) & AFP != "SISTEMA" & month(AÑO) == 12)

saveRDS(cotizantes_region_afp_totales.xls, "data/pensiones/grafico_totales_generales/cotizantes_region_afp_totales.rds")
rm(cotizantes_region_afp_totales.xls, cotizantes_region_afp.xls)

lista_archivos_general <- lista_archivos_general %>% discard_at(c(1:3))

# Bases de Datos Gráficos 5 - 7  ------------------------------------------

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls <- lista_archivos_general$evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls")

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls <- evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% setNames(replace(
  unlist(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls[3,]), 1, "SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS")) %>% tail(-4) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "PORCENTAJE") %>% mutate(AÑO = as.Date(as.numeric(AÑO), origin = "1899-12-30"), PORCENTAJE = round(as.numeric(PORCENTAJE), digits = 10),
  `SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS` = case_when(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS` == "(1)" ~ "TOTAL ACTIVOS EN MILLONES DE PESOS A DICIEMBRE DE CADA AÑO", 
                                                                TRUE ~ `SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`)) %>% filter(!is.na(PORCENTAJE))

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal.xls <- evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Estatal")) %>% mutate(PORCENTAJE = round(PORCENTAJE, digits = 2))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal.xls, 
        "data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal.rds")

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero.xls <- evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Financiero")) %>% mutate(PORCENTAJE = round(PORCENTAJE, digits = 2))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero.xls, 
        "data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero.rds")

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas.xls <- evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Empresas")) %>% mutate(PORCENTAJE = round(PORCENTAJE, digits = 2))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas.xls, 
        "data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas.rds")

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero.xls <- evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Extranjero")) %>% mutate(PORCENTAJE = round(PORCENTAJE, digits = 2))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero.xls, 
        "data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero.rds")

rm(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal.xls, 
   evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero.xls,
   evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas.xls,
   evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero.xls)

variacion_patrimonial_fondos_pensiones.xls <- lista_archivos_general$variacion_patrimonial_fondos_pensiones.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("variacion_patrimonial_fondos_pensiones.xls")

variacion_patrimonial_fondos_pensiones_patrimonio_inicial.xls <- variacion_patrimonial_fondos_pensiones.xls %>% 
  setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% select(-19) %>% slice(-1) %>% 
  mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% select(1:3, 19, 31) %>% 
  setNames(c("AÑO", "PATRIMONIO INICIAL", "AUMENTOS", "DISMINUCIONES", "PATRIMONIO FINAL")) %>% select(AÑO, `PATRIMONIO INICIAL`)

saveRDS(variacion_patrimonial_fondos_pensiones_patrimonio_inicial.xls, "data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_patrimonio_inicial.rds")

variacion_patrimonial_fondos_pensiones_aumentos.xls <- variacion_patrimonial_fondos_pensiones.xls %>% 
  setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% select(-19) %>% slice(-1) %>% 
  mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% select(1:3, 19, 31) %>% 
  setNames(c("AÑO", "PATRIMONIO INICIAL", "AUMENTOS", "DISMINUCIONES", "PATRIMONIO FINAL")) %>% select(AÑO, AUMENTOS)

saveRDS(variacion_patrimonial_fondos_pensiones_aumentos.xls, "data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_aumentos.rds")

variacion_patrimonial_fondos_pensiones_disminuciones.xls <- variacion_patrimonial_fondos_pensiones.xls %>% 
  setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% select(-19) %>% slice(-1) %>% 
  mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% select(1:3, 19, 31) %>% 
  setNames(c("AÑO", "PATRIMONIO INICIAL", "AUMENTOS", "DISMINUCIONES", "PATRIMONIO FINAL")) %>% select(AÑO, DISMINUCIONES)

saveRDS(variacion_patrimonial_fondos_pensiones_disminuciones.xls, "data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_disminuciones.rds")

variacion_patrimonial_fondos_pensiones_patrimonio_final.xls <- variacion_patrimonial_fondos_pensiones.xls %>% 
  setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% select(-19) %>% slice(-1) %>% 
  mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% select(1:3, 19, 31) %>% 
  setNames(c("AÑO", "PATRIMONIO INICIAL", "AUMENTOS", "DISMINUCIONES", "PATRIMONIO FINAL")) %>% select(AÑO, `PATRIMONIO FINAL`)

saveRDS(variacion_patrimonial_fondos_pensiones_patrimonio_final.xls, "data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_patrimonio_final.rds")

rm(variacion_patrimonial_fondos_pensiones_patrimonio_inicial.xls, variacion_patrimonial_fondos_pensiones_aumentos.xls, variacion_patrimonial_fondos_pensiones_disminuciones.xls,
   variacion_patrimonial_fondos_pensiones_patrimonio_final.xls)

rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls <- lista_archivos_general$rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls")

rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_totales.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls %>% tail(-4) %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls[5,]), 1, "AFP")) %>% slice(-1) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% 
  mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = str_remove_all(AÑO, "\\(\\d+\\)"), RENTABILIDAD = str_replace(RENTABILIDAD, "_$", NA_character_),
         RENTABILIDAD = as.double(RENTABILIDAD)) %>% filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "Sep") & AFP == "SISTEMA") 

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_totales.xls, "data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_totales.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls <- lista_archivos_general$rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls")

rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_totales.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls %>% tail(-4) %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls[5,]), 1, "AFP")) %>% slice(-1) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% 
  mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = str_remove_all(AÑO, "\\(\\d+\\)"), RENTABILIDAD = str_replace(RENTABILIDAD, "_$", NA_character_),
         RENTABILIDAD = as.double(RENTABILIDAD)) %>% filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "Sep") & AFP == "SISTEMA") 

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_totales.xls, "data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_totales.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls <- lista_archivos_general$rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls")

rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_totales.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls %>% tail(-4) %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls[5,]), 1, "AFP")) %>% slice(-1) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% 
  mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = str_remove_all(AÑO, "\\(\\d+\\)"), RENTABILIDAD = str_replace(RENTABILIDAD, "_$", NA_character_),
         RENTABILIDAD = as.double(RENTABILIDAD)) %>% filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "Jul") & AFP == "SISTEMA")

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_totales.xls, "data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_totales.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls <- lista_archivos_general$rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls")

rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_totales.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls %>% tail(-4) %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls[5,]), 1, "AFP")) %>% slice(-1) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% 
  mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = str_remove_all(AÑO, "\\(\\d+\\)"), RENTABILIDAD = str_replace(RENTABILIDAD, "_$", NA_character_),
         RENTABILIDAD = as.double(RENTABILIDAD)) %>% filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "Sep") & AFP == "SISTEMA")

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_totales.xls, "data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_totales.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls <- lista_archivos_general$rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls")

rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_totales.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls %>% tail(-4) %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls[5,]), 1, "AFP")) %>% slice(-1) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% 
  mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = str_remove_all(AÑO, "\\(\\d+\\)"), RENTABILIDAD = str_replace(RENTABILIDAD, "_$", NA_character_),
         RENTABILIDAD = as.double(RENTABILIDAD)) %>% filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "May") & AFP == "SISTEMA")

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_totales.xls, "data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_totales.rds")

rm(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_totales.xls, rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_totales.xls, 
   rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_totales.xls, rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_totales.xls,
   rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_totales.xls)

# Bases de Datos Detalles AFP ---------------------------------------------

rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_afp.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls[5,]), 1, "AFP")) %>% tail(-5) %>% filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = as.numeric(str_remove_all(AÑO, "\\(\\d+\\)")),
                                                                                                RENTABILIDAD = as.double(str_replace(RENTABILIDAD, "_$", NA_character_))) %>% 
  filter(!is.na(RENTABILIDAD) & !is.na(AFP) & AFP != "SISTEMA" & !str_detect(AÑO, "Sep"))

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_afp.xls, "data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_afp.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_afp.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls[5,]), 1, "AFP")) %>% tail(-5) %>% filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = as.numeric(str_remove_all(AÑO, "\\(\\d+\\)")),
                                                                                                RENTABILIDAD = as.double(str_replace(RENTABILIDAD, "_$", NA_character_))) %>% 
  filter(!is.na(RENTABILIDAD) & !is.na(AFP) & AFP != "SISTEMA" & !str_detect(AÑO, "Sep"))

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_afp.xls, "data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_afp.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_afp.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls %>% 
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls[5,]), 1, "AFP")) %>% tail(-5) %>% 
  filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>%
  mutate(AFP = ifelse(AFP == "BANSANDER(4)", "SUMMA BANSANDER", AFP), AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = as.numeric(str_remove_all(AÑO, "\\(\\d+\\)")), 
         RENTABILIDAD = as.double(str_replace(RENTABILIDAD, "_$", NA_character_))) %>% mutate(AFP = case_when(row_number() == 366 ~ "QUALITAS MAGISTER", 
                                                                                                              row_number() == 367 ~ "QUALITAS MAGISTER",
                                                                                                              row_number() == 368 ~ "QUALITAS MAGISTER", 
                                                                                                              row_number() == 369 ~ "QUALITAS MAGISTER", TRUE ~ AFP)) %>%
  filter(!is.na(RENTABILIDAD) & !str_detect(AÑO, "Jul") & AFP != "SISTEMA")
  
saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_afp.xls, "data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_afp.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_afp.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls %>%
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls[5,]), 1, "AFP")) %>% tail(-5) %>% filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = as.numeric(str_remove_all(AÑO, "\\(\\d+\\)")),
                                                                                                RENTABILIDAD = as.double(str_replace(RENTABILIDAD, "_$", NA_character_))) %>% 
  filter(!is.na(RENTABILIDAD) & !is.na(AFP) & AFP != "SISTEMA" & !str_detect(AÑO, "Sep"))

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_afp.xls, "data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_afp.rds")

rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_afp.xls <- rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls %>%
  setNames(replace(unlist(rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls[5,]), 1, "AFP")) %>% tail(-5) %>% filter(row_number() <= which(str_detect(AFP, "SISTEMA"))[1]) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "RENTABILIDAD") %>% mutate(AFP = str_remove_all(AFP, "\\(\\d+\\)"), AÑO = as.numeric(str_remove_all(AÑO, "\\(\\d+\\)")),
                                                                                                RENTABILIDAD = as.double(str_replace(RENTABILIDAD, "_$", NA_character_))) %>% 
  filter(!is.na(RENTABILIDAD) & !is.na(AFP) & AFP != "SISTEMA" & !str_detect(AÑO, "May"))

saveRDS(rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_afp.xls, "data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_afp.rds")

rm(rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_afp.xls, rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_afp.xls,
   rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_afp.xls, rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_afp.xls,
   rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_afp.xls, rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf.xls,
   rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf.xls, rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf.xls,
   rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf.xls, rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf.xls)

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal_totales.xls <- 
  evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(row_number() <= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Financiero"))[1] & 
           !str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Financiero|Sector Estatal")) %>% mutate(PORCENTAJE = round(PORCENTAJE, digits = 7))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal_totales.xls, 
        "data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal_totales.rds")

rm(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal_totales.xls)

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero_totales.xls <- 
  evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(row_number() >= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "MINVIU"))[1] & 
           row_number() <= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Empresas"))[1] &
           !str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "MINVIU|Sector Financiero|Sector Empresas"))

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero_totales.xls, 
        "data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero_totales.rds")

rm(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero_totales.xls)

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas_totales.xls <- 
  evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(row_number() >= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Empresas"))[1] & 
           row_number() <= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Extranjero"))[1] &
                                   !str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Empresas|Sector Extranjero")) %>% 
  filter(PORCENTAJE != 0.000000000)

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas_totales.xls, 
        "data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas_totales.rds")

rm(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas_totales.xls)

evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero_totales.xls <- 
  evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls %>% 
  filter(row_number() >= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Extranjero"))[1] & 
           row_number() <= which(str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "TOTAL ACTIVOS"))[1] &
           !str_detect(`SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS`, "Sector Extranjero|TOTAL ACTIVOS")) %>% 
  filter(PORCENTAJE != 0.000000000 & `SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS` != "Activo Disponible")

saveRDS(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero_totales.xls, 
        "data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero_totales.rds")

rm(evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero_totales.xls, 
   evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros.xls)

variacion_patrimonial_fondos_pensiones_aumentos.xls <- variacion_patrimonial_fondos_pensiones.xls %>% setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% 
  select(1, 4:18) %>% tail(-2) %>% mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% pivot_longer(cols = c(2:last_col()), names_to = "TIPO DE AUMENTO PATRIMONIAL", 
  values_to = "VARIACION PATRIMONIAL") %>% mutate(`TIPO DE AUMENTO PATRIMONIAL` = str_trim(str_remove_all(`TIPO DE AUMENTO PATRIMONIAL`, "\\(\\d+\\)"))) %>% filter(!is.na(`VARIACION PATRIMONIAL`)) %>% 
  mutate(`VARIACION PATRIMONIAL` = round(`VARIACION PATRIMONIAL`, digits = 3))

saveRDS(variacion_patrimonial_fondos_pensiones_aumentos.xls, "data/pensiones/grafico_afps/variacion_patrimonial_fondos_pensiones_aumentos.rds")
rm(variacion_patrimonial_fondos_pensiones_aumentos.xls)

variacion_patrimonial_fondos_pensiones_disminuciones.xls <- variacion_patrimonial_fondos_pensiones.xls %>% setNames(replace(unlist(variacion_patrimonial_fondos_pensiones.xls[2, ]), 1, "AÑO")) %>% 
  select(1, 21:31) %>% tail(-2) %>% mutate(across(everything(), as.double)) %>% filter(between(AÑO, 1970, 2040)) %>% pivot_longer(cols = c(2:last_col()), names_to = "TIPO DE DISMINUCION PATRIMONIAL", 
  values_to = "VARIACION PATRIMONIAL") %>% mutate(`TIPO DE DISMINUCION PATRIMONIAL` = str_trim(str_remove_all(`TIPO DE DISMINUCION PATRIMONIAL`, "\\(\\d+\\)"))) %>% filter(!is.na(`VARIACION PATRIMONIAL`)) %>%
  mutate(`VARIACION PATRIMONIAL` = round(`VARIACION PATRIMONIAL`, digits = 3))

saveRDS(variacion_patrimonial_fondos_pensiones_disminuciones.xls, "data/pensiones/grafico_afps/variacion_patrimonial_fondos_pensiones_disminuciones.rds")
rm(variacion_patrimonial_fondos_pensiones_disminuciones.xls, variacion_patrimonial_fondos_pensiones.xls)

activos_fondos_pensiones_anuales.xls <- lista_archivos_general$activos_fondos_pensiones_anuales.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("activos_fondos_pensiones_anuales.xls")

activos_fondos_pensiones_anuales_afp.xls <- activos_fondos_pensiones_anuales.xls %>% setNames(replace(unlist(activos_fondos_pensiones_anuales.xls[3, ]), 1, "AFP")) %>%
  tail(-5) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "ACTIVOS") %>% mutate(AFP = str_trim((str_remove_all(AFP, "\\(\\d+\\)"))),  
  AÑO = as.numeric(AÑO), ACTIVOS = as.double(ACTIVOS)) %>% filter(!is.na(ACTIVOS) & AFP != "TOTAL")

saveRDS(activos_fondos_pensiones_anuales_afp.xls, "data/pensiones/grafico_afps/activos_fondos_pensiones_anuales_afp.rds")
rm(activos_fondos_pensiones_anuales_afp.xls, activos_fondos_pensiones_anuales.xls)

lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("pasivos_fondos_pensiones.xls")
pasivos_fondos_pensiones.xls <- descargar_archivo_ind("https://www.spensiones.cl/inf_estadistica/series_afp/fondos_pensiones//pasivos_fondos_pensiones.xls")

pasivos_fondos_pensiones_totales.xls <- pasivos_fondos_pensiones.xls %>% select(-c(16:29)) %>% pivot_longer(cols = c(3:15), names_to = "TIPO DE PASIVOS", values_to = "PASIVOS") %>% 
  rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO), PASIVOS = round(PASIVOS, digits = 2)) %>% filter(!is.na(PASIVOS) & `TIPO DE PASIVOS` == "TOTAL DE PASIVOS" & 
                                                                                                        !str_detect(AFP,"SISTEMA|TOTAL")) %>% select(-`TIPO DE PASIVOS`)

saveRDS(pasivos_fondos_pensiones_totales.xls, "data/pensiones/grafico_afps/pasivos_fondos_pensiones_totales.rds")
rm(pasivos_fondos_pensiones_totales.xls, pasivos_fondos_pensiones.xls)

valor_fondos_pensiones.xls <- lista_archivos_general$valor_fondos_pensiones.xls
lista_archivos_general <- lista_archivos_general %>% discard_at("valor_fondos_pensiones.xls")

valor_fondos_pensiones_anuales_afp.xls <- valor_fondos_pensiones.xls %>% setNames(replace(unlist(valor_fondos_pensiones.xls[3, ]), 1, "AFP")) %>% tail(-4) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "VALOR FONDOS") %>% mutate(AFP = str_trim((str_remove_all(AFP, "\\(\\d+\\)"))),  
  AÑO = as.numeric(AÑO), `VALOR FONDOS` = as.double(`VALOR FONDOS`)) %>% filter(!is.na(`VALOR FONDOS`) & AFP != "TOTAL")

saveRDS(valor_fondos_pensiones_anuales_afp.xls, "data/pensiones/grafico_afps/valor_fondos_pensiones_anuales_afp.rds")
rm(valor_fondos_pensiones_anuales_afp.xls, valor_fondos_pensiones.xls)

# Bases de Datos Gráficos 8 - 10 ------------------------------------------

serie_pilar1.xls <- lista_archivos_POSIXct_general$serie_pilar1.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar1.xls")

serie_pilar1_pbs_invalidez.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PBS Invalidez" & month(AÑO) == 12)

saveRDS(serie_pilar1_pbs_invalidez.xls, "data/pensiones/grafico_8/serie_pilar1_pbs_invalidez.rds")
rm(serie_pilar1_pbs_invalidez.xls)

serie_pilar1_pbs_vejez_pgu_noncon.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PBS Vejez / PGU No Contributiva1" & month(AÑO) == 12)

saveRDS(serie_pilar1_pbs_vejez_pgu_noncon.xls, "data/pensiones/grafico_8/serie_pilar1_pbs_vejez_pgu_noncon.rds")
rm(serie_pilar1_pbs_vejez_pgu_noncon.xls)

serie_pilar1_aps_invalidez.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "APS Invalidez" & month(AÑO) == 12)

saveRDS(serie_pilar1_aps_invalidez.xls, "data/pensiones/grafico_8/serie_pilar1_aps_invalidez.rds")
rm(serie_pilar1_aps_invalidez.xls)

serie_pilar1_aps_vejez.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "APS Vejez 2" & month(AÑO) == 12)

saveRDS(serie_pilar1_aps_vejez.xls, "data/pensiones/grafico_8/serie_pilar1_aps_vejez.rds")
rm(serie_pilar1_aps_vejez.xls)

serie_pilar1_articulo9.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "Artículo 9°bis 3" & month(AÑO) == 12)

saveRDS(serie_pilar1_articulo9.xls, "data/pensiones/grafico_8/serie_pilar1_articulo9.rds")
rm(serie_pilar1_articulo9.xls)

serie_pilar1_pgu_contributiva.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO) %>% 
  arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PGU Contributiva 1" & month(AÑO) == 12)

saveRDS(serie_pilar1_pgu_contributiva.xls, "data/pensiones/grafico_8/serie_pilar1_pgu_contributiva.rds")
rm(serie_pilar1_pgu_contributiva.xls)

serie_pilar3.xls <- lista_archivos_POSIXct_general$serie_pilar3.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar3.xls")

serie_pilar3_pbs_invalidez.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PBS Invalidez" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_pbs_invalidez.xls, "data/pensiones/grafico_9/serie_pilar3_pbs_invalidez.rds")
rm(serie_pilar3_pbs_invalidez.xls)
  
serie_pilar3_pbs_vejez_pgu_noncon.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PBS Vejez / PGU No Contributiva1" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_pbs_vejez_pgu_noncon.xls, "data/pensiones/grafico_9/serie_pilar3_pbs_vejez_pgu_noncon.rds")
rm(serie_pilar3_pbs_vejez_pgu_noncon.xls)

serie_pilar3_aps_invalidez.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "APS Invalidez" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_aps_invalidez.xls, "data/pensiones/grafico_9/serie_pilar3_aps_invalidez.rds")
rm(serie_pilar3_aps_invalidez.xls)

serie_pilar3_aps_vejez.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "APS Vejez2" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_aps_vejez.xls, "data/pensiones/grafico_9/serie_pilar3_aps_vejez.rds")
rm(serie_pilar3_aps_vejez.xls)

serie_pilar3_articulo9.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "Artículo 9°bis3" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_articulo9.xls, "data/pensiones/grafico_9/serie_pilar3_articulo9.rds")
rm(serie_pilar3_articulo9.xls)

serie_pilar3_pgu_contributiva.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% 
  arrange(`FECHA`, `REGIÓN`, `TIPO DE BENEFICIO`) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% rename(AÑO = FECHA) %>% 
  mutate(AÑO = as.Date(AÑO)) %>% filter(`TIPO DE BENEFICIO` == "PGU Contributiva1" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`))

saveRDS(serie_pilar3_pgu_contributiva.xls, "data/pensiones/grafico_10/serie_pilar3_pgu_contributiva.rds")
rm(serie_pilar3_pgu_contributiva.xls)

serie_pilar5.xls <- lista_archivos_POSIXct_general$serie_pilar5.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar5.xls")

serie_pilar5_pbs_invalidez.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>% 
  filter(`TIPO DE BENEFICIO` == "PBS Invalidez4" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_pbs_invalidez.xls, "data/pensiones/grafico_10/serie_pilar5_pbs_invalidez.rds")
rm(serie_pilar5_pbs_invalidez.xls)

serie_pilar5_pgu_noncon.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "PGU No Contributiva1y4" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_pgu_noncon.xls, "data/pensiones/grafico_10/serie_pilar5_pgu_noncon.rds")
rm(serie_pilar5_pgu_noncon.xls)

serie_pilar5_pgu_con.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "PGU Contributiva1" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_pgu_con.xls, "data/pensiones/grafico_10/serie_pilar5_pgu_con.rds")
rm(serie_pilar5_pgu_con.xls)

serie_pilar5_aps_invalidez.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "APS Invalidez" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_aps_invalidez.xls, "data/pensiones/grafico_10/serie_pilar5_aps_invalidez.rds")
rm(serie_pilar5_aps_invalidez.xls)

serie_pilar5_aps_vejez_pengan.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "APS Vejez Pensión Garantizada" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`),
  `TIPO DE ENTIDAD PRINCIPAL` = ifelse(`TIPO DE ENTIDAD PRINCIPAL` == "MUTUALIDADES, LEY 16,744", "MUTUALIDADES, LEY 16.744", `TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_aps_vejez_pengan.xls, "data/pensiones/grafico_10/serie_pilar5_aps_vejez_pengan.rds")
rm(serie_pilar5_aps_vejez_pengan.xls)

serie_pilar5_aps_vejez_pengan_sol.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "APS Vejez Pensión Garantizada solicitada desde el año 20205" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_aps_vejez_pengan_sol.xls, "data/pensiones/grafico_10/serie_pilar5_aps_vejez_pengan_sol.rds")
rm(serie_pilar5_aps_vejez_pengan_sol.xls)

serie_pilar5_aps_vejez_subdef.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "APS Vejez Subsidio Definido" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`),
  `TIPO DE ENTIDAD PRINCIPAL` = ifelse(`TIPO DE ENTIDAD PRINCIPAL` == "MUTUALIDADES, LEY 16,744", "MUTUALIDADES, LEY 16.744", `TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_aps_vejez_subdef.xls, "data/pensiones/grafico_10/serie_pilar5_aps_vejez_subdef.rds")
rm(serie_pilar5_aps_vejez_subdef.xls)

serie_pilar5_beneficio_art9.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE ENTIDAD PRINCIPAL` != "Total" & `TIPO DE ENTIDAD PRINCIPAL` != "TOTAL") %>% arrange(`FECHA`, `TIPO DE BENEFICIO`) %>% rename(AÑO = FECHA) %>% mutate(AÑO = as.Date(AÑO)) %>%  
  filter(`TIPO DE BENEFICIO` == "Beneficio artículo 9° bis5" & month(AÑO) == 12) %>% select(-c(SEXO, `TIPO DE BENEFICIO`)) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`))

saveRDS(serie_pilar5_beneficio_art9.xls, "data/pensiones/grafico_10/serie_pilar5_beneficio_art9.rds")
rm(serie_pilar5_beneficio_art9.xls)

# Bases de Datos Gráfico Pilar - PGU --------------------------------------

serie_pilar1_totales.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
  names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% rename(AÑO = FECHA) %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD), AÑO = as.Date(AÑO)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & 
  `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO)

saveRDS(serie_pilar1_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar1_totales.rds")
rm(serie_pilar1_totales.xls)

serie_pilar2.xls <- lista_archivos_POSIXct_general$serie_pilar2.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar2.xls")

serie_pilar2_totales.xls <- serie_pilar2.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
  names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% rename(AÑO = FECHA) %>% mutate(CANTIDAD = as.integer(CANTIDAD), 
  CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD), AÑO = as.Date(AÑO)) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total") %>% select(-SEXO)

saveRDS(serie_pilar2_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar2_totales.rds")
rm(serie_pilar2_totales.xls)

serie_pilar3_totales.xls <- serie_pilar3.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% rename(AÑO = FECHA) %>% 
  filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% select(-SEXO) %>% mutate(AÑO = as.Date(AÑO)) %>% 
  mutate(MACROZONA = case_when(REGIÓN == "I. Región de Tarapacá" | REGIÓN == "II. Región de Antofagasta" | REGIÓN == "III. Región de Atacama" | REGIÓN == "XV. Región de Arica y Parinacota" ~ "Norte",
                               REGIÓN == "IV. Región de Coquimbo" | REGIÓN == "V. Región de Valparaíso" | REGIÓN == "XIII. Región Metropolitana de Santiago" | 
                                 REGIÓN == "VI. Región del Libertador General Bernardo O'Higgins" | REGIÓN == "Sin información" ~ "Centro Norte",
                               REGIÓN == "VII. Región deL Maule" | REGIÓN == "XVI. Región de Ñuble" | REGIÓN == "VIII. Región deL Biobío" | REGIÓN == "IX. Región de La Araucanía" ~ "Centro Sur",
                               REGIÓN == "XIV. Región de Los Ríos" | REGIÓN == "X. Región de Los Lagos" ~ "Sur",
                               REGIÓN == "XI. Región de Aysén del General Carlos Ibañez del Campo" | REGIÓN == "XII. Región de Magallanes y de la Antártica Chilena" ~ "Austral",
                               TRUE ~ NA)) 

saveRDS(serie_pilar3_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar3_totales.rds")
rm(serie_pilar3_totales.xls)

serie_pilar4.xls <- lista_archivos_POSIXct_general$serie_pilar4.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar4.xls")

serie_pilar4_totales.xls <- serie_pilar4.xls %>% nombrar_variables2() %>% tail(-1) %>% pivot_longer(cols = c(3:111), names_to = c("REGIÓN", "TIPO DE BENEFICIO"), 
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.integer(CANTIDAD), CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD)) %>% rename(AÑO = FECHA) %>% 
  filter(!is.na(CANTIDAD) & SEXO == "Total" & REGIÓN != "Totales por tipo de beneficio") %>% select(-SEXO) %>% mutate(AÑO = as.Date(AÑO)) %>% 
  mutate(MACROZONA = case_when(REGIÓN == "I. Región de Tarapacá" | REGIÓN == "II. Región de Antofagasta" | REGIÓN == "III. Región de Atacama" | REGIÓN == "XV. Región de Arica y Parinacota" ~ "Norte",
                               REGIÓN == "IV. Región de Coquimbo" | REGIÓN == "V. Región de Valparaíso" | REGIÓN == "XIII. Región Metropolitana de Santiago" | 
                                 REGIÓN == "VI. Región del Libertador General Bernardo O'Higgins" | REGIÓN == "Sin información" ~ "Centro Norte",
                               REGIÓN == "VII. Región deL Maule" | REGIÓN == "XVI. Región de Ñuble" | REGIÓN == "VIII. Región deL Biobío" | REGIÓN == "IX. Región de La Araucanía" ~ "Centro Sur",
                               REGIÓN == "XIV. Región de Los Ríos" | REGIÓN == "X. Región de Los Lagos" ~ "Sur",
                               REGIÓN == "XI. Región de Aysén del General Carlos Ibañez del Campo" | REGIÓN == "XII. Región de Magallanes y de la Antártica Chilena" ~ "Austral",
                               TRUE ~ NA))

saveRDS(serie_pilar4_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar4_totales.rds")
rm(serie_pilar4_totales.xls)

serie_pilar5_totales.xls <- serie_pilar5.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"),
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD), CANTIDAD = as.double(CANTIDAD)) %>% 
  rename(AÑO = FECHA) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total Mujeres" & !str_detect(`TIPO DE ENTIDAD PRINCIPAL`, "Total|TOTAL")) %>% 
  select(-SEXO) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`), 
  `TIPO DE ENTIDAD PRINCIPAL` = ifelse(`TIPO DE ENTIDAD PRINCIPAL` == "MUTUALIDADES, LEY 16,744", "MUTUALIDADES, LEY 16.744", `TIPO DE ENTIDAD PRINCIPAL`), AÑO = as.Date(AÑO))

saveRDS(serie_pilar5_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar5_totales.rds")
rm(serie_pilar5_totales.xls)

serie_pilar6.xls <- lista_archivos_POSIXct_general$serie_pilar6.xls
lista_archivos_POSIXct_general <- lista_archivos_POSIXct_general %>% discard_at("serie_pilar6.xls")

serie_pilar6_totales.xls <- serie_pilar6.xls %>% nombrar_variables3() %>% tail(-1) %>% pivot_longer(cols = c(3:32), names_to = c("SEXO", "TIPO DE BENEFICIO"),
  names_pattern = "(.*)_(.*)", values_to = "CANTIDAD") %>% mutate(CANTIDAD = case_when(CANTIDAD == 0 ~ NA, TRUE ~ CANTIDAD), CANTIDAD = round(as.double(CANTIDAD), digits = 2)) %>% 
  rename(AÑO = FECHA) %>% filter(!is.na(CANTIDAD) & SEXO == "Total" & `TIPO DE BENEFICIO` != "Total Mujeres" & !str_detect(`TIPO DE ENTIDAD PRINCIPAL`, "Total|TOTAL")) %>% 
  select(-SEXO) %>% mutate(`TIPO DE ENTIDAD PRINCIPAL` = toupper(`TIPO DE ENTIDAD PRINCIPAL`), `TIPO DE ENTIDAD PRINCIPAL` = ifelse(`TIPO DE ENTIDAD PRINCIPAL` == "MUTUALIDADES, LEY 16,744",
  "MUTUALIDADES, LEY 16.744", `TIPO DE ENTIDAD PRINCIPAL`), AÑO = as.Date(AÑO))

saveRDS(serie_pilar6_totales.xls, "data/pensiones/grafico_pilarpgu/serie_pilar6_totales.rds")
rm(serie_pilar6_totales.xls)

# Base de Datos Gráfico Género 2 ------------------------------------------

serie_pilar1_totales_sexo.xls <- serie_pilar1.xls %>% nombrar_variables1() %>% tail(-1) %>% select(1:22) %>% 
  pivot_longer(cols = starts_with("Femenino") | starts_with("Masculino") | starts_with("Total"), names_to = c("SEXO", "TIPO DE BENEFICIO"),
               names_pattern = "(Femenino|Masculino|Total)\\s*(.*)", values_to = "CANTIDAD") %>% rename(AÑO = FECHA) %>% 
  mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD), AÑO = as.Date(AÑO)) %>% filter(!is.na(CANTIDAD) & month(AÑO) == 12 & SEXO != "Total")

saveRDS(serie_pilar1_totales_sexo.xls, "data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds")
rm(serie_pilar1_totales_sexo.xls, serie_pilar1.xls, serie_pilar2.xls, serie_pilar3.xls, serie_pilar4.xls, serie_pilar5.xls, serie_pilar6.xls,
   lista_archivos_general, lista_archivos_POSIXct_afil_cot, lista_archivos_POSIXct_general, nombres_variables)
