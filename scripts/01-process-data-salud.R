source("scripts/01-process-data-pen.R")

# Procesamiento de Datos --------------------------------------------------

# Bases de Datos Gráfico 1 - 2 --------------------------------------------

base_beneficiarios_salud <- read_xlsx(drive_download("Boletin Estadistico 2017-2018.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "C01")
file.remove("Boletin Estadistico 2017-2018.xlsx")

beneficiarios_fonasa <- base_beneficiarios_salud %>% mutate(`TABLA C01` = as.double(`TABLA C01`)) %>% filter(between(`TABLA C01`, 1990, 2040)) %>% select(1, 2) %>%  
  rename(AÑO = `TABLA C01`, BENEFICIARIOS = ...2) %>% mutate(BENEFICIARIOS = as.double(BENEFICIARIOS))

saveRDS(beneficiarios_fonasa, "data/salud/grafico_1/beneficiarios_fonasa.rds")

beneficiarios_isapres <- base_beneficiarios_salud %>% mutate(`TABLA C01` = as.double(`TABLA C01`)) %>% filter(between(`TABLA C01`, 1990, 2040)) %>% select(1, 4) %>%  
  rename(AÑO = `TABLA C01`, BENEFICIARIOS = ...4) %>% mutate(BENEFICIARIOS = as.double(BENEFICIARIOS))

saveRDS(beneficiarios_isapres, "data/salud/grafico_2/beneficiarios_isapres.rds")
rm(base_beneficiarios_salud, beneficiarios_fonasa, beneficiarios_isapres)

# Bases de Datos Gráfico Género -------------------------------------------

beneficiarios_fonasa_genero <- c(map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, 
                                     ~ read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C02")) %>% 
                                   set_names(drive_find(pattern = "fonasa", type = "folder")$name), 
                                 map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name, 
                                     ~ read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C02")) %>% 
                                   set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name))

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

beneficiarios_fonasa_genero <- beneficiarios_fonasa_genero %>% map(~.x %>% tail(-3) %>% filter(str_detect(.[[1]], "GRUPOS") | str_detect(.[[1]], "EDAD") | str_detect(.[[1]], "TOTAL")) %>% 
                                                                     select(-c(1,4,7,10)) %>% t() %>% as_tibble() %>% rename(AÑO = "V1", GENERO = "V2", CANTIDAD = "V3")) %>% bind_rows() %>% `rownames<-`(NULL) %>% arrange(AÑO) %>%  
  mutate(GENERO = ifelse(GENERO == "AMBOS SEXOS", "TOTAL", GENERO), CANTIDAD = as.double(ifelse(CANTIDAD == 0, NA, CANTIDAD)), AÑO = as.integer(AÑO)) %>% filter(!is.na(CANTIDAD)) %>% 
  distinct(AÑO, GENERO, .keep_all = TRUE)

saveRDS(beneficiarios_fonasa_genero, "data/salud/grafico_genero_salud/beneficiarios_fonasa_genero.rds")
rm(beneficiarios_fonasa_genero)

cotizantes_fonasa_genero_1 <- map(drive_find(pattern = "Boletin Estadistico (2014|2012)-(2015|2013)", type = "xls")$name, 
                                  ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C08")) %>% 
  set_names(drive_find(pattern = "Boletin Estadistico (2014|2012)-(2015|2013)", type = "xls")$name)

cotizantes_fonasa_genero_2 <- map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name, 
                                  ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C08")) %>% 
  set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

cotizantes_fonasa_genero <- c(cotizantes_fonasa_genero_1, cotizantes_fonasa_genero_2)

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

cotizantes_fonasa_genero_1 <- cotizantes_fonasa_genero %>% keep_at(c("Boletin Estadistico 2014-2015.xls", "Boletin Estadistico 2016-2017.xlsx", "Boletin Estadistico 2017-2018.xlsx"))
cotizantes_fonasa_genero_2 <- cotizantes_fonasa_genero %>% discard_at(c("Boletin Estadistico 2014-2015.xls", "Boletin Estadistico 2016-2017.xlsx", "Boletin Estadistico 2017-2018.xlsx"))

cotizantes_fonasa_genero_1 <- cotizantes_fonasa_genero_1 %>% map(~.x %>% filter(str_detect(.[[1]], "Grupo") | str_detect(.[[1]], "TOTAL") | row_number() == 4) %>% t() %>% as_tibble()) %>% 
  bind_rows() %>% mutate(V1 = case_when(row_number() >= 2 & row_number() <= 4 ~ "2012", row_number() >= 5 & row_number() <= 8 ~ "2013", row_number() >= 9 & row_number() <= 12 ~ "2014", 
                                        row_number() >= 13 & row_number() <= 16 ~ "2015", row_number() >= 18 & row_number() <= 21 ~ "2014", row_number() >= 22 & row_number() <= 25 ~ "2015",
                                        row_number() >= 26 & row_number() <= 29 ~ "2016", row_number() >= 30 & row_number() <= 33 ~ "2017", row_number() >= 35 & row_number() <= 38 ~ "2015", 
                                        row_number() >= 39 & row_number() <= 42 ~ "2016", row_number() >= 43 & row_number() <= 46 ~ "2017", row_number() >= 47 & row_number() <= 50 ~ "2018", TRUE ~ V1)) %>% 
  filter(V3 != "TOTAL") %>% rename(AÑO = "V1", GENERO = "V2", CANTIDAD = "V3") %>% distinct(AÑO, GENERO, CANTIDAD) %>% mutate(AÑO = as.integer(AÑO), GENERO = toupper(GENERO), 
                                                                                                                              CANTIDAD = as.double(CANTIDAD), GENERO = case_when(!str_detect(GENERO, "MUJER|HOMBRE|TOTAL") ~ "INDETERMINADO", TRUE ~ GENERO))

cotizantes_fonasa_genero_2 <- cotizantes_fonasa_genero_2 %>% map(~.x %>% filter(str_detect(.[[1]], "DICIEMBRE") | str_detect(.[[1]], "TOTAL") | str_detect(.[[4]], "Total") | 
  str_detect(.[[ncol(.)]], "Total")) %>% t() %>% as_tibble() %>% mutate(V1 = ifelse(row_number() >= 1, V1[1], V1), V4 = ifelse(row_number() >= 1, V4[1], V4))) %>% bind_rows() %>% 
  bind_rows(.[1:3], .[4:6]) %>% mutate(AÑO = as.integer(str_remove_all(coalesce(V1,V4), "A  DICIEMBRE ")), GENERO = toupper(coalesce(V2, V5)), 
                                       GENERO = case_when(!str_detect(GENERO, "MUJER|HOMBRE|TOTAL") ~ "INDETERMINADO", TRUE ~ GENERO), CANTIDAD = as.double(coalesce(V3,V6))) %>% select(-c(1:6)) %>% filter(CANTIDAD != "TOTAL") %>%
  distinct(AÑO, GENERO, .keep_all = TRUE) %>% arrange(AÑO)

cotizantes_fonasa_genero <- bind_rows(cotizantes_fonasa_genero_1, cotizantes_fonasa_genero_2) %>% arrange(AÑO) %>% distinct(AÑO, GENERO, .keep_all = TRUE)

saveRDS(cotizantes_fonasa_genero, "data/salud/grafico_genero_salud/cotizantes_fonasa_genero.rds")
rm(cotizantes_fonasa_genero, cotizantes_fonasa_genero_1, cotizantes_fonasa_genero_2)

beneficiarios_isapres_genero <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Sexo")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx") 

beneficiarios_isapres_genero <- beneficiarios_isapres_genero %>% tail(4) %>% setNames(beneficiarios_isapres_genero[28, ]) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% rename(SEXO = Sexo) %>% mutate(AÑO = as.numeric(AÑO), 
                                                                                                                    SEXO = ifelse(SEXO == "Sin clasificar", "Sin Información", SEXO)) %>% filter(!is.na(CANTIDAD))

saveRDS(beneficiarios_isapres_genero, "data/salud/grafico_genero_salud/beneficiarios_isapres_genero.rds")
rm(beneficiarios_isapres_genero)

cotizantes_isapres_genero <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Sexo")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx") 

cotizantes_isapres_genero <- cotizantes_isapres_genero %>% head(8) %>% setNames(cotizantes_isapres_genero[4, ]) %>% tail(-4) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% rename(SEXO = Sexo) %>% mutate(AÑO = as.numeric(AÑO), 
                                                                                                                    SEXO = ifelse(SEXO == "Sin clasificar", "Sin Información", SEXO), CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_isapres_genero, "data/salud/grafico_genero_salud/cotizantes_isapres_genero.rds")
rm(cotizantes_isapres_genero)

# Bases de Datos Gráfico General ------------------------------------------

beneficiarios_salud <- read_xlsx(drive_download("Boletin Estadistico 2017-2018.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "C01")
file.remove("Boletin Estadistico 2017-2018.xlsx")

beneficiarios_salud <- beneficiarios_salud %>% select(1, 2, 4) %>% tail(-6) %>% rename(AÑO = "TABLA C01", FONASA = "...2", ISAPRE = "...4") %>% head(-15) %>% 
              mutate(AÑO = as.integer(str_remove_all(AÑO, "\\(\\d+\\)"))) %>% pivot_longer(cols = c(2,3), names_to = "COBERTURA", values_to = "CANTIDAD") %>% 
              mutate(CANTIDAD = as.double(CANTIDAD))

saveRDS(beneficiarios_salud, "data/salud/grafico_general_salud/beneficiarios_salud.rds")
rm(beneficiarios_salud)

cotizantes_fonasa_totales <- read_rds("data/salud/grafico_genero_salud/cotizantes_fonasa_genero.rds") %>% filter(GENERO == "TOTAL")  %>% rename(COBERTURA = GENERO) %>% 
  mutate(COBERTURA = "FONASA")

cotizantes_isapres_totales <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera Anual de Isapre")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")

cotizantes_isapres_totales <- cotizantes_isapres_totales %>% head(6) %>% tail(-3) %>% setNames(cotizantes_isapres_totales[3, ]) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% filter(Isapres == "Total Sistema") %>% rename(COBERTURA = Isapres) %>% 
  mutate(COBERTURA = "ISAPRE", AÑO = as.integer(AÑO)) %>% filter(between(AÑO, 2009, 2018))

cotizantes_salud <- bind_rows(cotizantes_fonasa_totales, cotizantes_isapres_totales)

saveRDS(cotizantes_salud, "data/salud/grafico_general_salud/cotizantes_salud.rds")
rm(cotizantes_salud, cotizantes_fonasa_totales, cotizantes_isapres_totales)

beneficiarios_fonasa_region <- c(map(drive_find(pattern = "Boletin Estadistico", type = "xls")$name, 
                                     ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C03")) %>% 
                                   set_names(drive_find(pattern = "Boletin Estadistico", type = "xls")$name), 
                                 map(drive_find(pattern = "Boletin Estadistico", type = "xlsx")$name, 
                                     ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C03")) %>% 
                                   set_names(drive_find(pattern = "Boletin Estadistico", type = "xlsx")$name))

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

anios <- c(2011, 2014, 2015, 2012, 2009, 2010, 2017, 2016)

beneficiarios_fonasa_region <- beneficiarios_fonasa_region %>% map2(., anios, ~.x %>% mutate(AÑO = case_when(row_number() >= 25 ~ .y + 1, row_number() <= 24 ~ .y))) %>% 
  bind_rows() %>% select(1, 16, 14) %>% rename(REGIÓN = `TABLA C03a`, FONASA = "...14") %>% filter(REGIÓN %in% c("I", "II", "III", "IV", "V", "VI",  "VII",  "VIII", "IX", 
  "X", "XI", "XII", "XIII", "XIV", "XV", "Sin Ubicación", "Total Nacional")) %>% transformar_variable() %>% distinct(AÑO, FONASA, REGIÓN) %>% arrange(AÑO) %>% 
  mutate(REGIÓN = ifelse(REGIÓN == "Total Nacional", "Total", REGIÓN), AÑO = as.integer(AÑO), FONASA = as.integer(FONASA))

beneficiarios_isapres_region <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Región")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")  
  
beneficiarios_isapres_region <- beneficiarios_isapres_region %>% tail(18) %>% setNames(replace(unlist(beneficiarios_isapres_region[56, ]), 1, "REGIÓN")) %>% select(-c(2)) %>% 
  mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "ISAPRE") %>% transformar_variable() %>% filter(!is.na(ISAPRE)) %>% 
  mutate(AÑO = as.integer(AÑO), ISAPRE = as.integer(ISAPRE))

beneficiarios_salud_region <- left_join(beneficiarios_fonasa_region, beneficiarios_isapres_region, by = c("AÑO", "REGIÓN")) %>% 
  pivot_longer(cols = c("FONASA", "ISAPRE"), names_to = "COBERTURA", values_to = "CANTIDAD") %>% arrange(AÑO) %>% 
  mutate(MACROZONA = case_when(REGIÓN == "I. Región de Tarapacá" | REGIÓN == "II. Región de Antofagasta" | REGIÓN == "III. Región de Atacama" | REGIÓN == "XV. Región de Arica y Parinacota" ~ "Norte",
                               REGIÓN == "IV. Región de Coquimbo" | REGIÓN == "V. Región de Valparaíso" | REGIÓN == "XIII. Región Metropolitana de Santiago" | 
                                 REGIÓN == "VI. Región del Libertador General Bernardo O'Higgins" | REGIÓN == "Sin información" ~ "Centro Norte",
                               REGIÓN == "VII. Región deL Maule" | REGIÓN == "XVI. Región de Ñuble" | REGIÓN == "VIII. Región deL Biobío" | REGIÓN == "IX. Región de La Araucanía" ~ "Centro Sur",
                               REGIÓN == "XIV. Región de Los Ríos" | REGIÓN == "X. Región de Los Lagos" ~ "Sur",
                               REGIÓN == "XI. Región de Aysén del General Carlos Ibañez del Campo" | REGIÓN == "XII. Región de Magallanes y de la Antártica Chilena" ~ "Austral",
                               TRUE ~ NA))

saveRDS(beneficiarios_salud_region, "data/salud/grafico_general_salud/beneficiarios_salud_region.rds")
rm(beneficiarios_salud_region, beneficiarios_fonasa_region, beneficiarios_isapres_region, anios)

cotizantes_fonasa_region <- c(map(drive_find(pattern = "Boletin Estadistico (201[0-4]|201[6-7])-(201[0-8])", type = "xls")$name, 
                                  ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C06")) %>% 
                                   set_names(drive_find(pattern = "Boletin Estadistico (201[0-4]|201[6-7])-(201[0-8])", type = "xls")$name), 
                                 map(drive_find(pattern = "Boletin Estadistico (201[0-4]|201[6-7])-(201[0-8])", type = "xlsx")$name, 
                                     ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C06")) %>% 
                                   set_names(drive_find(pattern = "Boletin Estadistico (201[0-4]|201[6-7])-(201[0-8])", type = "xlsx")$name))

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

cotizantes_fonasa_region_1 <- cotizantes_fonasa_region %>% keep_at(c("Boletin Estadistico 2014-2015.xls","Boletin Estadistico 2016-2017.xlsx","Boletin Estadistico 2017-2018.xlsx"))
cotizantes_fonasa_region_2 <- cotizantes_fonasa_region %>% discard_at(c("Boletin Estadistico 2014-2015.xls","Boletin Estadistico 2016-2017.xlsx","Boletin Estadistico 2017-2018.xlsx"))

cotizantes_fonasa_region_1 <- cotizantes_fonasa_region_1 %>% map(~.x %>% rellenar_variables() %>% tail(-4) %>% setNames(.[1,]) %>% tail(-4) %>% head(-8) %>% select(1, 4, 6, 8, 10) %>% 
                                pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "FONASA")) %>% bind_rows() %>% rename(REGIÓN = `Región (1)`) %>% filter(!is.na(FONASA)) %>% 
                                  transformar_variable() %>% mutate(REGIÓN = ifelse(REGIÓN == "TOTAL NACIONAL", "Total", REGIÓN)) %>% distinct(REGIÓN, AÑO, FONASA) %>% arrange(AÑO)

cotizantes_fonasa_region_2 <- cotizantes_fonasa_region_2 %>% map(~.x %>% rellenar_variables() %>% tail(-4) %>% select(1, 4, 6) %>% setNames(.[1,]) %>% tail(-4) %>% head(-7) %>% 
                                     pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "FONASA")) %>% bind_rows() %>% rename(REGIÓN = `Región (1)`) %>% filter(!is.na(FONASA)) %>% 
                                        transformar_variable()
  
cotizantes_fonasa_region_2_1 <- cotizantes_fonasa_region_2 %>% head(34) %>% filter(AÑO != 2011)
cotizantes_fonasa_region_2_2 <- cotizantes_fonasa_region_2 %>% tail(-34) 

cotizantes_fonasa_region_2 <- bind_rows(cotizantes_fonasa_region_2_1, cotizantes_fonasa_region_2_2) %>% mutate(REGIÓN = ifelse(REGIÓN == "TOTAL NACIONAL", "Total", REGIÓN)) %>%
  distinct(REGIÓN, AÑO, FONASA) %>% arrange(AÑO)

cotizantes_fonasa_region <- bind_rows(cotizantes_fonasa_region_1, cotizantes_fonasa_region_2) %>% arrange(AÑO) %>% 
  mutate(MACROZONA = case_when(REGIÓN == "I. Región de Tarapacá" | REGIÓN == "II. Región de Antofagasta" | REGIÓN == "III. Región de Atacama" | REGIÓN == "XV. Región de Arica y Parinacota" ~ "Norte",
                               REGIÓN == "IV. Región de Coquimbo" | REGIÓN == "V. Región de Valparaíso" | REGIÓN == "XIII. Región Metropolitana de Santiago" | 
                                 REGIÓN == "VI. Región del Libertador General Bernardo O'Higgins" | REGIÓN == "Sin Información" ~ "Centro Norte",
                               REGIÓN == "VII. Región deL Maule" | REGIÓN == "XVI. Región de Ñuble" | REGIÓN == "VIII. Región deL Biobío" | REGIÓN == "IX. Región de La Araucanía" ~ "Centro Sur",
                               REGIÓN == "XIV. Región de Los Ríos" | REGIÓN == "X. Región de Los Lagos" ~ "Sur",
                               REGIÓN == "XI. Región de Aysén del General Carlos Ibañez del Campo" | REGIÓN == "XII. Región de Magallanes y de la Antártica Chilena" ~ "Austral",
                               TRUE ~ "Nacional"), AÑO = as.integer(AÑO), FONASA = as.integer(FONASA)) %>% distinct(REGIÓN, AÑO, FONASA)

cotizantes_isapres_region <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Región")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")  

cotizantes_isapres_region <- cotizantes_isapres_region %>% head(22) %>% setNames(cotizantes_isapres_region[4, ]) %>% tail(-4) %>% select(-2) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  rename(REGIÓN = "Regiones") %>% transformar_variable() %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "ISAPRE") %>% filter(!is.na(ISAPRE)) %>% 
  mutate(MACROZONA = case_when(REGIÓN == "I. Región de Tarapacá" | REGIÓN == "II. Región de Antofagasta" | REGIÓN == "III. Región de Atacama" | REGIÓN == "XV. Región de Arica y Parinacota" ~ "Norte",
                               REGIÓN == "IV. Región de Coquimbo" | REGIÓN == "V. Región de Valparaíso" | REGIÓN == "XIII. Región Metropolitana de Santiago" | 
                                 REGIÓN == "VI. Región del Libertador General Bernardo O'Higgins" | REGIÓN == "Sin información" ~ "Centro Norte",
                               REGIÓN == "VII. Región deL Maule" | REGIÓN == "XVI. Región de Ñuble" | REGIÓN == "VIII. Región deL Biobío" | REGIÓN == "IX. Región de La Araucanía" ~ "Centro Sur",
                               REGIÓN == "XIV. Región de Los Ríos" | REGIÓN == "X. Región de Los Lagos" ~ "Sur", 
                               REGIÓN == "XI. Región de Aysén del General Carlos Ibañez del Campo" | REGIÓN == "XII. Región de Magallanes y de la Antártica Chilena" ~ "Austral",
                               TRUE ~ "Nacional"), AÑO = as.integer(AÑO))

cotizantes_salud_region <- left_join(cotizantes_fonasa_region, cotizantes_isapres_region) %>% mutate(FONASA = as.double(FONASA), AÑO = as.integer(AÑO)) %>% 
                                  pivot_longer(cols = c("FONASA", "ISAPRE"), names_to = "COBERTURA", values_to = "CANTIDAD") %>% filter(!is.na(CANTIDAD)) %>% 
  select(AÑO, REGIÓN, COBERTURA, CANTIDAD, MACROZONA) %>% arrange(AÑO)

saveRDS(cotizantes_salud_region, "data/salud/grafico_general_salud/cotizantes_salud_region.rds")
rm(cotizantes_salud_region, cotizantes_isapres_region, cotizantes_fonasa_region, cotizantes_fonasa_region_1, cotizantes_fonasa_region_2, cotizantes_fonasa_region_2_1, 
   cotizantes_fonasa_region_2_2)

beneficiarios_fonasa_edad <- c(map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C02")) %>% 
                                      set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name), 
                               map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name, ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C02")) %>% 
                                 set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name))

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name) 

beneficiarios_fonasa_edad <- beneficiarios_fonasa_edad %>% map(~.x %>% select(1:3) %>% setNames(replace(unlist(.[5, ]), 1, "RANGO_EDAD")) %>% tail(-6) %>% filter(row_number() <= which(str_detect(RANGO_EDAD, "TOTAL"))[1]) %>% 
                                  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD")) %>% bind_rows() %>% arrange(AÑO) %>% mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD), 
                                    RANGO_EDAD = ifelse(RANGO_EDAD == "Sin Edad", "Sin Información", RANGO_EDAD)) %>% filter(!is.na(CANTIDAD)) %>% filter(!row_number() == 263) %>% distinct(RANGO_EDAD, AÑO, CANTIDAD)

saveRDS(beneficiarios_fonasa_edad, "data/salud/grafico_general_salud/beneficiarios_fonasa_edad_total.rds")
rm(beneficiarios_fonasa_edad)

cotizantes_fonasa_edad_1 <- map(drive_find(pattern = "Boletin Estadistico (2009|201[0-4]|201[6-7])-(201[0-8])", type = "xls")$name, 
                               ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C08")) %>% 
  set_names(drive_find(pattern = "Boletin Estadistico (2009|201[0-4]|201[6-7])-(201[0-8])", type = "xls")$name)

cotizantes_fonasa_edad_2 <- map(drive_find(pattern = "Boletin Estadistico (2009|201[0-4]|201[6-7])-(201[0-8])", type = "xlsx")$name, 
                                 ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C08")) %>% 
  set_names(drive_find(pattern = "Boletin Estadistico (2009|201[0-4]|201[6-7])-(201[0-8])", type = "xlsx")$name)

cotizantes_fonasa_edad <- c(cotizantes_fonasa_edad_1, cotizantes_fonasa_edad_2)

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name, 
            drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

rm(cotizantes_fonasa_edad_1, cotizantes_fonasa_edad_2)

cotizantes_fonasa_edad_1 <- cotizantes_fonasa_edad %>% keep_at(c("Boletin Estadistico 2014-2015.xls","Boletin Estadistico 2016-2017.xlsx","Boletin Estadistico 2017-2018.xlsx"))
cotizantes_fonasa_edad_2 <- cotizantes_fonasa_edad %>% discard_at(c("Boletin Estadistico 2014-2015.xls","Boletin Estadistico 2016-2017.xlsx","Boletin Estadistico 2017-2018.xlsx",
                                                                    "Boletin Estadistico 2010-2011.xlsx", "Boletin Estadistico 2012-2013.xls"))

cotizantes_fonasa_edad_1 <- cotizantes_fonasa_edad_1 %>% map(~.x %>% rellenar_variables1() %>% tail(-3) %>% head(-8) %>% filter(if_any(1:last_col(), ~ !is.na(.))) %>% t() %>% as_tibble() %>% 
                                                               setNames(replace(unlist(.[1, ]), 1, "AÑO")) %>% tail(-1) %>% pivot_longer(cols = c(3:last_col()), names_to = "RANGO_EDAD", values_to = "CANTIDAD") %>% 
                                                               rename(GENERO = "Grupo de Edad (años)")) %>% bind_rows() %>% mutate(AÑO = as.double(AÑO))

cotizantes_fonasa_edad_2$`Boletin Estadistico 2011-2012.xls` <- cotizantes_fonasa_edad_2$`Boletin Estadistico 2011-2012.xls` %>% head(24)

cotizantes_fonasa_edad_2 <- cotizantes_fonasa_edad_2 %>% bind_rows() %>% mutate(RANGO_EDAD = coalesce(`TABLA C08b`, `TABLA C08a`)) %>% select(-c(`TABLA C08b`, `TABLA C08a`)) %>%
  mutate(AÑO = case_when(row_number() >= 7 & row_number() <= 23 ~ 2011, row_number() >= 30 & row_number() <= 45 ~ 2009, row_number() >= 54 & row_number() <= 69 ~ 2010, TRUE ~ NA)) %>%
  rename("Hombre" =...2, "Mujer" = ...3, "Total" = ...4) %>% filter(!is.na(AÑO) & !is.na(Total)) %>% pivot_longer(cols = c(1:3), names_to = "GENERO", values_to = "CANTIDAD")

cotizantes_fonasa_edad_total <- bind_rows(cotizantes_fonasa_edad_1, cotizantes_fonasa_edad_2) %>% mutate(CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>%
  filter(!is.na(CANTIDAD) & GENERO == "Total") %>% arrange(AÑO) %>% distinct(AÑO, RANGO_EDAD, GENERO, CANTIDAD) %>% 
  mutate(RANGO_EDAD = ifelse(RANGO_EDAD == "Sin Dato", "Sin Información", RANGO_EDAD))

saveRDS(cotizantes_fonasa_edad_total, "data/salud/grafico_general_salud/cotizantes_fonasa_edad_total.rds")
rm(cotizantes_fonasa_edad_total, cotizantes_fonasa_edad_1, cotizantes_fonasa_edad_2, cotizantes_fonasa_edad)

beneficiarios_isapres_edad_total <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Edad")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")

beneficiarios_isapres_edad_total <- beneficiarios_isapres_edad_total %>% tail(13) %>% setNames(.[1, ]) %>% head(-1) %>% tail(-1) %>% mutate(`Rangos de edad` = ifelse(is.na(`Rangos de edad`), "Sin Información", `Rangos de edad`)) %>% 
  mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% mutate(AÑO = as.integer(str_trim(str_remove(AÑO, "\\(\\*\\)")))) %>% 
  filter(!is.na(CANTIDAD)) %>% rename(RANGO_EDAD = `Rangos de edad`)

saveRDS(beneficiarios_isapres_edad_total, "data/salud/grafico_general_salud/beneficiarios_isapres_edad_total.rds")
rm(beneficiarios_isapres_edad_total)

cotizantes_isapres_edad_total <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera x Edad")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")

cotizantes_isapres_edad_total <- cotizantes_isapres_edad_total %>% head(15) %>% setNames(cotizantes_isapres_edad_total[4, ]) %>% tail(-4) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "CANTIDAD") %>% mutate(AÑO = as.integer(str_trim(str_remove(AÑO, "\\(\\*\\)"))), CANTIDAD = ifelse(CANTIDAD == 0, NA, CANTIDAD)) %>% 
  rename(RANGO_EDAD = `Rangos de edad`) %>% filter(!is.na(CANTIDAD))

saveRDS(cotizantes_isapres_edad_total, "data/salud/grafico_general_salud/cotizantes_isapre_edad_total.rds")
rm(cotizantes_isapres_edad_total)

cotizantes_fonasa_relacion_laboral_1 <- read_xlsx(drive_download("Boletin Estadistico 2017-2018.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "C05")
file.remove("Boletin Estadistico 2017-2018.xlsx")

cotizantes_fonasa_relacion_laboral_1 <- cotizantes_fonasa_relacion_laboral_1 %>% rellenar_variables2() %>% tail(-5) %>% setNames(.[1,]) %>% filter(str_detect(`Mes Remuneración`, "Diciembre"))

cotizantes_fonasa_relacion_laboral_2 <- read_xlsx(drive_download("Boletin Estadistico 2009-2010.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "C05")
file.remove("Boletin Estadistico 2009-2010.xlsx")

cotizantes_fonasa_relacion_laboral_2 <- cotizantes_fonasa_relacion_laboral_2 %>% select(1, 3) %>% tail(-7) %>% head(-7) %>% t() %>% as_tibble() %>% setNames(.[1, ]) %>% 
  mutate(Año = "2009", `Mes Remuneración` = "Diciembre", Indicador = "N° Personas", .before = Dependientes,) %>% tail(-1) %>% rename(`Dependientes y \r\nPensionados` = `Dependientes y Pensionados`)

cotizantes_fonasa_relacion_laboral <- bind_rows(cotizantes_fonasa_relacion_laboral_1, cotizantes_fonasa_relacion_laboral_2) %>% arrange(Año) %>% select(-c(`Mes Remuneración`, Indicador)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "TIPO DE RELACION LABORAL", values_to = "COTIZANTES POR TIPO DE RELACION LABORAL") %>% filter(!is.na("COTIZANTES POR TIPO DE RELACION LABORAL")) %>% 
  rename(AÑO = Año) %>% mutate(AÑO = as.integer(AÑO), `COTIZANTES POR TIPO DE RELACION LABORAL` = as.numeric(`COTIZANTES POR TIPO DE RELACION LABORAL`))

saveRDS(cotizantes_fonasa_relacion_laboral, "data/salud/grafico_general_salud/cotizantes_fonasa_relacion_laboral.rds")
rm(cotizantes_fonasa_relacion_laboral, cotizantes_fonasa_relacion_laboral_1, cotizantes_fonasa_relacion_laboral_2)

cotizantes_isapre_relacion_laboral <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cotizantes x Cond. Previsional")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")

cotizantes_isapre_relacion_laboral <- cotizantes_isapre_relacion_laboral %>% tail(8) %>% head(-1) %>% setNames(.[1, ]) %>% mutate(across(c(2:last_col()), as.numeric)) %>% tail(-1) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "COTIZANTES POR TIPO DE RELACION LABORAL") %>% rename("TIPO DE RELACION LABORAL" = `Condición Previsional`) %>% 
  mutate(AÑO = as.integer(AÑO), `COTIZANTES POR TIPO DE RELACION LABORAL` = ifelse(`COTIZANTES POR TIPO DE RELACION LABORAL` == 0, NA, `COTIZANTES POR TIPO DE RELACION LABORAL`)) %>% 
  filter(!is.na(`COTIZANTES POR TIPO DE RELACION LABORAL`))

saveRDS(cotizantes_isapre_relacion_laboral, "data/salud/grafico_general_salud/cotizantes_isapre_relacion_laboral.rds")
rm(cotizantes_isapre_relacion_laboral)
  
# Gráfico Treemap Fonasa --------------------------------------------------

fonasa_tramos_1 <- map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name,  ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C03")) %>% 
  set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name)

fonasa_tramos_2 <- map(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name,  ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C03")) %>% 
  set_names(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name)

file.remove(drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xlsx")$name, drive_ls(drive_find(pattern = "fonasa", type = "folder"), type = "xls")$name)

fonasa_tramos <- c(fonasa_tramos_1, fonasa_tramos_2)

fonasa_tramos <- fonasa_tramos %>% map(~.x %>% filter(row_number() >= which(str_detect(`TABLA C03a`, "Región"))[1]) %>% select(c(1,4,6,8,10,12,14)))

anios_inicio <- c(2011, 2014, 2015, 2012)

fonasa_tramos_1 <- map2(fonasa_tramos_1, anios_inicio, ~.x %>% mutate(AÑO = case_when(row_number() >= 25 ~ .y + 1, row_number() <= 24 ~ .y))) %>% 
  map(~.x %>% select(c(1, 4, 6, 8, 10, 16)) %>% setNames(replace(unlist(.[6, ]), 6, "AÑO")) %>% filter(`Región (2)` %in% c("I", "II", "III", "IV", "V",
  "VI",  "VII",  "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "Sin Ubicación", "Total Nacional"))) %>% bind_rows() %>% rename(REGIÓN = "Región (2)")

anios_inicio <- c(2009, 2016, 2017)

fonasa_tramos_2 <- fonasa_tramos_2 %>% discard_at("Boletin Estadistico 2010-2011.xlsx")

fonasa_tramos_2 <- map2(fonasa_tramos_2, anios_inicio, ~.x %>% mutate(AÑO = case_when(row_number() >= 25 ~ .y + 1, row_number() <= 24 ~ .y))) %>% 
  map(~.x %>% select(c(1, 4, 6, 8, 10, 16)) %>% setNames(replace(unlist(.[6, ]), 6, "AÑO")) %>% filter(.[[1]] %in% c("I", "II", "III", "IV", "V",
  "VI",  "VII",  "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "Sin Ubicación", "Total Nacional"))) %>% bind_rows() %>% mutate(REGIÓN = coalesce(Región, `Región (1)`), 
                                                                                                                                      .before = Región) %>% select(-c(Región, `Región (1)`))

fonasa_tramos <- bind_rows(fonasa_tramos_1, fonasa_tramos_2) %>% distinct(AÑO, REGIÓN, A, B, C, D, .keep_all = TRUE) %>% arrange(AÑO) %>% 
  pivot_longer(cols = c(2:5), names_to = "TRAMOS", values_to = "CANTIDAD") %>% mutate(CANTIDAD = as.double(CANTIDAD)) %>% transformar_variable() 

saveRDS(fonasa_tramos, "data/salud/grafico_treemap/beneficiarios_por_tramos.rds")
rm(fonasa_tramos, fonasa_tramos_1, fonasa_tramos_2, anios_inicio)

# Gráficos ISAPRE 1 -------------------------------------------------------

beneficiarios_salud_por_isapre <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Beneficiarios x Isapre")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx") 

beneficiarios_salud_por_isapre_abiertas <- beneficiarios_salud_por_isapre %>% select(-c(1)) %>% head(34) %>% tail(-2) %>% setNames(.[1, ]) %>% mutate(Isapres = ifelse(row_number() == 32, 
  "Total Isapres Abiertas", Isapres)) %>% tail(-1) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "BENEFICIARIOS ISAPRES ABIERTAS") %>% mutate(AÑO = as.integer(AÑO)) %>% 
  filter(!is.na(`BENEFICIARIOS ISAPRES ABIERTAS`))

names(beneficiarios_salud_por_isapre_abiertas) <- toupper(names(beneficiarios_salud_por_isapre_abiertas))
saveRDS(beneficiarios_salud_por_isapre_abiertas, "data/salud/grafico_isapre/beneficiarios_salud_por_isapre_abiertas.rds")

beneficiarios_salud_por_isapre_cerradas <- beneficiarios_salud_por_isapre %>% select(-c(1)) %>% setNames(.[3, ]) %>% tail(16) %>% mutate(Isapres = ifelse(row_number() == 15, 
  "Total Isapres Cerradas", Isapres), Isapres = ifelse(row_number() == 16, "Total Sistema", Isapres)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "BENEFICIARIOS ISAPRES CERRADAS") %>% 
  mutate(AÑO = as.integer(AÑO)) %>% filter(!is.na(`BENEFICIARIOS ISAPRES CERRADAS`)) %>% filter(Isapres != "Total Sistema")

names(beneficiarios_salud_por_isapre_cerradas) <- toupper(names(beneficiarios_salud_por_isapre_cerradas))
saveRDS(beneficiarios_salud_por_isapre_cerradas, "data/salud/grafico_isapre/beneficiarios_salud_por_isapre_cerradas.rds")
rm(beneficiarios_salud_por_isapre, beneficiarios_salud_por_isapre_abiertas, beneficiarios_salud_por_isapre_cerradas)

cotizantes_salud_por_isapre <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cotizantes x Isapre")
file.remove("Cartera Beneficiarios Isapre 1990-2023.xlsx")

cotizantes_salud_por_isapre_abiertas <- cotizantes_salud_por_isapre %>% select(-c(1)) %>% head(34) %>% tail(-2) %>% setNames(.[1, ]) %>% mutate(Isapres = ifelse(row_number() == 32, 
  "Total Isapres Abiertas", Isapres)) %>% tail(-1) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "COTIZANTES ISAPRES ABIERTAS") %>% mutate(AÑO = as.integer(AÑO),
  AÑO = ifelse(AÑO == 2204, 2004, AÑO)) %>% filter(!is.na(`COTIZANTES ISAPRES ABIERTAS`))

names(cotizantes_salud_por_isapre_abiertas) <- toupper(names(cotizantes_salud_por_isapre_abiertas))
saveRDS(cotizantes_salud_por_isapre_abiertas, "data/salud/grafico_isapre/cotizantes_salud_por_isapre_abiertas.rds")

cotizantes_salud_por_isapre_cerradas <- cotizantes_salud_por_isapre %>% select(-c(1)) %>% setNames(.[3, ]) %>% tail(16) %>% mutate(Isapres = ifelse(row_number() == 15, 
  "Total Isapres Cerradas", Isapres), Isapres = ifelse(row_number() == 16, "Total Sistema", Isapres)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "COTIZANTES ISAPRES CERRADAS") %>% 
  mutate(AÑO = as.integer(AÑO), AÑO = ifelse(AÑO == 2204, 2004, AÑO)) %>% filter(!is.na(`COTIZANTES ISAPRES CERRADAS`)) %>% filter(Isapres != "Total Sistema")

names(cotizantes_salud_por_isapre_cerradas) <- toupper(names(cotizantes_salud_por_isapre_cerradas))
saveRDS(cotizantes_salud_por_isapre_cerradas, "data/salud/grafico_isapre/cotizantes_salud_por_isapre_cerradas.rds")
rm(cotizantes_salud_por_isapre_abiertas, cotizantes_salud_por_isapre_cerradas, cotizantes_salud_por_isapre)

# Gráfico ISAPRE 2 --------------------------------------------------------

info_fin_isapres_1990_2011 <- read_xls(drive_download("1. Información Financiera PCGA años 1990-2011.xls", type = "xls", overwrite = TRUE)$local_path, sheet = "Estado de Resultados")
file.remove("1. Información Financiera PCGA años 1990-2011.xls")

info_fin_isapres_1990_2011 <- info_fin_isapres_1990_2011 %>% tail(13) %>% head(-3) %>% select(-c(24, 25)) %>% tail(-1) %>% setNames(.[1, ]) %>% tail(-1) %>% 
  mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", names_transform = list(AÑO = as.integer), values_to = "MILLONES DE PESOS", 
  values_transform = list("MILLONES DE PESOS" = as.integer)) %>% mutate(Rubros = case_when(Rubros =="Ingreso operacional" ~ "Ingresos de actividades ordinarias",
  Rubros =="Costo de operación (-)" ~ "Costo de ventas (menos)", Rubros =="Margen de explotación" ~ "Ganancia bruta", Rubros =="Gastos de administración y ventas (-)" ~ "Gastos de administración y otros gastos por función (menos)",
  Rubros =="Resultado operacional" ~ "Otros items de ingresos y egresos (1)", Rubros =="Resultado no operacional" ~ "Ganancia (pérdida) antes de impuestos", Rubros =="Impuesto a la renta (-)" ~ "Gasto por impuestos a las ganancias (menos)",
  Rubros =="Resultado del ejercicio" ~ "Ganancia (pérdida)", TRUE ~ NA))

info_fin_isapres_2012_2019 <- read_xls(drive_download("1. Información Financiera IFRS años 2012-2019 (no actualizado).xls", type = "xls", overwrite = TRUE)$local_path, sheet = "Estado de Resultados")
file.remove("1. Información Financiera IFRS años 2012-2019 (no actualizado).xls")

info_fin_isapres_2012_2019 <- info_fin_isapres_2012_2019 %>% tail(14) %>% head(-5) %>% select(-c(10, 11)) %>% setNames(.[1, ]) %>% tail(-1) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
  pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "MILLONES DE PESOS", values_transform = list("MILLONES DE PESOS" = as.integer)) %>% 
  mutate(AÑO = str_trim(str_remove_all(AÑO, "\\(\\*+\\)")), AÑO = as.integer(AÑO))


info_fin_isapres_2020_2023 <- c(map(drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xls")$name, 
                                     ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "Result. Financieros comparados")) %>% 
                                   set_names(drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xls")$name), 
                                 map(drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xlsx")$name, 
                                     ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "Result. Financieros comparados")) %>% 
                                   set_names(drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xlsx")$name))

file.remove(drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xlsx")$name, drive_find(pattern = "Estadísticas Financieras de Isapre a Diciembre de", type = "xls")$name)

info_fin_isapres_2020_2023 <- info_fin_isapres_2020_2023 %>% map(~.x %>% head(15) %>% select(1, 2, 4) %>% setNames(replace(unlist(.[3, ]), 1, "Rubros")) %>% tail(-7) %>% pivot_longer(cols = c(2,3), names_to = "AÑO",
  names_transform = list(AÑO = as.integer), values_to = "MILLONES DE PESOS", values_transform = list("MILLONES DE PESOS" = as.integer))) %>% bind_rows() %>% arrange(AÑO) %>% distinct(Rubros, AÑO, `MILLONES DE PESOS`)

informacion_financiera_isapres <- bind_rows(info_fin_isapres_1990_2011, info_fin_isapres_2012_2019, info_fin_isapres_2020_2023) %>% arrange(AÑO)
saveRDS(informacion_financiera_isapres, "data/salud/grafico_isapre/informacion_financiera_isapres.rds")
rm(info_fin_isapres_1990_2011, info_fin_isapres_2012_2019, info_fin_isapres_2020_2023, informacion_financiera_isapres)

estadisticas_movilidad_2021_2023 <- map(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes", type = "xlsx")$name, 
                                      ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "Nacional")) %>% 
                                         set_names(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes", type = "xlsx")$name)

anios <- c(2023, 2021, 2022)

cotizantes_abandonan_isapre <- estadisticas_movilidad_2021_2023 %>% map(~.x %>% head(17) %>% select(-c(1,2)) %>% select(1:5) %>% setNames(replace(unlist(.[5, ]), 1, "Tramo de Edad")) %>% tail(-5)) %>% 
  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% bind_rows() %>% select(1:3) %>% rename("COTIZANTES QUE ABANDONAN ISAPRES" = `N° Cotizantes`, "TRAMOS DE EDAD" = `Tramo de Edad`) %>%
  arrange(AÑO)

cotizantes_ingresan_isapre <- estadisticas_movilidad_2021_2023 %>% map(~.x %>% filter(row_number() >= 18 & row_number() <= 29) %>% select(-c(1,2)) %>% select(1:5) %>%  setNames(c("Tramo de Edad", 
  "N° Cotizantes", "% de Cotizantes Vigentes", "Cotización Pactada Promedio por Cotizante ($)", "N° Cargas Promedio por Cotizante"))) %>% map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% 
  bind_rows() %>% select(1:3) %>% rename("COTIZANTES QUE INGRESAN A ISAPRES" = `N° Cotizantes`, "TRAMOS DE EDAD" = `Tramo de Edad`) %>% arrange(AÑO)

estadisticas_movilidad_2021_2023 <- left_join(cotizantes_abandonan_isapre, cotizantes_ingresan_isapre) %>% pivot_longer(cols = c(3,4), names_to = "CONDICION COTIZANTES", values_to = "NUMERO DE COTIZANTES", values_transform = list("NUMERO DE COTIZANTES" = as.integer))

estadisticas_movilidad_2015_2020 <- map(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes – 201[5-9]|2020", type = "xls")$name, 
                                              ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "Mov Cotizantes Sistema Isapre")) %>% 
                                                  set_names(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes – 201[5-9]|2020", type = "xls")$name)

anios <- c(2017, 2018, 2019, 2020, 2015, 2016)

cotizantes_abandonan_isapre <- estadisticas_movilidad_2015_2020 %>% map(~.x %>% head(17) %>% tail(-5) %>% select(1:5) %>% setNames(c("Tramo de Edad", "N° Cotizantes", 
  "% de Cotizantes Vigentes", "Cotización Pactada Promedio por Cotizante ($)", "N° Cargas Promedio por Cotizante"))) %>% map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% 
  bind_rows() %>% select(1:3) %>% rename("COTIZANTES QUE ABANDONAN A ISAPRES" = `N° Cotizantes`, "TRAMOS DE EDAD" = `Tramo de Edad`) %>% arrange(AÑO)

cotizantes_ingresan_isapre <- estadisticas_movilidad_2015_2020 %>% map(~.x %>% filter(row_number() >= 18 & row_number() <= 30) %>% select(1:5) %>% setNames(c("Tramo de Edad", 
  "N° Cotizantes", "% de Cotizantes Vigentes", "Cotización Pactada Promedio por Cotizante ($)", "N° Cargas Promedio por Cotizante"))) %>% 
  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% bind_rows() %>% select(1:3) %>%  rename("COTIZANTES QUE INGRESAN A ISAPRES" = `N° Cotizantes`, 
  "TRAMOS DE EDAD" = `Tramo de Edad`) %>% filter(!is.na(`COTIZANTES QUE INGRESAN A ISAPRES`)) %>% arrange(AÑO)

estadisticas_movilidad_2015_2020 <- left_join(cotizantes_abandonan_isapre, cotizantes_ingresan_isapre) %>% pivot_longer(cols = c(3,4), names_to = "CONDICION COTIZANTES", values_to = "NUMERO DE COTIZANTES", values_transform = list("NUMERO DE COTIZANTES" = as.integer))

estadisticas_movilidad_2010_2014 <- map(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes – 201[0-4]", type = "xls")$name, 
                                        ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "Movilidad Cotizantes Diciembre")) %>% 
                                            set_names(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes – 201[0-4]", type = "xls")$name)

file.remove(drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes", type = "xls")$name, drive_find(pattern = "Estadística Mensual de Movilidad de Cotizantes", type = "xlsx")$name)
                                  
anios <- c(2013, 2012, 2014, 2011, 2010)

cotizantes_abandonan_isapre <- estadisticas_movilidad_2010_2014 %>% map(~.x %>% head(17) %>% select(1:5) %>% setNames(replace(unlist(.[4, ]), 1, "Tramo de Edad")) %>% tail(-5)) %>% 
  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% bind_rows() %>% select(1:3) %>% rename("COTIZANTES QUE ABANDONAN ISAPRES" = `N° de cotizantes`, "TRAMOS DE EDAD" = `Tramo de Edad`) %>%
  arrange(AÑO)

cotizantes_ingresan_isapre <- estadisticas_movilidad_2010_2014 %>% map(~.x %>% filter(row_number() >= 18 & row_number() <= 30) %>% select(1:5) %>% setNames(c("Tramo de Edad", 
  "N° Cotizantes", "% de Cotizantes Vigentes", "Cotización Pactada Promedio por Cotizante ($)", "N° Cargas Promedio por Cotizante"))) %>% 
  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = `Tramo de Edad`)) %>% bind_rows() %>% select(1:3) %>%  rename("COTIZANTES QUE INGRESAN A ISAPRES" = `N° Cotizantes`, 
  "TRAMOS DE EDAD" = `Tramo de Edad`) %>% filter(!is.na(`COTIZANTES QUE INGRESAN A ISAPRES`)) %>% arrange(AÑO)

estadisticas_movilidad_2010_2014 <- left_join(cotizantes_abandonan_isapre, cotizantes_ingresan_isapre) %>% pivot_longer(cols = c(3,4), names_to = "CONDICION COTIZANTES", values_to = "NUMERO DE COTIZANTES", values_transform = list("NUMERO DE COTIZANTES" = as.integer))

estadisticas_movilidad_2021_2023 <- estadisticas_movilidad_2021_2023 %>% mutate(`TRAMOS DE EDAD` = case_when(`TRAMOS DE EDAD` == "0 a 19 años"  ~  "de 00 a 19 años", 
  `TRAMOS DE EDAD` == "20 a 24 años"  ~  "de 20 a 24 años", `TRAMOS DE EDAD` == "25 a 29 años"  ~  "de 25 a 29 años", `TRAMOS DE EDAD` == "30 a 34 años"  ~  "de 30 a 34 años",
  `TRAMOS DE EDAD` == "35 a 39 años"  ~  "de 35 a 39 años", `TRAMOS DE EDAD` == "40 a 44 años"  ~  "de 40 a 44 años", `TRAMOS DE EDAD` == "45 a 49 años"  ~  "de 45 a 49 años",
  `TRAMOS DE EDAD` == "50 a 54 años"  ~  "de 50 a 54 años", `TRAMOS DE EDAD` == "55 a 59 años"  ~  "de 55 a 59 años", `TRAMOS DE EDAD` == "60 a 64 años"  ~  "de 60 a 64 años",
  `TRAMOS DE EDAD` == "65 y más años" ~  "de 65 y más años", TRUE ~ `TRAMOS DE EDAD`))

estadisticas_movilidad_2015_2020 <- estadisticas_movilidad_2015_2020 %>% mutate(`CONDICION COTIZANTES` = ifelse(`CONDICION COTIZANTES` == "COTIZANTES QUE ABANDONAN A ISAPRES", 
  "COTIZANTES QUE ABANDONAN ISAPRES", `CONDICION COTIZANTES`))

estadisticas_movilidad <- bind_rows(estadisticas_movilidad_2010_2014, estadisticas_movilidad_2015_2020, estadisticas_movilidad_2021_2023)
saveRDS(estadisticas_movilidad, "data/salud/grafico_isapre/estadisticas_movilidad.rds")
rm(estadisticas_movilidad, estadisticas_movilidad_2010_2014, estadisticas_movilidad_2015_2020, estadisticas_movilidad_2021_2023, cotizantes_abandonan_isapre, cotizantes_ingresan_isapre,
   anios)


estadisticas_planes <- map(drive_find(pattern = "Estadística Anual Planes de Salud año 201[6-8]", type = "xls")$name, 
                        ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "Tipo Plan")) %>% 
                           set_names(drive_find(pattern = "Estadística Anual Planes de Salud año 201[6-8]", type = "xls")$name)

anios <- c(2018, 2017, 2016) 

beneficiarios_tipo_plan_2016_2018 <- estadisticas_planes %>% map(~.x %>% filter(row_number() >= 49 & row_number() <= 69) %>% select(2, 3, 5, 7) %>% 
                  setNames(c("ISAPRE", "INDIVIDUALES", "GRUPALES", "TOTALES")) %>% filter(row_number() >= which(str_detect(ISAPRE, "Colmena"))[1]) %>% pivot_longer(cols = c(2:4), 
                  names_to = "TIPO DE PLAN", values_to = "NUMERO DE BENEFICIARIOS", values_transform = list("NUMERO DE BENEFICIARIOS" = as.integer))) %>% 
                  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = "ISAPRE"))  %>% bind_rows() %>% mutate(`NUMERO DE BENEFICIARIOS` = ifelse(`NUMERO DE BENEFICIARIOS` == 0, NA, 
                  `NUMERO DE BENEFICIARIOS`)) %>% filter(!is.na(`NUMERO DE BENEFICIARIOS`))

cotizantes_tipo_plan_2016_2018 <- estadisticas_planes %>% map(~.x %>% filter(row_number() >= 27 & row_number() <= 45) %>% select(2, 3, 5, 7) %>% setNames(c("ISAPRE", 
                  "INDIVIDUALES", "GRUPALES", "TOTALES")) %>% filter(row_number() >= which(str_detect(ISAPRE, "Colmena"))[1]) %>% pivot_longer(cols = c(2:4), 
                  names_to = "TIPO DE PLAN", values_to = "NUMERO DE COTIZANTES", values_transform = list("NUMERO DE COTIZANTES" = as.integer))) %>% 
                  map2(., anios, ~.x %>% mutate(AÑO = .y, .before = "ISAPRE")) %>% bind_rows() %>% mutate(`NUMERO DE COTIZANTES` = ifelse(`NUMERO DE COTIZANTES` == 0, NA, 
                  `NUMERO DE COTIZANTES`)) %>% filter(!is.na(`NUMERO DE COTIZANTES`))

anios <- c(2020, 2021, 2019, 2022, 2023, 2024)

estadisticas_planes <- map(drive_find(pattern = "Estadística Anual Planes de Salud año (2019|202[0-4])", type = "xlsx")$name, 
                           ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "Tipo Plan")) %>% 
                              set_names(drive_find(pattern = "Estadística Anual Planes de Salud año (2019|202[0-4])", type = "xlsx")$name)

beneficiarios_tipo_plan_2019_2024 <- estadisticas_planes %>% map(~.x %>% filter(row_number() >= 45 & row_number() <= 69) %>% select(2, 3, 5, 7) %>% 
                  setNames(c("ISAPRE", "INDIVIDUALES", "GRUPALES", "TOTALES")) %>% filter(row_number() >= which(str_detect(ISAPRE, "Colmena"))[1] &
                  row_number() <= which(str_detect(ISAPRE, "Sistema"))[1]) %>% pivot_longer(cols = c(2:4), names_to = "TIPO DE PLAN", values_to = "NUMERO DE BENEFICIARIOS", 
                  values_transform = list("NUMERO DE BENEFICIARIOS" = as.integer))) %>% map2(., anios, ~.x %>% mutate(AÑO = .y, .before = "ISAPRE")) %>% bind_rows() %>% 
                  mutate(`NUMERO DE BENEFICIARIOS` = ifelse(`NUMERO DE BENEFICIARIOS` == 0, NA, `NUMERO DE BENEFICIARIOS`)) %>% filter(!is.na(`NUMERO DE BENEFICIARIOS`))

cotizantes_tipo_plan_2019_2024 <- estadisticas_planes %>% map(~.x %>% filter(row_number() >= 23 & row_number() <= 45) %>% select(2, 3, 5, 7) %>% setNames(c("ISAPRE", 
                  "INDIVIDUALES", "GRUPALES", "TOTALES")) %>% filter(row_number() >= which(str_detect(ISAPRE, "Colmena"))[1] &
                  row_number() <= which(str_detect(ISAPRE, "Sistema"))[1]) %>% pivot_longer(cols = c(2:4), names_to = "TIPO DE PLAN", values_to = "NUMERO DE COTIZANTES", 
                  values_transform = list("NUMERO DE COTIZANTES" = as.integer))) %>% map2(., anios, ~.x %>% mutate(AÑO = .y, .before = "ISAPRE")) %>% bind_rows() %>% 
                  mutate(`NUMERO DE COTIZANTES` = ifelse(`NUMERO DE COTIZANTES` == 0, NA, `NUMERO DE COTIZANTES`)) %>% filter(!is.na(`NUMERO DE COTIZANTES`))

beneficiarios_tipo_plan <- bind_rows(beneficiarios_tipo_plan_2016_2018, beneficiarios_tipo_plan_2019_2024) %>% mutate(ISAPRE = case_when(ISAPRE == "I. Abiertas" ~ "Isapres Abiertas",
                  ISAPRE == "I. Cerradas" ~ "Isapres Cerradas", TRUE ~ ISAPRE)) %>% arrange(AÑO)

cotizantes_tipo_plan <- bind_rows(cotizantes_tipo_plan_2016_2018, cotizantes_tipo_plan_2019_2024) %>% mutate(ISAPRE = case_when(ISAPRE == "I. Abiertas" ~ "Isapres Abiertas",
                  ISAPRE == "I. Cerradas" ~ "Isapres Cerradas", TRUE ~ ISAPRE)) %>% arrange(AÑO)

saveRDS(beneficiarios_tipo_plan, "data/salud/grafico_isapre/beneficiarios_tipo_plan.rds")
saveRDS(cotizantes_tipo_plan, "data/salud/grafico_isapre/cotizantes_tipo_plan.rds")

rm(beneficiarios_tipo_plan, beneficiarios_tipo_plan_2016_2018, beneficiarios_tipo_plan_2019_2024, cotizantes_tipo_plan, cotizantes_tipo_plan_2016_2018, cotizantes_tipo_plan_2019_2024,
   estadisticas_planes, anios)

file.remove(drive_find(pattern = "Estadística Anual Planes de Salud año 201[6-8]", type = "xls")$name, drive_find(pattern = "Estadística Anual Planes de Salud año (2019|202[0-4])", type = "xlsx")$name)

# Base Gráfico Renta Beneficiarios-Cotizantes -----------------------------

renta_fonasa <- c(map(drive_find(pattern = "Boletin Estadistico (201[0-1]|2017)-(201[0-2])|2018", type = "xlsx")$name, 
                           ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C07")) %>% 
                              set_names(drive_find(pattern = "Boletin Estadistico (201[0-1]|2017)-(201[0-2])|2018", type = "xlsx")$name),
                  map(drive_find(pattern = "Boletin Estadistico 201(1|4)-201(2|5)", type = "xls")$name, 
                      ~read_xls(drive_download(.x, type = "xls", overwrite = TRUE)$local_path, sheet = "C07")) %>% 
                    set_names(drive_find(pattern = "Boletin Estadistico 201(1|4)-201(2|5)", type = "xls")$name))         

file.remove(drive_find(pattern = "Boletin Estadistico (201[0-1]|2017)-(201[0-2])|2018", type = "xlsx")$name, drive_find(pattern = "Boletin Estadistico 201(1|4)-201(2|5)", type = "xls")$name)

renta_fonasa_1 <- renta_fonasa %>% keep_at(c("Boletin Estadistico 2010-2011.xlsx", "Boletin Estadistico 2011-2012.xls"))

renta_fonasa_1 <- renta_fonasa_1 %>% map(~.x %>% rellenar_variables() %>% select(1, 3, 5) %>% tail(-4) %>% 
                            setNames(replace(unlist(.[1,]), 1, "TRAMOS DE RENTA IMPONIBLE MENSUAL")) %>% tail(-3) %>% 
                            filter(row_number() <= which(str_detect(`TRAMOS DE RENTA IMPONIBLE MENSUAL`, "Total general"))[1]) %>% 
                            mutate(`TRAMOS DE RENTA IMPONIBLE MENSUAL` = str_trim(str_remove_all(`TRAMOS DE RENTA IMPONIBLE MENSUAL`, ">|<"))) %>% select(1,2) %>% 
                            pivot_longer(cols = c(2), names_to = "AÑO", names_transform = list("AÑO" = as.integer), values_to = "NUMERO DE COTIZANTES", 
                                         values_transform = list("NUMERO DE COTIZANTES" = as.integer))) %>% bind_rows()
                 
renta_fonasa_2 <- renta_fonasa %>% discard_at(c("Boletin Estadistico 2010-2011.xlsx", "Boletin Estadistico 2011-2012.xls"))

renta_fonasa_2 <- renta_fonasa_2 %>% map(~.x %>% rellenar_variables() %>% select(1, 3, 5, 7, 9) %>% tail(-4) %>% setNames(replace(unlist(.[1,]), 1, "TRAMOS DE RENTA IMPONIBLE MENSUAL")) %>% tail(-4) %>% 
                                        head(-7) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", values_to = "NUMERO DE COTIZANTES", values_transform = list("NUMERO DE COTIZANTES" = as.integer)) %>% 
                                          mutate(AÑO = str_trim(str_remove_all(AÑO, "\\(\\d+\\)")), AÑO = as.integer(AÑO))) %>% bind_rows()


renta_fonasa <- bind_rows(renta_fonasa_1, renta_fonasa_2) %>% distinct(`TRAMOS DE RENTA IMPONIBLE MENSUAL`, AÑO, `NUMERO DE COTIZANTES`) %>% arrange(AÑO)
saveRDS(renta_fonasa, "data/salud/grafico_general_salud/tramos_renta_fonasa.rds")
rm(renta_fonasa, renta_fonasa_1, renta_fonasa_2)

renta_isapre <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cotiz. x Rta. Imponible 98 y +")

renta_isapre <- renta_isapre %>% tail(36) %>% head(-2) %>% setNames(replace(unlist(.[1, ]), 1, "TRAMOS DE RENTA IMPONIBLE MENSUAL")) %>% tail(-1) %>% 
                    pivot_longer(cols = c(2:last_col()), names_to = "AÑO", names_transform = list(AÑO = as.integer), values_to = "NUMERO DE COTIZANTES") %>% filter(!is.na(`NUMERO DE COTIZANTES`)) %>% 
                      mutate(`TRAMOS DE RENTA IMPONIBLE MENSUAL` = str_trim(str_remove_all(`TRAMOS DE RENTA IMPONIBLE MENSUAL`, "\\(\\*+\\)"))) %>% arrange(AÑO)

saveRDS(renta_isapre, "data/salud/grafico_general_salud/tramos_renta_isapre.rds")
         
cotizantes_carga_isapre <- read_xlsx(drive_download("Cartera Beneficiarios Isapre 1990-2023.xlsx", type = "xlsx", overwrite = TRUE)$local_path, sheet = "Cartera Anual de Isapre")

cotizantes_carga_isapre <- left_join(cotizantes_carga_isapre %>% head(6) %>% setNames(.[3, ]) %>% tail(-5) %>% mutate(across(c(2:last_col()), as.numeric)) %>% pivot_longer(cols = c(2:last_col()), names_to = "AÑO", names_transform = list(AÑO = as.integer),
                           values_to = "COTIZANTES"),cotizantes_carga_isapre %>% filter(row_number() >= 10 & row_number() <= 16) %>% setNames(.[4, ]) %>% tail(-6) %>% mutate(across(c(2:last_col()), as.numeric)) %>% 
                           pivot_longer(cols = c(2:last_col()), names_to = "AÑO", names_transform = list(AÑO = as.integer), values_to = "CARGAS"))

cotizantes_carga_isapre <- cotizantes_carga_isapre %>% select(-c(Isapres)) %>% pivot_longer(cols = c(2:3), names_to = "TIPO DE BENEFICIARIO", values_to = "CANTIDAD")

saveRDS(cotizantes_carga_isapre, "data/salud/grafico_general_salud/cotizantes_carga_isapre.rds")

cotizantes_fonasa_totales <- read_rds("data/salud/grafico_genero_salud/cotizantes_fonasa_genero.rds") %>% filter(GENERO == "TOTAL")  %>% rename(COBERTURA = GENERO) %>% 
  mutate(COBERTURA = "FONASA") %>% mutate(COBERTURA = "COTIZANTES")

carga_fonasa_totales <- read_xlsx(drive_download(drive_find(pattern = "Boletin Estadistico 2017-2018.xlsx", type = "xlsx")$name, type = "xlsx", overwrite = TRUE)$local_path, sheet = "C04")

carga_fonasa_totales <- carga_fonasa_totales %>% rellenar_variables2() %>% select(1, 2, 7) %>% setNames(replace(unlist(.[6, ]), 2, "INDICADOR")) %>% tail(-6) %>% 
  mutate(Año = as.integer(Año), `Cargas Familiares` = as.integer(`Cargas Familiares`)) %>% filter(INDICADOR == "N° Personas" & between(Año, 2009, 2018)) %>% select(-c(2)) %>% 
  pivot_longer(cols = c(2), names_to = "COBERTURA", values_to = "CANTIDAD") %>% mutate(COBERTURA = "CARGAS") %>% rename(AÑO = Año)

cotizantes_carga_fonasa <- bind_rows(cotizantes_fonasa_totales, carga_fonasa_totales) %>% arrange(AÑO) %>% rename(`TIPO DE BENEFICIARIO` = COBERTURA)
saveRDS(cotizantes_carga_fonasa, "data/salud/grafico_general_salud/cotizantes_carga_fonasa.rds")
file.remove("Boletin Estadistico 2017-2018.xlsx", "Cartera Beneficiarios Isapre 1990-2023.xlsx")

tiempo_fin <- Sys.time()
print(tiempo_fin - tiempo_inicio); rm(tiempo_inicio, tiempo_fin)
rm(list = ls())
