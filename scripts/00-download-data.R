# Cargar Funciones y Vectores Personalizados ------------------------------
tiempo_inicio <- Sys.time()
source("scripts/99-utilidades.R")

# Descargar archivos para procesamiento -----------------------------------

# Bases de Datos Banco Central --------------------------------------------

api_pib <- paste0("https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx?user=correo_de_usuario@ejemplo.cl&pass=password&timeseries=", 
                  c("F032.PIB.FLU.N.CLP.EP18.Z.Z.0.T", "F032.PIB.FLU.R.CLP.EP18.Z.Z.1.T", "F032.PIB.FLU.R.CLP.EP18.Z.Z.0.T", 
                  "F012.PPCP.FLU.N.7.AME.CL.USD.FMI.Z.0.A", "F032.PIB.PP.Z.USD.2018.Z.Z.0.A"), "&function=GetSeries")

lista_pib <- obtener_datos_PIB(api_pib)

# Bases de Datos del INE --------------------------------------------------

df_ine <- "https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-de-poblaci%C3%B3n-1992-2050_base-2017_tabulados.xlsx?sfvrsn=68eefb1_9"

esperanza_de_vida <- descargar_archivo_ind2(df_ine)
proyeccion_poblacion <- descargar_archivo_ind2(df_ine) %>% filter(between(row_number(), 3, 7)) 

ocup_inf <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_OCUINF_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")
ocup <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_OCU_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")
tasa_desocup <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_TDES_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")
tasa_ocup <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_TOCU_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")

ingreso_medio <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_YRMEDIOOCU_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")
ingreso_mediano <- read_csv("https://sdmx.ine.gob.cl/rest/data/CL01,DF_YRMEDIANOOCU_SEXO,1.0/all?dimensionAtObservation=AllDimensions&format=csvfile") %>% select("AREA_REF":"OBS_VALUE")

# Bases de Datos Gasto Salud ----------------------------------------------

gasto_salud <- map(drive_ls(drive_find(pattern = "gasto", type = "folder"), pattern = "Gasto")$name, 
                   ~read_xlsx(drive_download(.x, type = "xlsx", overwrite = TRUE)$local_path)) %>% 
  set_names(drive_ls(drive_find(pattern = "gasto", type = "folder"), pattern = "Gasto")$name)

file.remove(drive_ls(drive_find(pattern = "gasto", type = "folder"), pattern = "Gasto")$name)

# Bases de Datos Pensiones ------------------------------------------------

paginas <- map(paste0("https://www.spensiones.cl/apps/centroEstadisticas/paginaCuadrosCCEE.php?menu=sest&menuN1=sistpens&menuN2=", 
                      c("afil", "cotiz", "pens", "fondospen", "pilar")), read_html) %>% set_names("afiliados", "cotizantes", "pensionados", "fondos_pensiones", "pilar_pgu")

# Extracción de elementos necesarios --------------------------------------

href <- paginas %>% map(~.x %>% 
                          html_nodes("a[href^='javascript:document.location.href']") %>%
                          html_attr("href")) %>% flatten()

# Generación de URL's de descarga -----------------------------------------

url_archivos_general <- paste0("https://www.spensiones.cl", sub("javascript:document.location.href='(.*?)';", "\\1", href))
url_archivos_POSIXct <- url_archivos_general[str_detect(url_archivos_general, archivos_POSIXct)]
url_archivos_general <- setdiff(url_archivos_general, url_archivos_POSIXct)

lista_archivos_general <- map(url_archivos_general, descargar_archivo) %>% set_names(basename(url_archivos_general))

lista_archivos_POSIXct_afil_cot <- compact(map(url_archivos_POSIXct, descargar_archivo_fecha) %>% set_names(basename(url_archivos_POSIXct)) %>%
                                             imap(~(if(str_detect(.y, "afiliados|cotizantes|cotizaciones|activos")) .x else NULL))) %>% map(~.x %>% filter(
                                               between(.x[[1]], as.Date("1970-01-01"), as.Date("2040-12-31"))))

lista_archivos_POSIXct_general <- compact(map(url_archivos_POSIXct, descargar_archivo_fecha) %>% set_names(basename(url_archivos_POSIXct)) %>% 
                                            imap(~(if(!str_detect(.y, "afiliados|cotizantes|cotizaciones|activos")) .x else NULL))) %>% map(~.x %>% filter(
                                              if_any(1:last_col(), ~ !is.na(.))))

colnames(lista_archivos_POSIXct_afil_cot$cotizantes_region_tramos_ingreso_imponible.xls) <- 
  replace(unlist(filter(descargar_archivo_fecha(url_archivos_POSIXct[[9]]), str_detect(REGIÓN, "REGIÓN"))[1,]), 1, "FECHA")

rm(href, paginas, archivos_POSIXct, url_archivos_general, url_archivos_POSIXct, descargar_archivo, descargar_archivo_fecha)

# Imagenes ----------------------------------------------------------------

drive_download(drive_ls(drive_find(pattern = "imagenes", type = "folder"), pattern = "imagen_piloto")$name, type = "jpg", overwrite = TRUE, path = "www/imagen_piloto.jpg")
drive_download(drive_ls(drive_find(pattern = "imagenes", type = "folder"), pattern = "imagen_inicio")$name, type = "jpg", overwrite = TRUE, path = "www/imagen_inicio.jpg")