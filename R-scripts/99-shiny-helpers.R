# Cargar Librerías --------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(highcharter)
library(scales)
library(viridisLite)

# Configuración 1 ---------------------------------------------------------

hc_theme_sparkline2 <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(
        overflow = "visible"
      ),
      skipClone = TRUE
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = TRUE,
      headerFormat = "",
      pointFormat = "{point.x}: <b>{point.y}</b>"
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 1,
        shadow = FALSE,
        fillOpacity = 0.25
      )
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

# Configuración 2 ---------------------------------------------------------

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "100px",minititle = NULL) {
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if(!is.null(minititle)) tags$small(minititle),
      h3(value),
      # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
      tags$span(hc_size(spark, height = height_spark)),
      if (!is.null(subtitle)) p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

# Configuración 3 ---------------------------------------------------------

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#e95420",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

# Configuración 4 ---------------------------------------------------------

dropdownButtonp <- purrr::partial(
  dropdownButton,
  status = "customstatus",
  size = "xs",
  right = TRUE,
  status = "info",
  width = "2px",
  inline = TRUE,
)

# Sección Macroeconomía ---------------------------------------------------

# Gráfico Macroeconómico --------------------------------------------------

grafico_config_macroeconomica <- list(
  pib_precios_corrientes = list(
    datos = read_rds("data/macroeconomia/grafico_pib/PIB_precios_corrientes.rds"),
    titulo = "PIB a precios corrientes",
    columna_x = "AÑO",
    columna_y = "PIB",
    y_axis_title = "PIB a precios corrientes",
    group = "TIPO",
    prefix = "$MM "
  ),
  pib_volumen_desestacionalizado = list(
    datos = read_rds("data/macroeconomia/grafico_pib/PIB_volumen_desestacionalizado.rds"),
    titulo = "PIB volumen a precios del año anterior encadenado (desestacionalizado)",
    columna_x = "AÑO",
    columna_y = "PIB",
    y_axis_title = "PIB volumen a precios del año anterior encadenado (desestacionalizado)",
    group = "TIPO",
    prefix = "$MM "
  ),
  pib_volumen_encadenado = list(
    datos = read_rds("data/macroeconomia/grafico_pib/PIB_volumen_encadenado.rds"),
    titulo = "PIB volumen a precios del año anterior encadenado",
    columna_x = "AÑO",
    columna_y = "PIB",
    y_axis_title = "PIB volumen a precios del año anterior encadenado",
    group = "TIPO",
    prefix = "$MM "
  ),
  pib_percapita = list(
    datos = read_rds("data/macroeconomia/grafico_pib/PIB_per_capita.rds"),
    titulo = "PIB per cápita en USD (referencia 2018)",
    columna_x = "AÑO",
    columna_y = "PIB",
    y_axis_title = "PIB per cápita en USD (referencia 2018)",
    group = NULL,
    prefix = "$USD "
  ),
  pib_percapita_ppp = list(
    datos = read_rds("data/macroeconomia/grafico_pib/PIB_percapita_ppp.rds"),
    titulo = "PIB per cápita PPP",
    columna_x = "AÑO",
    columna_y = "PIB",
    y_axis_title = "PIB per cápita PPP",
    group = NULL,
    prefix = "$USD "
  ))

grafico_config_gasto <- list(
  gasto_salud_pib = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_pib.rds"),
    titulo = "Gasto en Salud como % del PIB",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Gasto en Salud como % del PIB",
    group = "SECTOR",
    suffix = "%",
    prefix = ""
  ),
  gasto_salud_pesos = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_pesos.rds"),
    titulo = "Gasto en Salud",
    subtitulo = "Millones de pesos corrientes",
    columna_x = "AÑO",
    columna_y = "PESOS",
    y_axis_title = "Gasto en Salud en Millones de pesos corrientes",
    group = "SECTOR",
    suffix = "",
    prefix = "$M "
  ),
  gasto_salud_pesos_constantes = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_pesos_constante.rds"),
    titulo = "Gasto en Salud",
    subtitulo = "Millones de pesos constantes del año 2022",
    columna_x = "AÑO",
    columna_y = "PESOS",
    y_axis_title = "Gasto en Salud en Millones de pesos constante",
    group = "SECTOR",
    suffix = "",
    prefix = "$M "
  ),
  gasto_salud_percapita = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_percapita.rds"),
    titulo = "Gasto en Salud Per Cápita",
    subtitulo = "Pesos Corrientes",
    columna_x = "AÑO",
    columna_y = "PESOS CORRIENTES",
    y_axis_title = "Gasto en Salud Per Cápita en Pesos Corrientes",
    group = "",
    suffix = "",
    prefix = ""
  ),
  gasto_salud_percapita_ppa = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_percapita_ppa.rds"),
    titulo = "Gasto en Salud Per Cápita",
    subtitulo = "Dólares PPA",
    columna_x = "AÑO",
    columna_y = "PPA",
    y_axis_title = "Gasto en Salud Per Cápita en Dólares PPA",
    group = "",
    suffix = "",
    prefix = ""
  ),
  gasto_salud_instituciones_t = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_instituciones_t.rds"),
    titulo = "Gasto en Salud por Instituciones",
    subtitulo = "Millones de pesos corrientes",
    columna_x = "AÑO",
    columna_y = "PESOS CORRIENTES",
    y_axis_title = "Gasto en Salud en Millones de pesos corrientes",
    group = "INSTITUCIONES",
    suffix = "",
    prefix = "$M "),
  gasto_salud_instituciones = list(
    datos = read_rds("data/salud/grafico_gasto/gasto_salud_instituciones.rds"),
    titulo = "Gasto en Salud por Sectores Institucionales",
    subtitulo = "Millones de pesos corrientes",
    columna_x = "AÑO",
    columna_y = "PESOS CORRIENTES",
    y_axis_title = "Gasto en Salud en Millones de pesos corrientes",
    group = "INSTITUCIONES",
    suffix = "",
    prefix = "$M ",
    instituciones = "SECCIONES"))

grafico_config_poblacion <- list(
  esperanza_de_vida_totales = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/esperanza_de_vida.rds"),
    titulo = "Esperanza de Vida",
    columna_x = "AÑO",
    columna_y = "EDAD",
    y_axis_title = "Esperanza de Vida",
    group = "GENERO",
    suffix = ""),
  proyeccion_totales = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/proyeccion_poblacion.rds"),
    titulo = "Proyección Población",
    columna_x = "AÑO",
    columna_y = "POBLACIÓN",
    y_axis_title = "Proyección Población",
    group = "GENERO",
    suffix = ""),
  estimacion_ocupados = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Ocupados",
    columna_x = "AÑO",
    columna_y = "OCUPADOS",
    y_axis_title = "Estimación Ocupados",
    group = "GENERO",
    suffix = ""),
  estimacion_ocupados_form = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Ocupados Informales",
    columna_x = "AÑO",
    columna_y = "OCUPADOS_FORMALES",
    y_axis_title = "Estimación Ocupados Informales",
    group = "GENERO",
    suffix = ""),
  estimacion_ocupados_inf = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Ocupados Informales",
    columna_x = "AÑO",
    columna_y = "OCUPADOS_INFORMALES",
    y_axis_title = "Estimación Ocupados Informales",
    group = "GENERO",
    suffix = ""),
  tasa_ocupados = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Tasa Ocupados",
    columna_x = "AÑO",
    columna_y = "TASA_OCUPADOS",
    y_axis_title = "Estimación Tasa Ocupados",
    group = "GENERO",
    suffix = "%"),
  tasa_ocupados_form = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Tasa Ocupados Formales",
    columna_x = "AÑO",
    columna_y = "TASA_OCUPADOS_FORMALES",
    y_axis_title = "Estimación Tasa Ocupados Formales",
    group = "GENERO",
    suffix = "%"),
  tasa_ocupados_inf = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Tasa Ocupados Informales",
    columna_x = "AÑO",
    columna_y = "TASA_OCUPADOS_INFORMALES",
    y_axis_title = "Estimación Tasa Ocupados Informales",
    group = "GENERO",
    suffix = "%"),
  tasa_desocupados = list(
    datos = read_rds("data/macroeconomia/grafico_poblacion/estimacion_ocupados.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Estimación Tasa de Desocupación",
    columna_x = "AÑO",
    columna_y = "TASA_DESOCUPADOS",
    y_axis_title = "Estimación Tasa Ocupados Informales",
    group = "GENERO",
    suffix = "%"))

grafico_config_ingresos <- list(
  ingreso_medio = list(
    datos = read_rds("data/macroeconomia/grafico_ingreso/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Ingreso Medio",
    columna_x = "AÑO",
    columna_y = "INGRESO_MEDIO",
    y_axis_title = "Ingreso Medio",
    group = "GENERO",
    prefix = "$ "),
  ingreso_mediano = list(
    datos = read_rds("data/macroeconomia/grafico_ingreso/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total"),
    titulo = "Ingreso Mediano",
    columna_x = "AÑO",
    columna_y = "INGRESO_MEDIANO",
    y_axis_title = "Ingreso Mediano",
    group = "GENERO",
    prefix = "$ "),
  ingreso_medio_mediano = list(
    datos = read_rds("data/macroeconomia/grafico_ingreso/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total" & GENERO == "Total") %>% 
      select(-c(REGIÓN, GENERO)) %>% pivot_longer(cols = c(2,3), names_to = "TIPO_DE_INGRESO", values_to = "INGRESO") %>% 
      mutate(TIPO_DE_INGRESO = ifelse(TIPO_DE_INGRESO == "INGRESO_MEDIO", "Ingreso Medio", "Ingreso Mediano")),
    titulo = "Ingreso Medio e Ingreso Mediano",
    columna_x = "AÑO",
    columna_y = "INGRESO",
    y_axis_title = "Ingreso Medio e Ingreso Mediano",
    group = "TIPO_DE_INGRESO",
    prefix = "$ "))

# Sección Pensiones -------------------------------------------------------

# Gráfico General ---------------------------------------------------------

grafico_config_general <- list(
  afiliados_activos_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_activos_afp_totales.rds"),
    titulo = "Afiliados Activos por AFP",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE AFILIADOS ACTIVOS",
    y_axis_title = "Número de Afiliados Activos",
    group = "AFP"
  ),
  afiliados_por_tipo = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_tipo_totales.rds"),
    titulo = "Afiliados por Tipo de Relación Laboral",
    columna_x = "AÑO",
    columna_y = "AFILIADOS POR TIPO DE RELACION LABORAL",
    y_axis_title = "Número de Afiliados por Tipo de Relación Laboral",
    group = "TIPO - SEXO"
  ),
  afiliados_por_region = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_region.rds"),
    titulo = "Afiliados por Región",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE AFILIADOS POR REGIÓN",
    y_axis_title = "Número de Afiliados por Región",
    group = "REGIÓN"
  ),
  afiliados_por_edad = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_edad.rds"),
    titulo = "Afiliados por Rango de Edad",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE AFILIADOS POR EDAD",
    y_axis_title = "Número de Afiliados por Rango de Edad",
    group = "RANGO EDAD"
  ),
  afiliados_por_region_edad = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_region_edad_totales.rds"),
    titulo = "Afiliados por Rango de Edad y Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Número de Afiliados por Rango de Edad y Región",
    group = c("REGIÓN","RANGO EDAD")
  ),
  afiliados_por_edad_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_edad_afp_totales.rds"),
    titulo = "Afiliados por AFP y Rango Edad",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Número de Afiliados por AFP y Rango de Edad",
    group = c("AFP","RANGO EDAD")
  ),
  afiliados_por_region_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/afiliados_region_afp_totales.rds"),
    titulo = "Afiliados por Región y AFP",
    columna_x = "AÑO",
    columna_y = "NUMERO DE AFILIADOS",
    y_axis_title = "Número de Afiliados por Región y AFP",
    group = c("AFP","REGIÓN")
  ),
  cotizantes_activos_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_mes_afp_totales.rds"),
    titulo = "Cotizantes Activos por AFP",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE COTIZANTES ACTIVOS",
    y_axis_title = "Número de Cotizantes Activos",
    group = "AFP"
  ),
  cotizantes_por_tipo = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_tipo_totales.rds"),
    titulo = "Cotizantes por Tipo de Relación Laboral",
    columna_x = "AÑO",
    columna_y = "COTIZANTES POR TIPO DE RELACION LABORAL",
    y_axis_title = "Número de Cotizantes por Tipo de Relación Laboral",
    group = "TIPO - SEXO"
  ),
  cotizantes_por_region = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_region.rds"),
    titulo = "Cotizantes por Región",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE COTIZANTES POR REGIÓN",
    y_axis_title = "Número de Cotizantes por Región",
    group = "REGIÓN"
  ),
  cotizantes_por_edad = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_edad.rds"),
    titulo = "Cotizantes por Rango de Edad",
    columna_x = "AÑO",
    columna_y = "NÚMERO DE COTIZANTES POR EDAD",
    y_axis_title = "Número de Cotizantes por Rango de Edad",
    group = "RANGO EDAD"
  ),
  cotizantes_por_edad_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_edad_afp_totales.rds"),
    titulo = "Cotizantes por AFP y Rango Edad",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Número de Cotizantes por AFP y Rango de Edad",
    group = c("AFP","RANGO EDAD")
  ),
  cotizantes_por_region_afp = list(
    datos = read_rds("data/pensiones/grafico_totales_generales/cotizantes_region_afp_totales.rds"),
    titulo = "Cotizantes por Región y AFP",
    columna_x = "AÑO",
    columna_y = "NUMERO DE COTIZANTES",
    y_axis_title = "Número de Cotizantes por Región y AFP",
    group = c("AFP","REGIÓN")
  ))

# Gráfico Género ----------------------------------------------------------

grafico_config_genero <- list(
  afiliados_tipo_sexo_dependientes = list(
    datos = read_rds("data/pensiones/grafico_genero/afiliados_tipo_sexo_dependientes.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Afiliados Dependientes por Género",
    subtitulo = "Por Género"
  ),
  afiliados_tipo_sexo_independientes = list(
    datos = read_rds("data/pensiones/grafico_genero/afiliados_tipo_sexo_independientes.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Afiliados Independientes por Género",
    subtitulo = "Por Género"
  ),
  afiliados_tipo_sexo_voluntarios = list(
    datos = read_rds("data/pensiones/grafico_genero/afiliados_tipo_sexo_voluntarios.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Afiliados Voluntarios por Género",
    subtitulo = "Por Género"
  ),
  afiliados_tipo_sexo_totales = list(
    datos = read_rds("data/pensiones/grafico_genero/afiliados_tipo_sexo_totales.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Afiliados por Género Totales",
    subtitulo = "Totales"
  ),
  cotizantes_tipo_sexo_dependientes = list(
    datos = read_rds("data/pensiones/grafico_genero/cotizantes_tipo_sexo_dependientes.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Cotizantes Dependientes por Género",
    subtitulo = "Por Género"
  ),
  cotizantes_tipo_sexo_independientes = list(
    datos = read_rds("data/pensiones/grafico_genero/cotizantes_tipo_sexo_independientes.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Cotizantes Independientes por Género",
    subtitulo = "Por Género"
  ),
  cotizantes_tipo_sexo_voluntarios = list(
    datos = read_rds("data/pensiones/grafico_genero/cotizantes_tipo_sexo_voluntarios.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Cotizantes Voluntarios por Género",
    subtitulo = "Por Género"
  ),
  cotizantes_tipo_sexo_totales = list(
    datos = read_rds("data/pensiones/grafico_genero/cotizantes_tipo_sexo_totales.rds") %>% mutate(AÑO = as.numeric(AÑO)),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Cotizantes por Género Totales",
    subtitulo = "Totales"
  ),
  edad_prom_pens_vejez_sexo = list(
    datos = read_rds("data/pensiones/grafico_genero/edad_prom_pens_vejez_edad_sexo.rds"),
    columna_x = "AÑO",
    columna_y = "EDAD PROMEDIO",
    group = "SEXO",
    titulo = "Edad Promedio de Pensionados por Vejez",
    subtitulo = "Totales"),
  edad_prom_pens_anticipada_vejez_sexo = list(
    datos = read_rds("data/pensiones/grafico_genero/edad_prom_pens_anticipada_vejez_edad_sexo.rds"),
    columna_x = "AÑO",
    columna_y = "EDAD PROMEDIO",
    group = "SEXO",
    titulo = "Edad Promedio de Pensionados por Vejez Anticipada",
    subtitulo = "Totales"))

# Gráficos 5 --------------------------------------------------------------

grafico_config_5 <- list(
  evolucion_inversion_est = list(
    datos = read_rds("data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal.rds") %>% mutate(AÑO = year(AÑO)),
    titulo = "Inversión Sector Estatal",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Estatal"
  ),
  evolucion_inversion_fin = list(
    datos = read_rds("data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero.rds") %>% mutate(AÑO = year(AÑO)),
    titulo = "Inversión Sector Financiero",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Financiero"
  ),
  evolucion_inversion_emp = list(
    datos = read_rds("data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas.rds") %>% mutate(AÑO = year(AÑO)),
    titulo = "Inversión Sector Empresas",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Empresas"
  ),
  evolucion_inversion_ext = list(
    datos = read_rds("data/pensiones/grafico_5/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero.rds") %>% mutate(AÑO = year(AÑO)),
    titulo = "Inversión Sector Extranjero",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Extranjero"
  ))

# Gráficos 6 --------------------------------------------------------------

grafico_config_6 <- list(
  variacon_patrimonial_i = list(
    datos = read_rds("data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_patrimonio_inicial.rds"),
    titulo = "Patrimonio Inicial Sistema",
    columna_x = "AÑO",
    columna_y = "PATRIMONIO INICIAL",
    y_axis_title = "Patrimonio Inicial"
  ),
  variacon_patrimonial_a = list(
    datos = read_rds("data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_aumentos.rds"),
    titulo = "Aumento Patrimonial Sistema",
    columna_x = "AÑO",
    columna_y = "AUMENTOS",
    y_axis_title = "Aumento Patrimonial"
  ),
  variacon_patrimonial_d = list(
    datos = read_rds("data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_disminuciones.rds"),
    titulo = "Disminución Patrimonial Sistema",
    columna_x = "AÑO",
    columna_y = "DISMINUCIONES",
    y_axis_title = "Disminución Patrimonial"
  ),
  variacon_patrimonial_f = list(
    datos = read_rds("data/pensiones/grafico_6/variacion_patrimonial_fondos_pensiones_patrimonio_final.rds"),
    titulo = "Patrimonio Final Sistema",
    columna_x = "AÑO",
    columna_y = "PATRIMONIO FINAL",
    y_axis_title = "Patrimonio Final"
  ))

# Gráficos 7 --------------------------------------------------------------

grafico_config_7 <- list(
  rentabilidad_a = list(
    datos = read_rds("data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_totales.rds"),
    titulo = "Rentabilidad Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondos A",
    group = "AFP"
  ),
  rentabilidad_b = list(
    datos = read_rds("data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_totales.rds"),
    titulo = "Rentabilidad Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondos B",
    group = "AFP"
  ),
  rentabilidad_c = list(
    datos = read_rds("data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_totales.rds"),
    titulo = "Rentabilidad Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondos C",
    group = "AFP"
  ),
  rentabilidad_d = list(
    datos = read_rds("data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_totales.rds"),
    titulo = "Rentabilidad Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondos D",
    group = "AFP"
  ),
  rentabilidad_e = list(
    datos = read_rds("data/pensiones/grafico_7/rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_totales.rds"),
    titulo = "Rentabilidad Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondos E",
    group = "AFP"
  ))

# Gráfico AFP's -----------------------------------------------------------

grafico_config_afps <- list(
  rentabilidad_fondo_a_afps = list(
    datos = read_rds("data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_a_deflactada_uf_afp.rds"),
    titulo = "Rentabilidad Fondo A por AFP",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondo A por AFP",
    group = "AFP",
    suffix = "%",
    prefix = ""
  ),
  rentabilidad_fondo_b_afps = list(
    datos = read_rds("data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_b_deflactada_uf_afp.rds"),
    titulo = "Rentabilidad Fondo B por AFP",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondo B por AFP",
    group = "AFP",
    suffix = "%",
    prefix = ""
  ),
  rentabilidad_fondo_c_afps = list(
    datos = read_rds("data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_c_deflactada_uf_afp.rds"),
    titulo = "Rentabilidad Fondo C por AFP",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondo C por AFP",
    group = "AFP",
    suffix = "%",
    prefix = ""
  ),
  rentabilidad_fondo_d_afps = list(
    datos = read_rds("data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_d_deflactada_uf_afp.rds"),
    titulo = "Rentabilidad Fondo D por AFP",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondo D por AFP",
    group = "AFP",
    suffix = "%",
    prefix = ""
  ),
  rentabilidad_fondo_e_afps = list(
    datos = read_rds("data/pensiones/grafico_afps/rentabilidad_real_anual_fondo_pensiones_tipo_e_deflactada_uf_afp.rds"),
    titulo = "Rentabilidad Fondo E por AFP",
    columna_x = "AÑO",
    columna_y = "RENTABILIDAD",
    y_axis_title = "Rentabilidad Fondo E por AFP",
    group = "AFP",
    suffix = "%",
    prefix = ""
  ),
  evolucion_inversion_sector_estatal = list(
    datos = read_rds("data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_estatal_totales.rds"),
    titulo = "Inversión Sector Estatal",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Estatal en Porcentaje",
    group = "SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS",
    suffix = "%",
    prefix = ""
  ),
  evolucion_inversion_sector_financiero = list(
    datos = read_rds("data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_financiero_totales.rds"),
    titulo = "Inversión Sector Financiero",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Financiero en Porcentaje",
    group = "SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS",
    suffix = "%",
    prefix = ""
  ),
  evolucion_inversion_sector_empresas = list(
    datos = read_rds("data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_empresas_totales.rds"),
    titulo = "Inversión Sector Empresas",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Empresas en Porcentaje",
    group = "SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS",
    suffix = "%",
    prefix = ""
  ),
  evolucion_inversion_sector_extranjero = list(
    datos = read_rds("data/pensiones/grafico_afps/evolucion_inversion_fondos_pensiones_sector_institucional_instrumentos_financieros_sector_extranjero_totales.rds"),
    titulo = "Inversión Sector Extranjero",
    columna_x = "AÑO",
    columna_y = "PORCENTAJE",
    y_axis_title = "Inversión Sector Extranjero en Porcentaje",
    group = "SECTOR INSTITUCIONAL E INSTRUMENTOS FINANCIEROS",
    suffix = "%",
    prefix = ""
  ),
  variacion_patrimonial_aumentos = list(
    datos = read_rds("data/pensiones/grafico_afps/variacion_patrimonial_fondos_pensiones_aumentos.rds"),
    titulo = "Variación Patrimonial (Aumentos)",
    columna_x = "AÑO",
    columna_y = "VARIACION PATRIMONIAL",
    y_axis_title = "Variación Patrimonial en Millones de Pesos",
    group = "TIPO DE AUMENTO PATRIMONIAL",
    suffix = "",
    prefix = "$"
  ),
  variacion_patrimonial_disminuciones = list(
    datos = read_rds("data/pensiones/grafico_afps/variacion_patrimonial_fondos_pensiones_disminuciones.rds") %>%
      mutate(`VARIACION PATRIMONIAL` = round(`VARIACION PATRIMONIAL`, digits = 3)),
    titulo = "Variación Patrimonial (Disminuciones)",
    columna_x = "AÑO",
    columna_y = "VARIACION PATRIMONIAL",
    y_axis_title = "Variación Patrimonial en Millones de Pesos",
    group = "TIPO DE DISMINUCION PATRIMONIAL",
    suffix = "",
    prefix = "$"
  ),
  activos_fondos_pensiones_anuales_afp = list(
    datos = read_rds("data/pensiones/grafico_afps/activos_fondos_pensiones_anuales_afp.rds") %>% 
      mutate(ACTIVOS = round(ACTIVOS, digits = 2)),
    titulo = "Activos Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "ACTIVOS",
    y_axis_title = "Activos Fondos de Pensiones",
    group = "AFP",
    suffix = "",
    prefix = "$M "),
  pasivos_fondos_pensiones_anuales_afp = list(
    datos = read_rds("data/pensiones/grafico_afps/pasivos_fondos_pensiones_totales.rds"),
    titulo = "Pasivos Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "PASIVOS",
    y_axis_title = "Pasivos Fondos de Pensiones",
    group = "AFP",
    suffix = "",
    prefix = "$M "),
  valor_fondos_pensiones_afp = list(
    datos = read_rds("data/pensiones/grafico_afps/valor_fondos_pensiones_anuales_afp.rds") %>% mutate(`VALOR FONDOS` = round(`VALOR FONDOS`, digits = 2)),
    titulo = "Valores de Fondos de Pensiones",
    columna_x = "AÑO",
    columna_y = "VALOR FONDOS",
    y_axis_title = "Valores de Fondos de Pensiones",
    group = "AFP",
    suffix = "",
    prefix = "$M "))


# Gráficos 8 --------------------------------------------------------------

grafico_config_8 <- list(
  serie_pilar3_pbs_invalidez = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_pbs_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ),
  serie_pilar3_pbs_vejez = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_pbs_vejez_pgu_noncon.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ),
  serie_pilar3_aps_invalidez = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_aps_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ),
  serie_pilar3_aps_vejez = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_aps_vejez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ),
  serie_pilar3_articulo9 = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_articulo9.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ),
  serie_pilar3_pgu_contributiva = list(
    datos = read_rds("data/pensiones/grafico_9/serie_pilar3_pgu_contributiva.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Región",
    group = "REGIÓN"
  ))

# Gráficos 9 --------------------------------------------------------------

grafico_config_9 <- list(
  serie_pilar1_pbs_invalidez = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_pbs_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ),
  serie_pilar1_pbs_vejez = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_pbs_vejez_pgu_noncon.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ),
  serie_pilar1_aps_invalidez = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_aps_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ),
  serie_pilar1_aps_vejez = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_aps_vejez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ),
  serie_pilar1_articulo9 = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_articulo9.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ),
  serie_pilar1_pgu_contributiva = list(
    datos = read_rds("data/pensiones/grafico_8/serie_pilar1_pgu_contributiva.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU a Diciembre de cada Año",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO"
  ))

# Gráfico 10 --------------------------------------------------------------

grafico_config_10 <- list(
  serie_pilar5_pbs_invalidez = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_pbs_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_pgu_con = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_pgu_con.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_pgu_noncon = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_pgu_noncon.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_aps_invalidez = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_aps_invalidez.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_aps_vejez_pengan = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_aps_vejez_pengan.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_aps_vejez_pengan_sol = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_aps_vejez_pengan_sol.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_aps_vejez_subdef = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_aps_vejez_subdef.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ),
  serie_pilar5_articulo9 = list(
    datos = read_rds("data/pensiones/grafico_10/serie_pilar5_beneficio_art9.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU por Institución",
    group = "TIPO DE ENTIDAD PRINCIPAL"
  ))

# Gráfico Pilar Solidario y PGU -------------------------------------------

grafico_config_pilarpgu <- list(
  serie_pilar1_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar1_totales.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU Mensuales",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios totales del Pilar Solidario y PGU por Año",
    group = "TIPO DE BENEFICIO",
    prefix = "",
    suffix = "",
    macrozonas = NULL
  ),
  serie_pilar2_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar2_totales.rds"),
    titulo = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU",
    group = "TIPO DE BENEFICIO",
    prefix = "$MM ",
    suffix = "",
    macrozonas = NULL
  ),
  serie_pilar3_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar3_totales.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU Mensuales por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU Mensuales por Región",
    group = c("REGIÓN", "TIPO DE BENEFICIO"),
    prefix = "",
    suffix = "",
    macrozonas = "MACROZONA"
  ),
  serie_pilar4_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar4_totales.rds"),
    titulo = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Región",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Región",
    group = c("REGIÓN", "TIPO DE BENEFICIO"),
    prefix = "$MM ",
    suffix = "",
    macrozonas = "MACROZONA"),
  serie_pilar5_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar5_totales.rds"),
    titulo = "Beneficiarios del Pilar Solidario y PGU Mensuales por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios del Pilar Solidario y PGU Mensuales por Institución",
    group = c("TIPO DE ENTIDAD PRINCIPAL", "TIPO DE BENEFICIO"),
    prefix = "",
    suffix = "",
    macrozonas = NULL),
  serie_pilar6_totales = list(
    datos = read_rds("data/pensiones/grafico_pilarpgu/serie_pilar6_totales.rds"),
    titulo = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Institución",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Institución",
    group = c("TIPO DE ENTIDAD PRINCIPAL", "TIPO DE BENEFICIO"),
    prefix = "$MM ",
    suffix = "",
    macrozonas = NULL))

# Gráfico Género 2 --------------------------------------------------------

grafico_config_genero2 <- list(
  serie_pilar1_pbsvejez_pgunon = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "PBS Vejez / PGU No Contributiva1"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios PBS Vejez / PGU No Contributiva según Género",
    subtitulo = "Por Género"
  ),
  serie_pilar1_pbsinvalidez = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "PBS Invalidez"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios PBS Invalidez según Género",
    subtitulo = "Por Género"
  ),
  serie_pilar1_apsvejez = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "APS Vejez 2"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios APS Vejez según Género",
    subtitulo = "Por Género"
  ),
  serie_pilar1_apsinvalidez = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "APS Invalidez"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios APS Invalidez según Género",
    subtitulo = "Por Género"
  ),
  serie_pilar1_pgucon = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "PGU Contributiva 1"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios PGU Contributiva según Género",
    subtitulo = "Por Género"
  ),
  serie_pilar1_art9 = list(
    datos = read_rds("data/pensiones/grafico_genero2/serie_pilar1_totales_sexo.rds") %>% filter(`TIPO DE BENEFICIO` == "Artículo 9°bis 3"),
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios Articulo 9º Bis según Género",
    subtitulo = "Por Género"
  ))

# Gráfico Gasto Salud -----------------------------------------------------

# Gráfico General ---------------------------------------------------------

grafico_config_general_salud <- list(
  beneficiarios_sistema_salud = list(
    datos = read_rds("data/salud/grafico_general_salud/beneficiarios_salud.rds"),
    titulo = "Beneficiarios Sistema de Salud",
    subtitulo = "",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios Sistema de Salud",
    group = "COBERTURA"
  ),
  cotizantes_sistema_salud = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_salud.rds"),
    titulo = "Cotizantes Sistema de Salud",
    subtitulo = "",
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Cotizantes Sistema de Salud",
    group = "COBERTURA"
  ),
  beneficiarios_sistema_salud_region = list(
    datos = read_rds("data/salud/grafico_general_salud/beneficiarios_salud_region.rds") %>% filter(!REGIÓN %in% c("Total")),
    titulo = "Beneficiarios Sistema de Salud Por Región",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios Sistema de Salud por Región",
    group = c("REGIÓN", "COBERTURA"),
    prefix = "",
    suffix = "",
    macrozona = "MACROZONA"),
  cotizantes_sistema_salud_region = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_salud_region.rds") %>% filter(!REGIÓN %in% c("Total")),
    titulo = "Cotizantes Sistema de Salud Por Región",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Cotizantes Sistema de Salud por Región",
    group = c("REGIÓN", "COBERTURA"),
    prefix = "",
    suffix = "",
    macrozona = "MACROZONA"),
  beneficiarios_fonasa_edad = list(
    datos = read_rds("data/salud/grafico_general_salud/beneficiarios_fonasa_edad_total.rds") %>% filter(RANGO_EDAD != "TOTAL"),
    titulo = "Beneficiarios FONASA Por Rango de Edad",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios FONASA por Rango de Edad",
    group = "RANGO_EDAD",
    prefix = "",
    suffix = "",
    macrozona = ""),
  cotizantes_fonasa_edad = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_fonasa_edad_total.rds") %>% filter(RANGO_EDAD != "TOTAL"),
    titulo = "Cotizantes FONASA Por Rango de Edad",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Cotizantes FONASA por Rango de Edad",
    group = "RANGO_EDAD",
    prefix = "",
    suffix = "",
    macrozona = ""),
  beneficiarios_isapre_edad = list(
    datos = read_rds("data/salud/grafico_general_salud/beneficiarios_isapres_edad_total.rds") %>% filter(RANGO_EDAD != "Total"),
    titulo = "Beneficiarios ISAPRE Por Rango de Edad",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Beneficiarios ISAPRE por Rango de Edad",
    group = "RANGO_EDAD",
    prefix = "",
    suffix = "",
    macrozona = ""),
  cotizantes_isapre_edad = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_isapre_edad_total.rds") %>% filter(RANGO_EDAD != "Total"),
    titulo = "Cotizantes ISAPRE Por Rango de Edad",
    subtitulo = NULL,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    y_axis_title = "Cotizantes ISAPRE por Rango de Edad",
    group = "RANGO_EDAD",
    prefix = "",
    suffix = "",
    macrozona = ""),
  cotizantes_fonasa_laboral = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_fonasa_relacion_laboral.rds") %>% filter(`TIPO DE RELACION LABORAL` != "Total general"),
    titulo = "Cotizantes FONASA",
    subtitulo = "Por Tipo de Relación Laboral",
    columna_x = "AÑO",
    columna_y = "COTIZANTES POR TIPO DE RELACION LABORAL",
    y_axis_title = "Cotizantes FONASA por Tipo de Relación Laboral",
    group = "TIPO DE RELACION LABORAL",
    prefix = "",
    suffix = "",
    macrozona = ""),
  cotizantes_isapre_laboral = list(
    datos = read_rds("data/salud/grafico_general_salud/cotizantes_isapre_relacion_laboral.rds") %>% filter(`TIPO DE RELACION LABORAL` != "Total"),
    titulo = "Cotizantes ISAPRE",
    subtitulo = "Por Tipo de Relación Laboral",
    columna_x = "AÑO",
    columna_y = "COTIZANTES POR TIPO DE RELACION LABORAL",
    y_axis_title = "Cotizantes ISAPRE por Tipo de Relación Laboral",
    group = "TIPO DE RELACION LABORAL",
    prefix = "",
    suffix = "",
    macrozona = ""))

# Gráfico Genero Salud ----------------------------------------------------
 
grafico_config_genero_salud <- list(
  beneficiarios_fonasa_genero = list(
    datos = read_rds("data/salud/grafico_genero_salud/beneficiarios_fonasa_genero.rds") %>% filter(GENERO != "TOTAL") ,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Beneficiarios FONASA",
    subtitulo = "Por Género"
  ),
  cotizantes_fonasa_genero = list(
    datos = read_rds("data/salud/grafico_genero_salud/cotizantes_fonasa_genero.rds") %>% filter(GENERO != "TOTAL") ,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "GENERO",
    titulo = "Cotizantes FONASA",
    subtitulo = "Por Género"
  ),
  beneficiarios_isapre_genero = list(
    datos = read_rds("data/salud/grafico_genero_salud/beneficiarios_isapres_genero.rds") %>% filter(SEXO != "Total") ,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Beneficiarios ISAPRE",
    subtitulo = "Por Género"
  ),
  cotizantes_isapre_genero = list(
    datos = read_rds("data/salud/grafico_genero_salud/cotizantes_isapres_genero.rds") %>% filter(SEXO != "Total") ,
    columna_x = "AÑO",
    columna_y = "CANTIDAD",
    group = "SEXO",
    titulo = "Cotizantes ISAPRE",
    subtitulo = "Por Género"))

# Gráfico Treemap Salud ---------------------------------------------------

grafico_config_treemap <-  list(
    datos = read_rds("data/salud/grafico_treemap/beneficiarios_por_tramos.rds") %>% filter(REGIÓN != "Total Nacional"),
    titulo = "Tramos de Beneficiarios de FONASA por Año")

# Gráfico ISAPRE ----------------------------------------------------------

grafico_config_isapre <- list(
  beneficiarios_isapre_abiertas = list(
    datos = read_rds("data/salud/grafico_isapre/beneficiarios_salud_por_isapre_abiertas.rds") %>% filter(ISAPRES != "Total Isapres Abiertas") ,
    columna_x = "AÑO",
    columna_y = "BENEFICIARIOS ISAPRES ABIERTAS",
    group = "ISAPRES",
    titulo = "Beneficiarios ISAPRE ABIERTAS",
    subtitulo = "Por Aseguradora",
    suffix = "",
    prefix= "",
    y_axis_title = "Beneficiarios ISAPRE por Aseguradora"
  ), 
  beneficiarios_isapre_cerradas = list(
    datos = read_rds("data/salud/grafico_isapre/beneficiarios_salud_por_isapre_cerradas.rds") %>% filter(ISAPRES != "Total Isapres Cerradas") ,
    columna_x = "AÑO",
    columna_y = "BENEFICIARIOS ISAPRES CERRADAS",
    group = "ISAPRES",
    titulo = "Beneficiarios ISAPRE CERRADAS",
    subtitulo = "Por Aseguradora",
    suffix = "",
    prefix= "",
    y_axis_title = "Beneficiarios ISAPRE por Aseguradora"
  ),
  cotizantes_isapre_abiertas = list(
    datos = read_rds("data/salud/grafico_isapre/cotizantes_salud_por_isapre_abiertas.rds") %>% filter(ISAPRES != "Total Isapres Abiertas") ,
    columna_x = "AÑO",
    columna_y = "COTIZANTES ISAPRES ABIERTAS",
    group = "ISAPRES",
    titulo = "Cotizantes ISAPRE ABIERTAS",
    subtitulo = "Por Aseguradora",
    suffix = "",
    prefix= "",
    y_axis_title = "Cotizantes ISAPRE por Aseguradora"
  ),
  cotizantes_isapre_cerradas = list(
    datos = read_rds("data/salud/grafico_isapre/cotizantes_salud_por_isapre_cerradas.rds") %>% filter(ISAPRES != "Total Isapres Cerradas") ,
    columna_x = "AÑO",
    columna_y = "COTIZANTES ISAPRES CERRADAS",
    group = "ISAPRES",
    titulo = "Cotizantes ISAPRE CERRADAS",
    subtitulo = "Por Aseguradora",
    suffix = "",
    prefix= "",
    y_axis_title = "Cotizantes ISAPRE por Aseguradora"
  ),
  estadisticas_movilidad = list(
    datos = read_rds("data/salud/grafico_isapre/estadisticas_movilidad.rds") %>% filter(`TRAMOS DE EDAD` == "Total"),
    titulo = "Cotizantes de ISAPRE según Movilidad",
    subtitulo = "",
    columna_x = "AÑO",
    columna_y = "NUMERO DE COTIZANTES",
    y_axis_title = "Cotizantes de ISAPRE según Movilidad",
    group = "CONDICION COTIZANTES"
  ))


grafico_config_isapre2 <- list(
  distribucion_tipo_plan_i_abiertas = list(
    datos = read_rds("data/salud/grafico_isapre/beneficiarios_tipo_plan.rds") %>% filter(ISAPRE %in% c("Isapres Abiertas")) %>% 
      select(AÑO, `TIPO DE PLAN`, `NUMERO DE BENEFICIARIOS`),
    columna_x = "AÑO",
    columna_y = "NUMERO DE BENEFICIARIOS",
    group = "TIPO DE PLAN",
    titulo = "Distribución Beneficiarios por Tipo de Plan ISAPRES abiertas",
    subtitulo = "ISAPRES Abiertas",
    suffix = "",
    prefix= "",
    y_axis_title = "Número de Beneficiarios"
  ),
  distribucion_tipo_plan_i_abiertas2 = list(
    datos = read_rds("data/salud/grafico_isapre/cotizantes_tipo_plan.rds") %>% filter(ISAPRE %in% c("Isapres Abiertas")) %>% 
      select(AÑO, `TIPO DE PLAN`, `NUMERO DE COTIZANTES`),
    columna_x = "AÑO",
    columna_y = "NUMERO DE COTIZANTES",
    group = "TIPO DE PLAN",
    titulo = "Distribución Cotizantes por Tipo de Plan ISAPRES abiertas",
    subtitulo = "ISAPRES Abiertas",
    suffix = "",
    prefix= "",
    y_axis_title = "Número de Cotizantes"
  ),
  distribucion_tipo_plan_i_cerradas = list(
    datos = read_rds("data/salud/grafico_isapre/beneficiarios_tipo_plan.rds") %>% filter(ISAPRE %in%  c("Isapres Cerradas")) %>% select(AÑO, `TIPO DE PLAN`, `NUMERO DE BENEFICIARIOS`),
    columna_x = "AÑO",
    columna_y = "NUMERO DE BENEFICIARIOS",
    group = "TIPO DE PLAN",
    titulo = "Distribución Beneficiarios por Tipo de Plan ISAPRES cerradas",
    subtitulo = "ISAPRES Cerradas",
    suffix = "",
    prefix= "",
    y_axis_title = "Número de Beneficiarios"
  ),
  distribucion_tipo_plan_i_cerradas2 = list(
    datos = read_rds("data/salud/grafico_isapre/cotizantes_tipo_plan.rds") %>% filter(ISAPRE %in% c("Isapres Cerradas")) %>% select(AÑO, `TIPO DE PLAN`, `NUMERO DE COTIZANTES`),
    columna_x = "AÑO",
    columna_y = "NUMERO DE COTIZANTES",
    group = "TIPO DE PLAN",
    titulo = "Distribución Cotizantes por Tipo de Plan ISAPRES cerradas",
    subtitulo = "ISAPRES Cerradas",
    suffix = "",
    prefix= "",
    y_axis_title = "Número de Cotizantes"
  ))

# Gráfico Movilidad ISAPRE ------------------------------------------------

grafico_config_financiero_isapre <- list(
  ingreso_actividades_ordinarias = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Ingresos de actividades ordinarias") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Ingresos de actividades ordinarias",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Ingresos de actividades ordinarias"
  ),
  costo_de_ventas = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Costo de ventas (menos)") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Costo de ventas (menos)",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Costo de ventas (menos)"
  ),
  ganancia_bruta = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Ganancia bruta") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Ganancia bruta",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Ganancia bruta"
  ),
  gastos_de_administracion = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Gastos de administración y otros gastos por función (menos)") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Gastos de administración y otros gastos por función (menos)",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Gastos de administración y otros gastos por función (menos)"
  ),
  otros_items = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Otros items de ingresos y egresos (1)") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Otros items de ingresos y egresos",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Otros items de ingresos y egresos"
  ),
  gananas_pre_impuestos = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Ganancia (pérdida) antes de impuestos") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Ganancia (pérdida) antes de impuestos",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Ganancia (pérdida) antes de impuestos"
  ),
  gastos_por_impuestos = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Gasto por impuestos a las ganancias (menos)") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Gasto por impuestos a las ganancias (menos)",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Gasto por impuestos a las ganancias (menos)"
  ),
  ganancia_perdida = list(
    datos = read_rds("data/salud/grafico_isapre/informacion_financiera_isapres.rds") %>% filter(Rubros == "Ganancia (pérdida)") ,
    columna_x = "AÑO",
    columna_y = "MILLONES DE PESOS",
    group = "Rubros",
    titulo = "Ganancia (pérdida)",
    subtitulo = "Sistema ISAPRE",
    suffix = "",
    prefix= "$ ",
    y_axis_title = "Ganancia (pérdida)"
  ))

