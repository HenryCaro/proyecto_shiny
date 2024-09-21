source("R-scripts/99-shiny-helpers.R")

ui <- navbarPage(title = "PROYECTO", theme = shinytheme("united"), footer = includeHTML("www/footer_web.html"),
                
                 tabPanel("Proyecto", fluidRow(column(3),column(6, includeHTML("./www/BLOQUE1.html")), column(3)),
                          
                          fluidRow(style = "height:50px;"),
                          tags$hr(),

                          fluidRow(column(7, style = "display: flex; flex-direction: column; align-items: center; justify-content: flex-start; height: 600px;", 
                                          includeHTML("./www/BLOQUE2.html")), 
                                   column(5, style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 600px;", 
                                                   div(class="panel-body", style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center;  border: none;",
                                                          div(tags$img(src = "imagen_inicio.jpg",
                                                                       style = "border: none; margin: 0; padding: 0; display: block;",
                                                                       width = "400px", height = "400px"))))),
                          fluidRow(style = "height:50px;"),
                          tags$hr(),
                          
                          fluidRow(column(3), column(6, includeHTML("./www/BLOQUE3.html")), column(3)),
                          
                          fluidRow(style = "height:50px;"),
                          tags$hr(),

                          fluidRow(column(3), column(6, includeHTML("./www/BLOQUE4.html"),
                                                     tags$div(style = "height:50px;"),
                                                     tags$div(class = "wrap", style="text-align: center;",
                                                              div(class = "center", 
                                                                  style="display: inline-block; vertical-align: top; width: 225px; text-align: center;",
                                                                  tags$a("Sistema de Pensiones",
                                                                         onclick = "window.open('https://www.spensiones.cl/', '_blank')",
                                                                         class="btn btn-primary btn-lg")),
                                                              div(class = "center",
                                                                  style="display: inline-block; vertical-align: top; width: 75px; text-align: center;",
                                                                  tags$br()),
                                                              div(class = "center",
                                                                  style="display: inline-block; vertical-align: top; width: 225px; text-align: center;",
                                                                  tags$a("Sistema de Salud", 
                                                                         onclick="window.open('https://www.superdesalud.gob.cl/', '_blank')",
                                                                         class="btn btn-primary btn-lg")))), column(3)),
                          fluidRow(style = "height:50px;")),
                 
                 tabPanel("Macroeconomía", fluidRow(column(3),column(6, includeHTML("./www/BLOQUE1.html")), column(3)),
                          
                          fluidRow(style = "height:50px;"),
                          tags$hr(),
                          fluidRow(style = "height:50px;"),
                          
                          fluidRow(
                            div(style = "display: flex; justify-content: space-between; height: 600px;",
                                
                                div(style = "position: relative; display: flex; flex-direction: column; align-items: flex-start;",
                                    div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                                        div(style = "position: relative; text-align: center; color: black;",
                                            pickerInput(inputId = "datos_poblacion",
                                                        label = "Datos Población",
                                                        choices = c("Esperanza de Vida" = "esperanza_de_vida_totales",
                                                                    "Proyección Población" = "proyeccion_totales", 
                                                                    "Estimado Población Ocupada" = "estimacion_ocupados",
                                                                    "Estimación Población Ocupada Formal" = "estimacion_ocupados_form",
                                                                    "Estimación Población Ocupada Informal" = "estimacion_ocupados_inf",
                                                                    "Estimación Tasa de Ocupados" = "tasa_ocupados",
                                                                    "Estimación Tasa de Ocupados Formales" = "tasa_ocupados_form",
                                                                    "Estimación Tasa de Ocupados Informales" = "tasa_ocupados_inf",
                                                                    "Estimación Tasa de Desocupación" = "tasa_desocupados"),
                                                        selected = "esperanza_de_vida_totales",
                                                        options = list(style = "btn-danger"))),
                                        div(style = "display: flex; justify-content: center; align-items: center;",
                                            highchartOutput("grafico_poblacion", width = "950px", height = "500px")))),
                                
                                div(style = "flex: 0 0 48%; display: flex; flex-direction: column; align-items: flex-end; padding-right: 20px;",
                                    div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center; overflow: auto;",
                                        div(style = "position: relative; text-align: center; color: black; ",
                                            includeHTML("./www/BLOQUE1_1.html")))))),
                          
                          fluidRow(style = "height:60px;"),
                          tags$hr(),
                          fluidRow(style = "height:60px;"),
                          
                          fluidRow(
                            div(style = "display: flex; justify-content: space-between; height: 600px;",

                                div(style = "flex-grow: 1; display: flex; flex-direction: column; align-items: center;",
                                    div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                                        div(style = "position: relative; text-align: center; color: black;",
                                            pickerInput(inputId = "datos_macroeconomia",
                                                        label = "Datos Macroeconómicos",
                                                        choices = c("PIB a precios corrientes" = "pib_precios_corrientes", 
                                                                    "PIB volumen a precios del año anterior encadenado (desestacionalizado)" = "pib_volumen_encadenado",
                                                                    "PIB volumen a precios del año anterior encadenado" = "pib_volumen_desestacionalizado",
                                                                    "PIB per cápita en USD (referencia 2018)" = "pib_percapita",
                                                                    "PIB per cápita PPP" = "pib_percapita_ppp"),
                                                        selected = "pib_precios_corrientes",
                                                        options = list(style = "btn-danger"))),
                                        div(style = "display: flex; justify-content: center; align-items: center;",
                                            highchartOutput("grafico_macroeconomico", width = "1500px", height = "600px")))))),
                                   
                          fluidRow(style = "height:100px;"),
                          tags$hr(),
                          fluidRow(style = "height:60px;"),
                          
                          fluidRow(
                            div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                div(style = "text-align: center; margin-bottom: 20px;",
                                    pickerInput(inputId = "datos_gasto",
                                                label = "Datos Gasto en Salud",
                                                choices = c("Gasto en Salud como % del PIB" = "gasto_salud_pib",
                                                            "Gasto en Salud en Millones de Pesos Corrientes" = "gasto_salud_pesos",
                                                            "Gasto en Salud en Millones de Pesos Constantes" = "gasto_salud_pesos_constantes",
                                                            "Gasto en Salud Per Cápita en Pesos Corrientes" = "gasto_salud_percapita",
                                                            "Gasto en Salud Per Cápita en Dólares PPA" = "gasto_salud_percapita_ppa",
                                                            "Gasto en Salud por Instituciones" = "gasto_salud_instituciones_t",
                                                            "Gasto en Salud por Secciones Institucionales" = "gasto_salud_instituciones"),
                                                selected = "gasto_salud_pib",
                                                options = list(style = "btn-danger"))),

                                div(style = "text-align: center; margin-bottom: 20px; min-height: 50px;",  
                                    uiOutput("instituciones_picker")),
                                div(style = "display: flex; align-items: center; justify-content: center;",
                                    highchartOutput("grafico_gasto", width = "1500px", height = "600px")))),
                          
                          fluidRow(style = "height:60px;"),
                          tags$hr(),
                          fluidRow(style = "height:60px;"),
                          
                          fluidRow(
                            div(style = "display: flex; justify-content: space-between; height: 600px;",
                                
                                div(style = "flex-grow: 1; display: flex; flex-direction: column; align-items: center;",
                                    div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                                        div(style = "position: relative; text-align: center; color: black;",
                                            pickerInput(inputId = "datos_ingresos",
                                                        label = "Datos Ingresos",
                                                        choices = c("Ingreso Medio" = "ingreso_medio",
                                                                    "Ingreso Mediano" = "ingreso_mediano",
                                                                    "Ingreso Medio e Ingreso Mediano" = "ingreso_medio_mediano"),
                                                        selected = "ingreso_medio",
                                                        options = list(style = "btn-danger"))),
                                        div(style = "display: flex; justify-content: center; align-items: center;",
                                            highchartOutput("grafico_ingresos", width = "1500px", height = "500px"))))))),
                 
                 tabPanel("Sistema de Pensiones", fluidRow(class = "top-buffer", style = "margin-top: 20px;",
                                                column(width = 12, class = PARS$classcol,
                                                       valueBoxOutput("grafico_1", width = 3),
                                                       valueBoxOutput("grafico_2", width = 3),
                                                       valueBoxOutput("grafico_3", width = 3),
                                                       valueBoxOutput("grafico_4", width = 3))),
                          
                          fluidRow(style = "height:40px;"),
                          tags$hr(),
                          
                          fluidRow(style = "height:40px;"),                          
                          
                          fluidRow(div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                                      div(style = "position: relative; text-align: center; color: black;",
                                        pickerInput(inputId = "datos_sistema_afp",
                                                    label = "Datos Sistema AFP's",
                                                    choices = c("Afiliados Activos por AFP" = "afiliados_activos_afp",
                                                                "Afiliados por Tipo de Relación Laboral" = "afiliados_por_tipo",
                                                                "Afiliados por Región" = "afiliados_por_region",
                                                                "Afiliados por Región y AFP" = "afiliados_por_region_afp",
                                                                "Afiliados por Rango de Edad" = "afiliados_por_edad",
                                                                "Afiliados por Rango de Edad y Región" = "afiliados_por_region_edad",
                                                                "Afiliados por Rango de Edad y AFP" = "afiliados_por_edad_afp",
                                                                "Cotizantes Activos por AFP" = "cotizantes_activos_afp",
                                                                "Cotizantes por Tipo de Relación Laboral" = "cotizantes_por_tipo",
                                                                "Cotizantes por Región" = "cotizantes_por_region",
                                                                "Cotizantes por Región y AFP" = "cotizantes_por_region_afp",
                                                                "Cotizantes por Rango de Edad" = "cotizantes_por_edad",
                                                                "Cotizantes por Rango de Edad y AFP" = "cotizantes_por_edad_afp"),
                                                    selected = "afiliados_activos_afp",
                                                    options = list(style = "btn-danger"))),
                                  div(style = "height: 20px;"),
                                     div(style = "display: flex; justify-content: center; align-items: center;",
                                        highchartOutput("grafico_general", width = "1500px", height = "500px")))),
                          
                          fluidRow(style = "height:60px;"),
                          
                          fluidRow(div(style = "display: flex; justify-content: space-between;",
                                     div(style = "flex: 0 0 58%;",
                                       highchartOutput("grafico_genero", height = "600px")),
                                     div(style = "flex: 0 0 40%; display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; 
                                                    height: 500px; background-image: url('imagen_piloto.jpg'); background-size: contain; background-position: center; 
                                                       background-repeat: no-repeat;",
                                    div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; text-align: center; 
                                                  color: white; transform: translateY(10%);",
                                        pickerInput(inputId = "tipo_genero",
                                                    label = "Datos según género",
                                                    choices = c("Afiliados Dependientes por Género" = "afiliados_tipo_sexo_dependientes", 
                                                                "Afiliados Independientes por Género" = "afiliados_tipo_sexo_independientes", 
                                                                "Afiliados Voluntarios por Género" = "afiliados_tipo_sexo_voluntarios", 
                                                                "Afiliados Totales por Género" = "afiliados_tipo_sexo_totales",
                                                                "Cotizantes Dependientes por Género" = "cotizantes_tipo_sexo_dependientes", 
                                                                "Cotizantes Independientes por Género" = "cotizantes_tipo_sexo_independientes", 
                                                                "Cotizantes Voluntarios por Género" = "cotizantes_tipo_sexo_voluntarios", 
                                                                "Cotizantes Totales por Género" = "cotizantes_tipo_sexo_totales", 
                                                                "Edad Promedio de Pensionados por Vejez" = "edad_prom_pens_vejez_sexo",
                                                                "Edad Promedio de Pensionados por Vejez Anticipada" = "edad_prom_pens_anticipada_vejez_sexo"),
                                                    selected = "afiliados_tipo_sexo_dependientes",
                                                    options = list(style = "btn-danger")))))),
                          
                          fluidRow(style = "height:60px;"),
                          tags$hr(),
                          fluidRow(style = "height:40px;"),

                          fluidRow(class = "top-buffer", style = "margin-top: 20px;", 
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center; 
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "evolucion_inversion",
                                                                   label = "Evolución Inversión:", 
                                                                   choices = c("Sector Estatal" = "evolucion_inversion_est", "Sector Financiero" = "evolucion_inversion_fin",
                                                                               "Sector Empresas" = "evolucion_inversion_emp", "Sector Extranjero" = "evolucion_inversion_ext"),
                                                                   status = "warning")),
                                             highchartOutput("grafico_5")), 
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center; 
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "variacion_patrimonial",
                                                                   label = "Variación Patrimonial:", 
                                                                   choices = c("Inicial" = "variacon_patrimonial_i" , "Aumentos" = "variacon_patrimonial_a",
                                                                               "Disminuciones" = "variacon_patrimonial_d", "Final" = "variacon_patrimonial_f"),
                                                                   status = "warning")),
                                          highchartOutput("grafico_6")),
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center;
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "Tipo_de_fondos",
                                                                   label = "Fondos de Pensiones",
                                                                   choices = c("A" = "rentabilidad_a", "B" = "rentabilidad_b", "C" = "rentabilidad_c", "D" = "rentabilidad_d", "E" = "rentabilidad_e"),
                                                                   status = "warning")),
                                          highchartOutput("grafico_7"))),
                          
                          fluidRow(style = "height:40px;"),
                          
                          fluidRow(div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                                       div(style = "position: relative; text-align: center; color: black;",
                                           pickerInput(inputId = "tipo_afps",
                                                       label = "Datos Afp's",
                                                       choices = c("Rentabilidad Fondo A por AFP" = "rentabilidad_fondo_a_afps",
                                                                   "Rentabilidad Fondo B por AFP" = "rentabilidad_fondo_b_afps",
                                                                   "Rentabilidad Fondo C por AFP" = "rentabilidad_fondo_c_afps",
                                                                   "Rentabilidad Fondo D por AFP" = "rentabilidad_fondo_d_afps",
                                                                   "Rentabilidad Fondo E por AFP" = "rentabilidad_fondo_e_afps",
                                                                   "Activos Fondos de Pensiones" = "activos_fondos_pensiones_anuales_afp",
                                                                   "Pasivos Fondos de Pensiones" = "pasivos_fondos_pensiones_anuales_afp",
                                                                   "Inversión Sector Estatal" = "evolucion_inversion_sector_estatal",
                                                                   "Inversión Sector Financiero" = "evolucion_inversion_sector_financiero",
                                                                   "Inversión Sector Empresas" = "evolucion_inversion_sector_empresas",
                                                                   "Inversión Sector Extranjero" = "evolucion_inversion_sector_extranjero",
                                                                   "Variación Patrimonial (Aumentos)" = "variacion_patrimonial_aumentos",
                                                                   "Variación Patrimonial (Disminuciones)" = "variacion_patrimonial_disminuciones",
                                                                   "Valores de Fondo de Pensiones" = "valor_fondos_pensiones_afp"),
                                                       selected = "rentabilidad_fondo_a_afps",
                                                       options = list(style = "btn-danger"))),
                                       div(style = "height: 20px;"),
                                         div(style = "display: flex; justify-content: center; align-items: center;",
                                             highchartOutput("grafico_afps", width = "1500px", height = "500px")))),
                          
                          fluidRow(style = "height:100px;"),
                          tags$hr(),
                          fluidRow(class = "top-buffer", style = "margin-top: 20px;", 
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center; 
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "serie_pilar3",
                                                                   label = "Serie Pilar 3:", 
                                                                   choices = c("PBS Invalidez" = "serie_pilar3_pbs_invalidez",
                                                                               "PBS Vejez / PGU No Contributiva" = "serie_pilar3_pbs_vejez",
                                                                               "APS Invalidez" = "serie_pilar3_aps_invalidez",
                                                                               "APS Vejez" = "serie_pilar3_aps_vejez",
                                                                               "Artículo 9º bis" = "serie_pilar3_articulo9",
                                                                               "PGU Contributiva" = "serie_pilar3_pgu_contributiva"),
                                                                   status = "warning")),
                          fluidRow(style = "height:40px;"),
                                          highchartOutput("grafico_8")), 
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center; 
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "serie_pilar1",
                                                                   label = "Serie Pilar 1:", 
                                                                   choices = c("PBS Invalidez" = "serie_pilar1_pbs_invalidez",
                                                                               "PBS Vejez / PGU No Contributiva" = "serie_pilar1_pbs_vejez",
                                                                               "APS Invalidez" = "serie_pilar1_aps_invalidez",
                                                                               "APS Vejez" = "serie_pilar1_aps_vejez",
                                                                               "Artículo 9º bis" = "serie_pilar1_articulo9",
                                                                               "PGU Contributiva" = "serie_pilar1_pgu_contributiva"),
                                                                   status = "warning")),
                                          fluidRow(style = "height:40px;"),
                                          highchartOutput("grafico_9")),
                                   column(4, div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center; align-items: center;
                                                 text-align: center; color: white;", 
                                                 radioGroupButtons(inputId = "serie_pilar5",
                                                                   label = "Serie Pilar 5:",
                                                                   choices = c("PBS Invalidez" = "serie_pilar5_pbs_invalidez", 
                                                                               "PGU Contributiva" = "serie_pilar5_pgu_con", 
                                                                               "PGU No Contributiva" = "serie_pilar5_pgu_noncon",
                                                                               "APS Invalidez" = "serie_pilar5_aps_invalidez",
                                                                               "APS Vejez Pen. Garantizada" = "serie_pilar5_aps_vejez_pengan", 
                                                                               "APS Vejez Pen. Garan. Solicitada 2020" = "serie_pilar5_aps_vejez_pengan_sol",
                                                                               "APS Vejez Sub. Definido" = "serie_pilar5_aps_vejez_subdef",
                                                                               "Beneficio Art. 9° bis" = "serie_pilar5_articulo9"),
                                                                   status = "warning")),
                                          highchartOutput("grafico_10"))),
                          
                          fluidRow(style = "height:40px;"),
                        
                          fluidRow(div(style = "display: flex; align-items: center; justify-content: center;",
                                  div(style = "margin-right: 10px;",
                                     pickerInput(inputId = "pilar_pgu",
                                                 label = "Pilar Solidario y PGU",
                                                 choices = c(
                                                   "Beneficiarios Totales Pilar Solidario y PGU" = "serie_pilar1_totales",
                                                   "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU" = "serie_pilar2_totales",
                                                   "Beneficiarios Totales por Región del Pilar Solidario y PGU" = "serie_pilar3_totales",
                                                   "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Región" = "serie_pilar4_totales",
                                                   "Beneficiarios Totales por Institución del Pilar Solidario y PGU" = "serie_pilar5_totales", 
                                                   "Monto Total en $MM de Pesos de Beneficios Pilar Solidario y PGU por Institución" = "serie_pilar6_totales"),
                                                 selected = "serie_pilar1_totales",
                                                 options = list(style = "btn-danger"))),
                            uiOutput("info_button")),
                              div(style = "display: flex; align-items: center; justify-content: center;", 
                            uiOutput("macrozona_picker")),
                          fluidRow(style = "height: 20px;"),
                              div(style = "display: flex; align-items: center; justify-content: center;",
                                highchartOutput("grafico_pilarpgu", width = "1500px", height = "600px"))),
                          
                          fluidRow(style = "height:60px;"),

                          fluidRow(column(7, highchartOutput("grafico_genero2", height = "600px")),
                                   column(5, div(style = "flex-grow: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; height: 500px; 
                                                             background-image: url('imagen_piloto.jpg'); background-size: contain; background-position: center; 
                                                                background-repeat: no-repeat;", 
                                                 div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; text-align: center; 
                                                                color: white; transform: translateY(10%);",
                                                     pickerInput(inputId = "tipo_genero2",
                                                                 label = "Datos según género",
                                                                 choices = c("Beneficiarios PBS Vejez / PGU No Contributiva por Género" = "serie_pilar1_pbsvejez_pgunon",
                                                                             "Beneficiarios PBS Invalidez por Género" = "serie_pilar1_pbsinvalidez",
                                                                             "Beneficiarios APS Vejez" = "serie_pilar1_apsvejez",
                                                                             "Beneficiarios APS Invalidez" = "serie_pilar1_apsinvalidez",
                                                                             "Beneficiarios PGU Contributiva" = "serie_pilar1_pgucon",
                                                                             "Beneficiarios Artículo 9º bis" = "serie_pilar1_art9"),
                                                                 selected = "serie_pilar1_pbsvejez_pgunon",
                                                                 options = list(style = "btn-danger")))))),
                         fluidRow(style = "height:40px;")),
                 
                 tabPanel("Sistema de Salud", fluidRow(class = "top-buffer", style = "margin-top: 20px;",
                                                       column(width = 12, class = PARS$classcol,
                                                              valueBoxOutput("grafico_1_1", width = 3),
                                                              valueBoxOutput("grafico_2_2", width = 3),
                                                              valueBoxOutput("grafico_3_3", width = 3),
                                                              valueBoxOutput("grafico_4_4", width = 3))),

                 fluidRow(style = "height:40px;"),
                 tags$hr(),
                 fluidRow(style = "height:40px;"),     
                 
                 fluidRow(div(style = "position: relative; height: 600px; display: flex; flex-direction: column; align-items: center;",
                              div(style = "position: relative; text-align: center; color: black;",
                                  pickerInput(inputId = "datos_sistema_salud",
                                              label = "Datos Sistema Salud",
                                              choices = c("Beneficiarios Sistema de Salud" = "beneficiarios_sistema_salud",
                                                          "Cotizantes Sistema de Salud" = "cotizantes_sistema_salud",
                                                          "Beneficiarios Sistema de Salud por Región" = "beneficiarios_sistema_salud_region",
                                                          "Cotizantes Sistema de Salud por Región" = "cotizantes_sistema_salud_region",
                                                          "Beneficiarios FONASA por Rango de Edad" = "beneficiarios_fonasa_edad",
                                                          "Cotizantes FONASA por Rango de Edad" = "cotizantes_fonasa_edad",
                                                          "Beneficiarios ISAPRE por Rango de Edad" = "beneficiarios_isapre_edad",
                                                          "Cotizantes ISAPRE por Rango de Edad" = "cotizantes_isapre_edad",
                                                          "Cotizantes FONASA por Tipo de Relación Laboral" = "cotizantes_fonasa_laboral",
                                                          "Cotizantes ISAPRE por Tipo de Relación Laboral" = "cotizantes_isapre_laboral"),
                                              selected = "beneficiarios_sistema_salud",
                                              options = list(style = "btn-danger"))),
                              
                              div(style = "text-align: center; margin-bottom: 20px; min-height: 50px;",  
                                  uiOutput("cobertura_picker")),
                              div(style = "display: flex; align-items: center; justify-content: center;",
                                  highchartOutput("grafico_general_salud", width = "1500px", height = "600px")))),
                 
                 fluidRow(style = "height:200px;"),
                 tags$hr(),
                 fluidRow(style = "height:100px;"),
                 
                 fluidRow(div(style = "display: flex; justify-content: space-between;",
                              div(style = "flex: 0 0 58%;",
                                  highchartOutput("grafico_genero_salud", height = "500px")),
                              div(style = "flex: 0 0 40%; display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; 
                                                    height: 500px; background-image: url('imagen_piloto.jpg'); background-size: contain; background-position: center; 
                                                       background-repeat: no-repeat;",
                                  div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; text-align: center; 
                                                  color: white; transform: translateY(10%);",
                                      pickerInput(inputId = "tipo_genero_salud",
                                                  label = "Datos FONASA según género",
                                                  choices = c("Beneficiarios FONASA por Género" = "beneficiarios_fonasa_genero",
                                                              "Cotizantes FONASA por Género" = "cotizantes_fonasa_genero",
                                                              "Beneficiarios ISAPRE por Género" = "beneficiarios_isapre_genero",
                                                              "Cotizantes ISAPRE por Género" = "cotizantes_isapre_genero"),
                                                  selected = "beneficiarios_fonasa_genero",
                                                  options = list(style = "btn-danger")))))),
                 fluidRow(style = "height:50px;"),
                 tags$hr(),
                 fluidRow(style = "height:50px;"),
                 
                 fluidRow(
                   div(style = "display: flex; justify-content: space-between; padding-right: 20px; height: 100vh;",
                       div(style = "flex: 0 0 50%; display: flex; flex-direction: column; align-items: center;",
                           div(style = "text-align: center; color: black; margin-bottom: 20px;",
                               pickerInput(inputId = "anio", 
                                           label = "Selecciona un Año:", 
                                           choices = unique(grafico_config_treemap$datos$AÑO), 
                                           selected = unique(grafico_config_treemap$datos$AÑO)[1],
                                           options = list(style = "btn-danger"))),
                           div(style = "display: flex; justify-content: center; align-items: center;",
                               highchartOutput("treemapPlot", width = "700px", height = "1000px"))),
                       
                       div(style = "flex: 0 0 50%; display: flex; flex-direction: column; justify-content: space-between;",
                           div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; text-align: center; color: white;",
                               pickerInput(inputId = "tipo_isapre",
                                           label = "Datos beneficiarios según ISAPRE",
                                           choices = c("Beneficiarios por ISAPRE abiertas" = "beneficiarios_isapre_abiertas"),
                                           selected = "beneficiarios_isapre_abiertas",
                                           options = list(style = "btn-danger")),
                               div(style = "flex: 1; margin-bottom: 10px;",
                                   highchartOutput("grafico_isapres", width = "950px", height = "450px"))),
                           
                           div(style = "flex: 0 0 50%; display: flex; flex-direction: column; justify-content: space-between;",
                               div(style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; text-align: center; color: white;",
                                   pickerInput(inputId = "tipo_isapre2",
                                               label = "Datos beneficiarios según ISAPRE",
                                               choices = c("Beneficiarios por ISAPRE cerradas" = "beneficiarios_isapre_cerradas"),
                                               selected = "beneficiarios_isapre_cerradas",
                                               options = list(style = "btn-danger")),
                                   div(style = "flex: 1; margin-bottom: 10px;",
                                       highchartOutput("grafico_isapres2", width = "950px", height = "450px"))))))),
                 
                 fluidRow(style = "height:200px;"),
                 tags$hr(),
                 fluidRow(style = "height:100px;")),

)


server <- function(input, output) {
 
# Sección Macroeconomía ---------------------------------------------------
 
  output$grafico_poblacion <- renderHighchart({
    seleccion <- input$datos_poblacion
    config <- grafico_config_poblacion[[seleccion]]
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "spline",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "spline",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    } else {
      
      hc <- hchart(daux, "spline", hcaes(x = AÑO, y = Y)) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    }
    
    return(hc)
  })
  
  output$grafico_macroeconomico <- renderHighchart({
    seleccion <- input$datos_macroeconomia
    config <- grafico_config_macroeconomica[[seleccion]]
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "column",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "column",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    } else {
      
      hc <- hchart(daux, "column", hcaes(x = AÑO, y = Y)) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    }
    return(hc)
  })
  
  observeEvent(input$datos_gasto, {
    if (input$datos_gasto == "gasto_salud_instituciones") {
      output$instituciones_picker <- renderUI({
        pickerInput(inputId = "instituciones",
                    label = "Seleccione Institución",
                    choices = unique(grafico_config_gasto$gasto_salud_instituciones$datos$SECCIONES), 
                    selected = "Ministerio de Salud",
                    options = list(style = "btn-danger"))
      })
    } else {
      output$instituciones_picker <- renderUI({
        div(style = "min-height: 50px;")
      })
    }
  })
  
  output$grafico_gasto <- renderHighchart({
    seleccion <- input$datos_gasto
    config <- grafico_config_gasto[[seleccion]]
    
    if (is.null(config$datos)) {
      return(NULL)
    }
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (!is.null(input$instituciones)) {
      instituciones_secciones <- !is.null(config$instituciones) && config$instituciones == "SECCIONES"
      
      if (instituciones_secciones) {
        daux <- daux %>%
          filter(SECCIONES == input$instituciones)
      }
    }
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    } else {
      
      hc <- hchart(daux, "column", hcaes(x = AÑO, y = Y)) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_title(text = config$titulo) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    }
    
    return(hc)
  })

  output$grafico_ingresos <- renderHighchart({
    seleccion <- input$datos_ingresos
    config <- grafico_config_ingresos[[seleccion]]
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "column",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "column",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    } else {
      
      hc <- hchart(daux, "column", hcaes(x = AÑO, y = Y)) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    }
    
    return(hc)
  })
  
  
# Sección Pensiones -------------------------------------------------------
  
  output$grafico_1 <- renderValueBox({
    
    d <- read_rds("data/pensiones/grafico_1/afiliados_activos_totales.rds") %>% mutate(AÑO = as.numeric(AÑO)) %>% 
      select(x = AÑO, y = `NÚMERO DE AFILIADOS ACTIVOS`)
    
    lbl <- d %>% pull(y) %>% last() %>% comma()
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Total Nacional",
      color = "black",
      spark = hc,
      minititle = "Cantidad de Afiliados Activos"
    )
    
  })
  
  output$grafico_2 <- renderValueBox({

    d <- read_rds("data/pensiones/grafico_2/cotizantes_mes_totales.rds") %>% mutate(AÑO = as.numeric(AÑO)) %>% 
      select(x = AÑO, y = `NÚMERO DE COTIZANTES ACTIVOS`)
    
    lbl <- d %>% pull(y) %>% last() %>% comma()
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Total Nacional",
      color = "black",
      spark = hc,
      minititle = "Cantidad de Cotizantes Activos"
    )
    
  })
  
  output$grafico_3 <- renderValueBox({

    d <- read_rds("data/pensiones/grafico_3/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total" & GENERO == "Total") %>%  
      select(x = AÑO, y = INGRESO_MEDIO)

    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("$", .)
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "$") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Ingreso Promedio",
      color = "black",
      spark = hc,
      minititle = "Ingreso Promedio"
    )
    
  })
  
  output$grafico_4 <- renderValueBox({
    
    d <- read_rds("data/pensiones/grafico_4/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total" & GENERO == "Total") %>%  
      select(x = AÑO, y = INGRESO_MEDIANO)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("$", .) 
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "$") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Ingreso Mediano",
      color = "black",
      spark = hc,
      minititle = "Ingreso Mediano"
    )
    
  })
  
  output$grafico_general <- renderHighchart({
    seleccion <- input$datos_sistema_afp
    config <- grafico_config_general[[seleccion]]
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    } else {
      
      hc <- hchart(daux, "line", hcaes(x = AÑO, y = Y)) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    }
    
    return(hc)
  })
  
  output$grafico_genero <- renderHighchart({
    seleccion <- input$tipo_genero
    config <- grafico_config_genero[[seleccion]]
  
  cols <- c("#49006a","#E95420", "#F0AD4E")
  cols <- substr(cols, 0, 7)
  
  daux <- config$datos %>% 
    mutate(X = .data[[config$columna_x]], 
           Y = as.double(.data[[config$columna_y]]))
  
  hc <- hchart(
    daux %>% select(x = X, y = Y, group = .data[[config$group]]), "streamgraph",
    hcaes(x, y, group = group)) %>%
    hc_colors(cols) %>%
    hc_title(text = config$titulo) %>%
    hc_subtitle(
      text = config$subtitulo,
      align = "center",
      style = list(color = "#2b908f", fontWeight = "bold")
    ) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_xAxis(title = list(text = "Año"),
      crosshair = list(label = list(enabled = TRUE))) %>%
    hc_tooltip(table = TRUE, sort = TRUE) %>%
    hc_plotOptions(
      series = list(
        borderWidth = 0,
        marker = list(states = list(hover = list(enabled = FALSE)))
      )
    )
  
  return(hc)
  
  })
  
  output$grafico_general2 <- renderHighchart({
    seleccion <- input$datos_sistema_afp
    config <- grafico_config_general[[seleccion]]
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      hc <- hchart(
        group_var, "line",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    } else {
      
      hc <- hchart(daux, "line", hcaes(x = AÑO, y = Y)) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    }
    
    return(hc)
  })
  
  output$grafico_5 <- renderHighchart({
    
    seleccion <- input$evolucion_inversion
    config <- grafico_config_5[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = as.numeric(.data[[config$columna_x]]), 
             Y = as.double(round(.data[[config$columna_y]], digits = 2)))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y), "column",
      hcaes(x, y)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE, valueSuffix = "%") %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(color = "#e95420",
          marker = list(enabled = FALSE)
        )
      ) 
    return(hc)
  })
  
  output$grafico_6 <- renderHighchart({
    
    seleccion <- input$variacion_patrimonial
    config <- grafico_config_6[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = as.numeric(.data[[config$columna_x]]), 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y), "column",
      hcaes(x, y)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = "$") %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(color = "#e95420",
          marker = list(enabled = FALSE)
        )
      ) 
    return(hc)
  })
  
  output$grafico_7 <- renderHighchart({
  
      seleccion <- input$Tipo_de_fondos
      config <- grafico_config_7[[seleccion]]
      
      daux <- config$datos %>% 
        mutate(AÑO = as.numeric(.data[[config$columna_x]]), 
               Y = as.double(.data[[config$columna_y]]))
      
      hc <- hchart(
        daux %>% select(x = AÑO, y = Y), "column",
        hcaes(x, y)
      ) %>% 
        hc_title(text = config$titulo) %>% 
        hc_tooltip(table = TRUE, sort = TRUE, valueSuffix = "%") %>% 
        hc_yAxis(
          title = list(text = config$y_axis_title)
        ) %>% 
        hc_xAxis(
          title = "Año"
        ) %>% 
        hc_plotOptions(
          series = list(color = "#e95420",
            marker = list(enabled = FALSE)
          )
        ) 
      return(hc)
    })
  
  output$grafico_afps <- renderHighchart({
    seleccion <- input$tipo_afps
    config <- grafico_config_afps[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y, group = .data[[config$group]]), "line",
      hcaes(x, y, group = group, colorByGroup = TRUE)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      ) %>% 
      hc_legend(layout = "vertical", align = "right", verticalAlign = "middle")
    
    return(hc)
  })  

  output$grafico_8 <- renderHighchart({
    
    seleccion <- input$serie_pilar3
    config <- grafico_config_8[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y, group = .data[[config$group]]), "line",
      hcaes(x, y, group = group, colorByGroup = TRUE)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Región"
      ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      ) %>%  
      hc_legend(enabled = FALSE)
    
    return(hc)
  })
  
  output$grafico_9 <- renderHighchart({
    
    seleccion <- input$serie_pilar1
    config <- grafico_config_9[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y), "column",
      hcaes(x, y)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(color = "#e95420",
                      marker = list(enabled = FALSE)
        )
      ) 
    return(hc)
  })
  
  output$grafico_10 <- renderHighchart({
    
    seleccion <- input$serie_pilar5
    config <- grafico_config_10[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y, group = .data[[config$group]]), "line",
      hcaes(x, y, group = group, colorByGroup = TRUE)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Región"
      ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      ) %>%  
      hc_legend(enabled = FALSE)
    
    return(hc)
  })
  
  output$grafico_pilarpgu <- renderHighchart({
    seleccion <- input$pilar_pgu
    config <- grafico_config_pilarpgu[[seleccion]]
    
    if (is.null(config$datos)) {
      return(NULL)
    }
    
    daux <- config$datos %>%
      mutate(AÑO = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    if (!is.null(input$macrozona)) {
      macrozonas_reg <- !is.null(config$macrozonas) && config$macrozonas == "MACROZONA"
      
      if (macrozonas_reg) {
        daux <- daux %>%
          filter(MACROZONA == input$macrozona)
      }
    }
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "spline",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = all_of(config$group))
      
      hc <- hchart(
        group_var, "spline",
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    } else {
     
      hc <- hchart(daux, "spline", hcaes(x = AÑO, y = Y)) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_title(text = config$titulo) %>%
        hc_legend(layout = "vertical", align = "right", verticalAlign = "middle", width = 250)
    }
    
    return(hc)
  })
  
  observeEvent(input$pilar_pgu, {
    if (input$pilar_pgu == "serie_pilar3_totales" || input$pilar_pgu == "serie_pilar4_totales") {
      output$macrozona_picker <- renderUI({
        pickerInput(inputId = "macrozona",
                    label = "Seleccione Macrozonas",
                    choices = unique(grafico_config_pilarpgu$serie_pilar3_totales$datos$MACROZONA), 
                    selected = "Norte",
                    options = list(style = "btn-danger"))
      })
      output$info_button <- renderUI({ NULL }) 
    } else if (input$pilar_pgu == "serie_pilar6_totales") {
      output$macrozona_picker <- renderUI({ NULL })
      
      output$info_button <- renderUI({
        dropdownButtonp(
          tags$h6("A partir de febrero 2022 se comienzan a reportar en esta serie los beneficios no contributivos (PGU No Contributiva y PBS Invalidez)"),
          icon = icon("info"),
          tooltip = tooltipOptions(title = "Información adicional")
        )
      })
    } else {
      output$macrozona_picker <- renderUI({ NULL })
      output$info_button <- renderUI({ NULL })
    }
  })
  
  output$grafico_genero2 <- renderHighchart({
    seleccion <- input$tipo_genero2
    config <- grafico_config_genero2[[seleccion]]
    
    cols <- c("#F0AD4E", "#E95420", "#49006a")
    cols <- substr(cols, 0, 7)
    
    daux <- config$datos %>% 
      mutate(X = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = X, y = Y, group = .data[[config$group]]), "streamgraph",
      hcaes(x, y, group = group)) %>%
      hc_colors(cols) %>%
      hc_title(text = config$titulo) %>%
      hc_subtitle(
        text = config$subtitulo,
        align = "center",
        style = list(color = "#2b908f", fontWeight = "bold")
      ) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_xAxis(title = list(text = "Año"),
               crosshair = list(label = list(enabled = TRUE))) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_plotOptions(
        series = list(
          borderWidth = 0,
          marker = list(states = list(hover = list(enabled = FALSE)))
        )
      )
    
    return(hc)
    
  })

# Sección Salud -----------------------------------------------------------

  output$grafico_1_1 <- renderValueBox({
    
    d <- read_rds("data/salud/grafico_1/beneficiarios_fonasa.rds") %>% 
      select(x = AÑO, y = BENEFICIARIOS)
    
    lbl <- d %>% pull(y) %>% last() %>% comma()
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Total Nacional",
      color = "black",
      spark = hc,
      minititle = "Cantidad de Beneficiarios FONASA"
    )
    
  })
  
  output$grafico_2_2 <- renderValueBox({
    
    d <- read_rds("data/salud/grafico_2/beneficiarios_isapres.rds") %>% 
      select(x = AÑO, y = BENEFICIARIOS)
    
    lbl <- d %>% pull(y) %>% last() %>% comma()
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Total Nacional",
      color = "black",
      spark = hc,
      minititle = "Cantidad de Beneficiarios ISAPRE"
    )
    
  })
  
  output$grafico_3_3 <- renderValueBox({
    
    d <- read_rds("data/salud/grafico_3/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total" & GENERO == "Total") %>% 
      select(x = AÑO, y = INGRESO_MEDIO)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("$", .)
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "$") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Ingreso Promedio",
      color = "black",
      spark = hc,
      minititle = "Ingreso Promedio"
    )
    
  })
  
  output$grafico_4_4 <- renderValueBox({
    
    d <- read_rds("data/salud/grafico_4/ingreso_medio_mediano.rds") %>% filter(REGIÓN == "Total" & GENERO == "Total") %>% 
      select(x = AÑO, y = INGRESO_MEDIANO)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("$", .) 
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "$") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Ingreso Mediano",
      color = "black",
      spark = hc,
      minititle = "Ingreso Mediano"
    )
    
  })
  
  observeEvent(input$datos_sistema_salud, {
    if (input$datos_sistema_salud == "beneficiarios_sistema_salud_region" || input$datos_sistema_salud == "cotizantes_sistema_salud_region" ) {
      output$cobertura_picker <- renderUI({
        pickerInput(inputId = "cobertura",
                    label = "Seleccione Macrozona",
                    choices = unique(grafico_config_general_salud$beneficiarios_sistema_salud_region$datos$MACROZONA), 
                    selected = "Centro Norte",
                    options = list(style = "btn-danger"))
      })
    } else {
      output$cobertura_picker <- renderUI({ NULL })
    }
  })
  
  output$grafico_general_salud <- renderHighchart({
    seleccion <- input$datos_sistema_salud
    config <- grafico_config_general_salud[[seleccion]]
    
    if (is.null(config$datos)) {
      return(NULL)
    }
    
    daux <- config$datos %>%
      mutate(AÑO = as.integer(.data[[config$columna_x]]), 
             Y = as.double(.data[[config$columna_y]]))
    
    if (!is.null(input$cobertura)) {
      cobertura_reg <- !is.null(config$macrozona) && config$macrozona == "MACROZONA"
      
      if (cobertura_reg) {
        daux <- daux %>%
          filter(MACROZONA == input$cobertura)
      }
    }
    
    if (is.character(config$group) && length(config$group) > 1) {
      daux <- daux %>%
        unite(group_combined, all_of(config$group), sep = " - ", remove = TRUE)
      
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = group_combined)
      
      hc <- hchart(
        group_var, "streamgraph",
        hcaes(x, y, group = interaction(group, drop = TRUE), colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_subtitle(text = config$subtitulo, align = "center", style = list(color = "#2b908f", fontWeight = "bold")) %>% 
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
      
    } else if (!is.null(config$group) && config$group %in% names(daux)) {
      group_var <- daux %>%
        select(x = AÑO, y = Y, group = .data[[config$group]])
      
      num_categorias <- n_distinct(group_var$group)

      tipo_grafico <- if (num_categorias > 6) "spline" else "column"
      
      hc <- hchart(
        group_var, tipo_grafico,
        hcaes(x, y, group = group, colorByGroup = TRUE)
      ) %>%
        hc_title(text = config$titulo) %>%
        hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>%
        hc_yAxis(title = list(text = config$y_axis_title)) %>%
        hc_xAxis(title = "Año") %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom")
    }
    
    return(hc)
  })
  
  output$grafico_genero_salud <- renderHighchart({
    seleccion <- input$tipo_genero_salud
    config <- grafico_config_genero_salud[[seleccion]]
    
    cols <- c("#E95420", "#49006a", "#F0AD4E")
    cols <- substr(cols, 0, 7)
    
    daux <- config$datos %>% 
      mutate(X = .data[[config$columna_x]], 
             Y = .data[[config$columna_y]])
    
    hc <- hchart(
      daux %>% select(x = X, y = Y, group = .data[[config$group]]), "streamgraph",
      hcaes(x, y, group = group)) %>%
      hc_colors(cols) %>%
      hc_title(text = config$titulo) %>%
      hc_subtitle(
        text = config$subtitulo,
        align = "center",
        style = list(color = "#2b908f", fontWeight = "bold")
      ) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_xAxis(title = list(text = "Año"),
               crosshair = list(label = list(enabled = TRUE))) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_plotOptions(
        series = list(
          borderWidth = 0,
          marker = list(states = list(hover = list(enabled = FALSE)))
        )
      )
    
    return(hc)
    
  })
  
  output$treemapPlot <- renderHighchart({
    
    config <- grafico_config_treemap
    
    data_filtrada <- config$datos %>% filter(AÑO == input$anio)
    
    data_treemap <- data_filtrada %>%
      mutate(X = paste(TRAMOS, REGIÓN, sep = "/"))  
    
    hchart(
      data_treemap, "treemap",
      hcaes(x = X, value = CANTIDAD, color = CANTIDAD)
    ) %>%
      hc_title(text = paste(config$titulo, input$anio)) %>%
      hc_colorAxis(minColor = "#F0AD4E", maxColor = "#E95420") %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.value}")
    
  })
  
  output$grafico_isapres <- renderHighchart({
    seleccion <- input$tipo_isapre
    config <- grafico_config_isapre[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y, group = .data[[config$group]]), "spline",
      hcaes(x, y, group = group, colorByGroup = TRUE)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      ) %>% 
      hc_legend(layout = "vertical", align = "right", verticalAlign = "middle")
    
    return(hc)
  })  
  
  output$grafico_isapres2 <- renderHighchart({
    seleccion <- input$tipo_isapre2
    config <- grafico_config_isapre2[[seleccion]]
    
    daux <- config$datos %>% 
      mutate(AÑO = .data[[config$columna_x]], 
             Y = as.double(.data[[config$columna_y]]))
    
    hc <- hchart(
      daux %>% select(x = AÑO, y = Y, group = .data[[config$group]]), "spline",
      hcaes(x, y, group = group, colorByGroup = TRUE)
    ) %>% 
      hc_title(text = config$titulo) %>% 
      hc_tooltip(table = TRUE, sort = TRUE, valuePrefix = config$prefix, valueSuffix = config$suffix) %>% 
      hc_yAxis(
        title = list(text = config$y_axis_title)
      ) %>% 
      hc_xAxis(
        title = "Año"
      ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
        )
      ) %>% 
      hc_legend(layout = "vertical", align = "right", verticalAlign = "middle")
    
    return(hc)
  })  
  
} 

shinyApp(ui = ui, server = server)
