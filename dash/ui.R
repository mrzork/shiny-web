mLoad <- function(...) {
  sapply(sapply(match.call(), as.character)[-1], require, character.only = TRUE)
}

suppressMessages(mLoad(shiny,shinydashboard,dplyr,xts,tidyr,DT,rpivotTable,googleVis,dygraphs,lubridate,ggplot2,ggvis))

options(shiny.maxRequestSize=30*1024^2)




dbHeader <- dashboardHeader(title = "Dashboard",titleWidth = 200,dropdownMenuOutput("messageMenu2"),dropdownMenuOutput("messageMenu"))
dbHeader$children[[2]]$children <- tags$a(href='http://www.celsia.com/',tags$a("Dashboard"))


###comment
dashboardPage(skin = "yellow",dbHeader,
              # dashboardHeader(title = "Dashboard TIC",titleWidth = 200,dropdownMenuOutput("messageMenu2"),dropdownMenuOutput("messageMenu")),
              dashboardSidebar(width = 200,sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Gerencia AO", tabName = "gerenciaAO", icon = icon("check")),
                selectInput("B1","Usuarios",c("Usuarios Internos","Usuarios Finales","Internos + Finales"),selected = "Usuarios Finales"),
                selectInput("B2","Tipo de Caso",c("Todos","Requerimientos","Incidentes"),selected = "Todos"),
                verbatimTextOutput("actual")
              )),
              dashboardBody(tags$head(tags$style(
                type = 'text/css',
                '#pivotew{ overflow-x: scroll; }'
              )),tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                
              ),
              tabItems(
                tabItem(tabName = "dashboard",
                        
                        fluidRow(
                          tabsetPanel(
                            tabPanel("Gerencia AO",
                                     box(
                                       title = "Estado de la Gerencia AO", width = 12,status = "primary", solidHeader = FALSE,
                                       collapsible = TRUE,
                                       valueBoxOutput("progressICGT"),
                                       valueBoxOutput("progressIVSGT"),
                                       valueBoxOutput("progressIESGT"),
                                       valueBoxOutput("ActivosGT"),
                                       valueBoxOutput("CerradosGT"),
                                       valueBoxOutput("IncumplidosGT"),
                                       hr(),
                                       box(
                                         title = "+Info",dygraphOutput("tsGT"),dygraphOutput("trend2GT"),dygraphOutput("trendGT"), width = 12,collapsible = TRUE, collapsed = FALSE),
                                       tags$head(tags$style( type = 'text/css',  '#pivoteCI{ overflow-x: scroll;overflow-y: scroll; }')),
                                       box(title = "Incidentes y CI's Involucrados", width = 12,h1(""),
                                           rpivotTableOutput("pivoteCI",width = "1000px", height = "530px"),collapsible = TRUE, collapsed = FALSE)
                                     )
                            ),
                            tabPanel("Areas",
                                     box(
                                       title = "Infraestructura", width = 12,status = "primary",solidHeader = FALSE,
                                       collapsible = TRUE,
                                       valueBoxOutput("progressICII"),
                                       valueBoxOutput("progressIVSII"),
                                       valueBoxOutput("progressIESII"),
                                       valueBoxOutput("ActivosII"),
                                       valueBoxOutput("CerradosII"),
                                       valueBoxOutput("IncumplidosII"),
                                       box(title = "+Info", width = 12,dygraphOutput("tsII"),collapsible = TRUE, collapsed = FALSE)
                                     ),
                                     box(
                                       title = "Sistemas de Informacion",width = 12, status = "primary",solidHeader = FALSE,
                                       collapsible = TRUE,
                                       valueBoxOutput("progressICSSII"),
                                       valueBoxOutput("progressIVSSSII"),
                                       valueBoxOutput("progressIESSSII"),
                                       valueBoxOutput("ActivosSSII"),
                                       valueBoxOutput("CerradosSSII"),
                                       valueBoxOutput("IncumplidosSSII"),
                                       box(title = "+Info", width = 12,dygraphOutput("tsSSII"),collapsible = TRUE, collapsed = FALSE)
                                     ),
                                     box(
                                       title = "Telecomunicaciones",width = 12, status = "primary",solidHeader = FALSE,
                                       collapsible = TRUE,
                                       valueBoxOutput("progressICSTELCO"),
                                       valueBoxOutput("progressIVSTELCO"),
                                       valueBoxOutput("progressIESTELCO"),
                                       valueBoxOutput("ActivosTELCO"),
                                       valueBoxOutput("CerradosTELCO"),
                                       valueBoxOutput("IncumplidosTELCO"),
                                       box(title = "+Info", width = 12,dygraphOutput("tsTELCO"),collapsible = TRUE, collapsed = FALSE)
                                     ),
                                     box(
                                       title = "Gestion del Servicio",width = 12, status = "primary",solidHeader = FALSE,
                                       collapsible = TRUE,
                                       valueBoxOutput("progressICSGDS"),
                                       valueBoxOutput("progressIVSGDS"),
                                       valueBoxOutput("progressIESGDS"),
                                       valueBoxOutput("ActivosGDS"),
                                       valueBoxOutput("CerradosGDS"),
                                       valueBoxOutput("IncumplidosGDS"),
                                       box(title = "+Info", width = 12,dygraphOutput("tsGDS"),collapsible = TRUE, collapsed = FALSE)
                                     )),
                            tabPanel("Mapas",
                                     tabsetPanel(tabPanel("Incidentes", hr() ,htmlOutput("view")),
                            tabPanel("Requerimientos",hr(), htmlOutput("view2")),
                            tabPanel("Incumplimientos",hr(),
                                     h3("Incidentes"), htmlOutput("view3"),
                                     hr(),h3("Requerimientos"), htmlOutput("view4")),
                            tabPanel("Casos a Gestionar",hr(), htmlOutput("view5")),
                            tabPanel("Casos Especialista",h4("Casos que Deben Cerrarse en Mes Actual"),
                                     selectInput("AREA", "AREA:", c("Todas"="Todas","Infraestructura Informatica"="II","Sistemas de Informacion"="SSII","Telecomunicaciones"="TELCO","Gestion del Servicio"="GDS")),
                                     hr(),
                                     div(style="display:inline-block",downloadButton('foo', 'Download Imagen'), style="float:right")
                                     ,
                                     div(style='height:900px',
                                         plotOutput('HeatE',width = "75%",height = "100%")
                                     ),hr(),h3("Detalle de Casos"),hr(),
                                     dataTableOutput("Especi")
                                     ),
                            tabPanel("Exploratorio de Casos",
                            plotOutput("scatter1", brush = brushOpts(id = "brush")),
                            plotOutput("scatter2"),hr(),h3("Detalle de Casos"),
                            dataTableOutput("bruselect")
                            ))
                          ),
                          tabPanel("Evolucion_Mes",selectInput("Detalle", "Area a Explorar:",
                                   choices = c("Gerencia Total", "Infraestructura Informatica", "Sistemas de Informacion","Telecomunicaciones","Gestión del Servicio","Arquitectura Organizacional")),
                                   ggvisOutput("evoluc"),sliderInput("sliderEvo", "Limites:",
                                                                    min = 55, max = 105, value = c(65,105))),
                          tabPanel("Indicadores Detallados",      
                                   selectInput("Detalle2", "Area a Explorar:",
                                               choices = c("Gerencia Total", "Infraestructura Informatica", "Sistemas de Informacion","Telecomunicaciones","Gestión del Servicio")),
                                   selectInput("AreaPS", "Nivel de Detalle:", 
                                               choices = c("Indicador Gerencia"="GT",
                                                           "Area"="Pr_CI_A",
                                                           "Area - Grupo"="Pr_CI_AG",
                                                           "Area - Grupo - Subgrupo"= "Pr_CI_AG_SG",
                                                           # "Area - Grupo - Categoria"="Pr_CI_AGJ",
                                                           # "Area - Categoria"="Pr_CI_AJ"
                                                           "Especialista"="Pr_CI_E","Especialista - Categoria"="Pr_CI_EJ",
                                                           "Servicio - Categoria"="Pr_CI_SJ"
                                                           # "Urgentes"="Urgentes",
                                                           # "Incumplidos"="Incumplidos",
                                                           # "# Casos Gestión del Mes"="Gestion",
                                                           # "Razon Casos del Mes"="razon"
                                               ))
                                   ,hr() ,DT::dataTableOutput('contents')),
                          
                           ####jmc
                          tabPanel("Inc&Req",
                                     
                                     tabsetPanel(tabPanel("Pendiente x Cliente", hr() ,selectInput("AREAP", "AREA:", c("Todas"="Todas","Infraestructura Informatica"="II","Sistemas de Informacion"="SSII","Telecomunicaciones"="TELCO","Gestion del Servicio"="GDS")),htmlOutput("viewJMG")),
                                                 tabPanel("Pendiente Reincidencia", hr() , selectInput("AREAP2", "AREA:", c("Todas"="Todas","Infraestructura Informatica"="II","Sistemas de Informacion"="SSII","Telecomunicaciones"="TELCO","Gestion del Servicio"="GDS")),DT::dataTableOutput("JMGDT"))
                                                 )
                          )
                          ####
                          ))
                ),
                tabItem(tabName = "gerenciaAO",
                        fluidRow(
                          tabsetPanel(
                            tabPanel("Explora los Casos",tags$head(tags$style( type = 'text/css',  '#pivote{ overflow-x: scroll;overflow-y: scroll; }')),hr(),rpivotTableOutput("pivote",width = "1000px", height = "530px")),
                            tabPanel("Casos en Gestion",DT::dataTableOutput('GT')),
                            tabPanel("Casos Activos x Gestionar",DT::dataTableOutput('Gestion')),
                            tabPanel("Probabilidad",DT::dataTableOutput('Probabi')),
                            tabPanel("Admin", 
                                     box(title = "Upload Info",fileInput('file1', 'Choose file to upload',
                                                       accept = c(
                                                         'text/csv',
                                                         'text/comma-separated-values',
                                                         'text/tab-separated-values',
                                                         'text/plain',
                                                         '.csv',
                                                         '.tsv'
                                                       )),
                                           passwordInput("passwd",label = "Enter Passkey"),
                                           actionButton("do", "Upload"),collapsible = TRUE))
                          )
                        )
                )
                
              )
              )
)


