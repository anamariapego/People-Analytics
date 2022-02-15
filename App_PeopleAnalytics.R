
 # Aplicativo Shiny --------
 
 # Caminho
 setwd("C:\\Users\\JDC\\AnaPinheiro\\AnaliseDadosRH")

 library(shinydashboard)
 library(shinydashboardPlus)
 library(shinyjs)
 library(DT) 
 library(highcharter)
 
 # Carregando os modules
 source("modules/contratacao.R",encoding = "UTF-8")
 source("modules/desligamento.R",encoding = "UTF-8")
 source("modules/colaboradores.R",encoding = "UTF-8")
 source("modules/absenteismo.R",encoding = "UTF-8")
 
 # Carregando o script com os dados tratado
 source("Limpeza_Transformacao_Dados.R", encoding = "UTF-8")
 
 # Construindo UI
 ui <- dashboardPage(
     dashboardHeader(
         tags$li(
             class = "dropdown",
             p("Desenvolvido por:"),
             tags$img(src="img/log.png",height="30",width="100")
         )
     ),
     dashboardSidebar(
         collapsed = TRUE,
         sidebarMenu(
             id = "tabs",
             menuItem("Contratações", tabName = "contratacao", icon = icon("address-card")),
             menuItem("Colaboradores Ativos", tabName = "ativos", icon = icon("users")),
             menuItem("Desligamentos", tabName = "desligamento", icon = icon("power-off")),
             menuItem("Absenteísmo", tabName = "absenteismo", icon = icon ("user-times"))
         )
     ),
     dashboardBody(
         includeCSS("www/css/style.css"),
         useShinyjs(),
         tabItems(
             contratacao_ui(id = "contratacao"),
             desligamento_ui(id = "desligamento"),
             ativos_ui(id = "ativos"),
             absenteismo_ui(id = "absenteismo")
         )
     )
 )
 
 
 # Construindo SERVER
 
 server <- function(input, output, session){
     
     callModule(
         module = contratacao_server,
         id = "contratacao",
         dados = funcionarios
     )
     
     callModule(
         module = desligamento_server,
         id = "desligamento",
         dados = funcionarios
     )
     
     callModule(
             module = ativos_server,
             id = "ativos",
             dados = funcionarios
     )

     callModule(
             module = absenteismo_server,
             id = "absenteismo",
             dados = financeiro
     )
 }
 
 shinyApp(ui = ui, server = server)
 