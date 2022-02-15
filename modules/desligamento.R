
 #  Modules do Aplicativo Shiny People Analytics - Desligamentos

 desligamento_ui <- function(id){
   ns <- NS(id)
   
   tabItem(
     tabName = "desligamento",
     fluidRow(
       h1("Desligamentos - People Analytics", align = "center")
     ),
     fluidRow(
       column(width = 4, infoBoxOutput(width = 12, ns("demissao"))),
       column(width = 4, infoBoxOutput(width = 12, ns("ma_contra"))),
       column(width = 4, infoBoxOutput(width = 12, ns("abs")))
     ),
     fluidRow(
       column(width = 6, highchartOutput(ns("desl_tempo"))),
       column(width = 6, highchartOutput(ns("faixa_etaria_desl")))
     ),
     fluidRow(
        column(width = 6, highchartOutput(ns("genero_desl"))),
        column(width = 6, highchartOutput(ns("escolaridade_desl")))
     ),
     fluidRow(
        column(width = 6, highchartOutput(ns("motivo"))),
       column(width = 6, dataTableOutput(ns("cargo_desl")))
     )
   )
 }
 
 
 desligamento_server <- function(input, output, session, dados){
   
   output$demissao <- renderInfoBox({
     demissoes <- dados  %>% 
       na.omit(Data_Afastamento) %>% 
       count(Data_Afastamento) %>% 
       summarise(sum(n)) %>%
       pull() 
     
     infoBox(
       "Desligamentos", demissoes, icon = icon("user-slash"))
   })

   
   output$ma_contra <- renderInfoBox({
     contratacoes <- dados %>% 
       count(Cod_Funcionario) %>% 
       summarise(sum(n)) %>% pull()
     
     ma_contratacoes <- sum(na.omit(dados$Data_Afastamento - dados$Data_Admissao) < 60)
     ma_contratacoes_percentil <- round((ma_contratacoes/contratacoes)*100, 2)
     
     infoBox(
       "Má Contratações", paste0(ma_contratacoes_percentil,"%"), icon = icon("thumbs-down"))
   })
   
   output$abs <- renderInfoBox({
     horas_normais <- financeiro %>% 
       filter(Categoria == "Normais") %>% 
       summarise(soma = sum(Valor))
     
     absenteismo <- financeiro %>% 
       filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
       summarise(soma = sum(Valor))
     absenteismo_percentil <- round((absenteismo/horas_normais)*100,2)
     
     infoBox(
       "Abseteísmo", paste0(absenteismo_percentil,"%"),icon = icon("user-times"))
   })
   
   output$desl_tempo <- renderHighchart({
     
     dados %>% 
         group_by(Ano_Desl) %>% 
         count(Data_Afastamento) %>% 
         summarise(total = sum(n))%>% 
         hchart("line", hcaes(x = Ano_Desl, y = total), name = "Total") %>% 
         hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
         hc_xAxis(title =list(text = "")) %>% 
         hc_title(text = "Desligamentos ao longo do tempo", align = "left") %>% 
         hc_colors(colors = "#bd3d17") %>% 
         hc_yAxis(gridLineWidth = 0) %>% 
         hc_plotOptions(line = list(
            dataLabels = list(enabled = TRUE)))
   })
   
   output$faixa_etaria_desl <- renderHighchart({
     
     dados %>% 
       filter(Situacao == "Demitido") %>% 
       group_by(Faixa_Etaria) %>% 
       count(Faixa_Etaria) %>% na.omit() %>% 
       hchart("column", hcaes(x = Faixa_Etaria, y = n), name ="Total") %>% 
       hc_yAxis(title = list(text = "")) %>% 
       hc_xAxis(title = list(text = "")) %>% 
       hc_title(text = "Faixa Etária", align = "left") %>% 
       hc_colors(colors =  "#bd3d17") %>% 
       hc_yAxis(gridLineWidth = 0) %>% 
       hc_plotOptions(
         column = list(
           dataLabels = list(
             enabled = TRUE)))
   })
   
   output$genero_desl <- renderHighchart({
     
     dados %>% 
       filter(Situacao == "Demitido") %>% 
       count(Sexo) %>%
         mutate(porc = round((n/sum(n))*100, 2)) %>% 
         hchart("pie", hcaes(x = Sexo, y = porc),name = "Porcentagem") %>% 
         hc_title(text = "Gênero", align = "left") %>% 
         hc_colors(colors = c("#bd3d17", "#ff815c")) %>% 
         hc_plotOptions(series = list(showInLegend = TRUE),
                        pie = list(size = 130,
                                   dataLabels = list(enabled = T,
                                                     format = "{point.porc:.1f}%",
                                                     distance = -25))) %>% 
         hc_legend(align = "center-left",
                   verticalAlign = "top",
                   layout = "horizontal"
         )
     
   })
   
   output$escolaridade_desl <- renderHighchart({
     
     dados %>% 
         filter(Situacao == "Demitido") %>% 
         group_by(Escolaridade) %>% 
         count(Escolaridade) %>% 
         arrange(desc(n)) %>% 
         hchart("bar", hcaes(x = Escolaridade, y = n), name = "Total") %>% 
         hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
         hc_xAxis(title =list(text = "")) %>% 
         hc_title(text = "Faixa Étaria", align = "left") %>% 
         hc_colors(colors = "#bd3d17") %>% 
         hc_yAxis(gridLineWidth = 0) %>% 
         hc_plotOptions(bar = list(
            dataLabels = list(enabled = TRUE))) 
   })
   
   output$cargo_desl <- renderDataTable({
     
     dados %>% 
         filter(Situacao == "Demitido") %>% 
         group_by(Cargo) %>% 
         summarise(`Qtd Contratações`=  n()) %>% 
         arrange(desc(`Qtd Contratações`)) %>% 
         datatable(options = list(pageLength = 7,  
                                  scrollY = TRUE,
                                  autoWidth = TRUE,
                                  dom = "Bfrtip",
                                  align =c("l","c","c")))
   })
   
   output$motivo <- renderHighchart({
     
     dados %>% 
         filter(Situacao == "Demitido") %>% 
         group_by(Causa_Afastamento) %>% 
         count(Causa_Afastamento) %>% 
         arrange(desc(n)) %>% 
         hchart("bar", hcaes(x = Causa_Afastamento, y = n), name ="Total") %>% 
         hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
         hc_xAxis(title =list(text = "")) %>% 
         hc_title(text = "Motivo do Desligamento", align = "left") %>% 
         hc_colors(colors = "#bd3d17") %>% 
         hc_yAxis(gridLineWidth = 0) %>% 
         hc_plotOptions(bar = list(
            dataLabels = list(enabled = TRUE))) 
     
   })
 }