
 #  Modules do Aplicativo Shiny People Analytics - Contratações
 
 contratacao_ui <- function(id){
   ns <- NS(id)
   
   tabItem(
     tabName = "contratacao",
     class = "active",
     fluidRow(
         h1("Contratações - People Analytics", align = "center")
     ),
     fluidRow(
         column(width = 4, infoBoxOutput(width = 12, ns("contratacao"))),
         column(width = 4, infoBoxOutput(width = 12, ns("salario"))),
         column(width = 4, infoBoxOutput(width = 12, ns("func_ativo")))
     ),
     fluidRow(
         column(width = 5, highchartOutput(ns("contra_tempo"))),
         column(width = 4, highchartOutput(ns("faixa_etaria"))),
         column(width = 3, highchartOutput(ns("genero")))
     ),
     fluidRow(
         column(width = 6, highchartOutput(ns("escolaridade"))),
         column(width = 6, dataTableOutput(ns("cargo")))
     )
   )
 }
 
 contratacao_server <- function(input, output, session, dados){
     
     output$contratacao <- renderInfoBox({
         contratacoes <- dados %>% 
             count(Cod_Funcionario) %>% 
             summarise(sum(n)) %>% pull()
         
         infoBox(
             "Contratações", contratacoes, icon = icon("users"))
     })

     output$func_ativo <- renderInfoBox({
         
         func_ativo <- dados %>%
             filter(Situacao != "Demitido") %>% 
             count(Situacao) %>% 
             summarise(sum(n)) %>%
             pull() 
         
         infoBox(
             "Colaboradores Ativos", func_ativo, icon = icon("user-check"))
     }) 
     
     output$salario <- renderInfoBox({
        
         massa_sal <- dados %>%
            filter(Situacao != "Demitido") %>% 
            summarise(MS = sum(Valor_Salario)) %>%
            pull() %>%
            prettyNum(big.mark = ".", decimal.mark = ",", scientific = FALSE) %>% 
            paste0("R$ ", .)
         
         infoBox(
             "Massa Salarial", massa_sal, icon = icon("dollar-sign"))
     })
     
    output$contra_tempo <- renderHighchart({
         
         dados %>% 
           group_by(Ano_Adm) %>% 
           count(Cod_Funcionario) %>% 
           summarise(total = sum(n)) %>% 
           hchart("area", hcaes(x = Ano_Adm, y = total), name = "Total") %>% 
           hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
           hc_xAxis(title =list(text = "")) %>% 
           hc_title(text = "Contratações ao longo do tempo", align = "left") %>% 
           hc_colors(colors = "#129e05") %>% 
           hc_yAxis(gridLineWidth = 0) %>% 
           hc_plotOptions(area = list(
              dataLabels = list(enabled = TRUE)))
     })
     
     output$faixa_etaria <- renderHighchart({
         
         dados %>% 
           group_by(Faixa_Etaria) %>% 
           count(Faixa_Etaria) %>% na.omit() %>% 
           hchart("column", hcaes(x = Faixa_Etaria, y = n), name = "Total") %>% 
           hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
           hc_xAxis(title =list(text = "")) %>% 
           hc_title(text = "Faixa Étaria", align = "left") %>% 
           hc_colors(colors = "#129e05") %>% 
           hc_yAxis(gridLineWidth = 0) %>% 
           hc_plotOptions(column = list(
              dataLabels = list(enabled = TRUE)))
     })
     
     output$genero <- renderHighchart({
         dados %>% 
           count(Sexo) %>%
           mutate(porc = round((n/sum(n))*100, 2)) %>% 
           hchart("pie", hcaes(x = Sexo, y = porc),name = "Porcentagem") %>% 
           hc_title(text = "Gênero", align = "left") %>% 
           hc_colors(colors = c("#129e05", "#83db7b")) %>% 
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
     
     output$escolaridade <- renderHighchart({
         
         dados %>% 
           group_by(Escolaridade) %>% 
           count(Escolaridade) %>% 
           arrange(desc(n)) %>% 
           hchart("bar", hcaes(x = Escolaridade, y = n), name = "Total") %>% 
           hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
           hc_xAxis(title =list(text = "")) %>% 
           hc_title(text = "Escolaridade", align = "left") %>% 
           hc_colors(colors = "#129e05") %>% 
           hc_yAxis(gridLineWidth = 0) %>% 
           hc_plotOptions(bar = list(
              dataLabels = list(enabled = TRUE))) 
     })
     
     output$cargo <- renderDataTable({
         
         dados %>% 
           group_by(Cargo) %>% 
           summarise(`Média Salário`= round(mean(Valor_Salario), 2), `Qtd Contratações`=  n())  %>% 
           arrange(desc(`Qtd Contratações`)) %>% 
           datatable(options = list(pageLength = 7,  
                                    scrollY = TRUE,
                                    autoWidth = TRUE,
                                    dom = "Bfrtip",
                                    align =c("l","c","c")))
     })
   
 }
 