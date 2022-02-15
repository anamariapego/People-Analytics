
 #  Modules do Aplicativo Shiny People Analytics - Colaboradores Ativos

 
 ativos_ui <- function(id){
   ns <- NS(id)
   
   tabItem(
     tabName = "ativos",
     fluidRow(
       h2("Colaboradores Ativos - People Analytics", align = "center")
     ),
     fluidRow(
       column(width = 6, highchartOutput(ns("colab"))),
       column(width = 6, highchartOutput(ns("faixa_etaria_colab")))
     ),
     fluidRow(
       column(width = 6, highchartOutput(ns("escolaridade_colab"))),
       column(width = 6, highchartOutput(ns("genero_colab")))
     ),
     fluidRow(
       column(width = 6, highchartOutput(ns("massa"))),
       column(width = 6, dataTableOutput(ns("cargo_colab")))
     )
   )
 }
 
 
 ativos_server <- function(input, output, session, dados){
   
   output$colab <- renderHighchart({
     
     dados %>% 
         group_by(Ano_Adm) %>% 
         filter(Situacao != "Demitido") %>% 
         count(Situacao) %>% 
         summarise(total = sum(n)) %>% 
         hchart("area", hcaes(x = Ano_Adm, y = total), name = "Total") %>% 
         hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
         hc_xAxis(title =list(text = "")) %>% 
         hc_title(text = "Colaboradores Ativos ao longo do tempo", align = "left") %>% 
         hc_colors(colors = "#129e05") %>% 
         hc_yAxis(gridLineWidth = 0) %>% 
         hc_plotOptions(area = list(
            dataLabels = list(enabled = TRUE)))

   })
   
   output$faixa_etaria_colab <- renderHighchart({
     
     dados %>% 
       filter(Situacao != "Demitido") %>% 
       group_by(Faixa_Etaria) %>% 
       count(Faixa_Etaria) %>% na.omit() %>% 
       hchart("column", hcaes(x = Faixa_Etaria, y = n), name ="Total") %>% 
       hc_yAxis(title = list(text = "")) %>% 
       hc_xAxis(title = list(text = "")) %>% 
       hc_title(text = "Faixa Etária", align = "left") %>% 
       hc_colors(colors =  "#129e05") %>% 
       hc_yAxis(gridLineWidth = 0) %>% 
       hc_plotOptions(column = list(
           dataLabels = list(
             enabled = TRUE)))
   })
   
   output$genero_colab <- renderHighchart({
     
     dados %>% 
         filter(Situacao != "Demitido") %>% 
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
   
   output$escolaridade_colab <- renderHighchart({
     
     dados %>% 
       filter(Situacao != "Demitido") %>% 
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
   
   output$cargo_colab <- renderDataTable({
     
     dados %>% 
       filter(Situacao != "Demitido") %>% 
       group_by(Cargo) %>% 
         summarise(`Qtd Contratações`=  n())  %>% 
         arrange(desc(`Qtd Contratações`)) %>% 
         datatable(options = list(pageLength = 7,  
                                  scrollY = TRUE,
                                  autoWidth = TRUE,
                                  dom = "Bfrtip",
                                  align =c("l","c","c")))
   })
   
   output$massa <- renderHighchart({
     
     dados %>% 
         filter(Situacao != "Demitido") %>% 
         group_by(Ano_Adm) %>% 
         summarise(n = sum(Valor_Salario)) %>% 
         hchart("column", hcaes(x = Ano_Adm, y = n), name ="Total") %>% 
         hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
         hc_xAxis(title =list(text = "")) %>% 
         hc_title(text = "Massa Salarial por ano", align = "left") %>% 
         hc_colors(colors = "#129e05") %>% 
         hc_yAxis(gridLineWidth = 0) %>% 
         hc_plotOptions(column = list(
            dataLabels = list(enabled = TRUE))) 

     
   })
   
 }