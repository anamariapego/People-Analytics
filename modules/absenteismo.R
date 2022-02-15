
 #  Modules do Aplicativo Shiny People Analytics - Absenteísmo
 
 absenteismo_ui <- function(id){
   ns <- NS(id)
   
   tabItem(
     tabName = "absenteismo",
     fluidRow(
       h2("Absenteísmo - People Analytics", align = "center")
     ),
     fluidRow(
        column(width = 3, infoBoxOutput(width = 12, ns("abs"))),
        column(width = 3, infoBoxOutput(width = 12, ns("faltas"))),
        column(width = 3, infoBoxOutput(width = 12, ns("atrasos"))),
        column(width = 3, infoBoxOutput(width = 12, ns("atestados")))
     ), 
     fluidRow(
        column(width = 6, highchartOutput(ns("abs_tempo"))),
        column(width = 6, highchartOutput(ns("abs_faixa")))
     ),
     fluidRow(
         column(width = 6, highchartOutput(ns("abs_sexo"))),
         column(width = 6, highchartOutput(ns("abs_escolaridade")))
     ),
     fluidRow(
         column(width = 6, dataTableOutput(ns("abs_cargo"))),
         column(width = 6, highchartOutput(ns("abs_cat")))
     )
   )
 }
 
 absenteismo_server <- function(input, output, session, dados){
    
    output$abs <- renderInfoBox({
          
          abs <- dados %>% 
             filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
             summarise(soma = sum(Valor))
          
          horas_normais <- dados %>% 
             filter(Categoria == "Normais") %>% 
             summarise(soma = sum(Valor))
          
          absenteismo_percentil <- round((abs/horas_normais)*100,2)
          
          infoBox(
             "Absenteísmo", paste0(absenteismo_percentil,"%"), icon = icon("user-times"))
       
    })
    
    output$faltas <- renderInfoBox({
       
       falta <- dados %>% 
          filter(Categoria == "Faltas") %>% 
          summarise(soma = sum(Valor)) %>% 
           pull() %>% 
           prettyNum(big.mark = ".", decimal.mark = ",", scientific = FALSE)
       
       infoBox(
          "Total Faltas", falta, icon = icon("dollar-sign"))
       
    })
    
    output$atrasos <- renderInfoBox({
       
       atraso <- dados %>% 
          filter(Categoria == "Atrasos") %>% 
          summarise(soma = sum(Valor)) %>% 
           pull() %>% 
           prettyNum(big.mark = ".", decimal.mark = ",", scientific = FALSE)
       
       infoBox(
          "Total Atrasos", atraso, icon = icon("dollar-sign"))
       
    })
    
    output$atestados <- renderInfoBox({
       
       atestado <- dados %>% 
          filter(Categoria == "Atestados") %>% 
          summarise(soma = sum(Valor)) %>% 
           pull() %>% 
           prettyNum(big.mark = ".", decimal.mark = ",", scientific = FALSE)
       
       infoBox(
          "Total Atestados", atestado, icon = icon("dollar-sign"))
       
    })
    
    output$abs_tempo <- renderHighchart({
       
       dados %>% 
       group_by(Mes_Pag) %>% 
          filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
          summarise(soma = sum(Valor)) %>% na.omit() %>% 
          hchart("line", hcaes(x = Mes_Pag, y = soma)) %>% 
            hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
            hc_xAxis(title =list(text = "")) %>% 
            hc_title(text = "Valor Absenteísmo ao longo do tempo", align = "left") %>% 
            hc_colors(colors = "#bd3d17") %>% 
            hc_yAxis(gridLineWidth = 0) %>% 
            hc_plotOptions(area = list(
                dataLabels = list(enabled = TRUE)))
    })
    
    output$abs_faixa <- renderHighchart({
        
        dados %>% 
            group_by(Faixa_Etaria) %>% 
            filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
            count(Faixa_Etaria) %>% na.omit() %>% 
            hchart("column", hcaes(x = Faixa_Etaria, y = n), name = "Total") %>% 
            hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
            hc_xAxis(title =list(text = "")) %>% 
            hc_title(text = "Faixa Étaria", align = "left") %>% 
            hc_colors(colors = "#bd3d17") %>% 
            hc_yAxis(gridLineWidth = 0) %>% 
            hc_plotOptions(column = list(
                dataLabels = list(enabled = TRUE)))
    })
    
    output$abs_sexo <- renderHighchart({
        
        dados %>% 
            filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
            count(Sexo) %>%
            mutate(porc = round((n/sum(n))*100, 2)) %>% na.omit() %>% 
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
    
    output$abs_escolaridade <- renderHighchart({
        
        dados %>% 
            group_by(Escolaridade) %>% 
            filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
            count(Escolaridade) %>% 
            arrange(desc(n)) %>% 
            hchart("bar", hcaes(x = Escolaridade, y = n), name = "Total") %>% 
            hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
            hc_xAxis(title =list(text = "")) %>% 
            hc_title(text = "Escolaridade", align = "left") %>% 
            hc_colors(colors = "#bd3d17") %>% 
            hc_yAxis(gridLineWidth = 0) %>% 
            hc_plotOptions(bar = list(
                dataLabels = list(enabled = TRUE))) 
    })
    
    output$abs_cargo <- renderDataTable({
        
        dados %>% 
            group_by(Cargo.x) %>% 
            filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
            summarise(`Qtd Contratações`=  n())  %>% 
            arrange(desc(`Qtd Contratações`)) %>% 
            datatable(options = list(pageLength = 7,  
                                     scrollY = TRUE,
                                     autoWidth = TRUE,
                                     dom = "Bfrtip",
                                     align =c("l","c","c")))
    })
    
    output$abs_cat <- renderHighchart({
        
        dados %>% 
            filter(Categoria == "Faltas" | Categoria == "Atrasos" | Categoria == "Atestados") %>% 
            group_by(Categoria) %>% 
            count(Categoria) %>% 
            arrange(desc(n)) %>% 
            hchart("column", hcaes(x = Categoria, y = n), name ="Total") %>% 
            hc_yAxis(title =list(text = ""), visible = FALSE) %>% 
            hc_xAxis(title =list(text = "")) %>% 
            hc_title(text = "Total Absenteísmo por categoria", align = "left") %>% 
            hc_colors(colors = "#bd3d17") %>% 
            hc_yAxis(gridLineWidth = 0) %>% 
            hc_plotOptions(column = list(
                dataLabels = list(enabled = TRUE))) 
    })
 }