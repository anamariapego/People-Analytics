  # Análise de dados do RH

 # Diretório de trabalho 
 setwd("C:\\Users\\JDC\\AnaPinheiro\\AnaliseDadosRH")
 
 # Bibliotecas necessárias
 library(tidyverse)
 library(readxl)
 library(lubridate)
 
 # Abrindo script de funções
 source("funcoes.R", encoding = "UTF-8")
 
 # Carregando os dados ----
 
 # Dataset Funcionários
 df <- read_excel("BaseDados/Funcionarios.xlsx")
 
 # Arquivos Financeiro
 setwd("C:\\Users\\JDC\\AnaPinheiro\\AnaliseDadosRH\\BaseDados")
 
 financeiro <- list.files(getwd(), pattern = "*.csv") %>% 
   map_df(~read.csv(., sep = ";"))
 
 setwd("C:\\Users\\JDC\\AnaPinheiro\\AnaliseDadosRH")
 
 # Transformação e Limpeza dos dados ----
 
 #     Dataset Funcionários
 
 # Limpando os nomes das colunas
 colnames(df) <- ajustar_nomes(names(df))
 
 # Tipo dos dados 
 str(df)
 
 # Removendo colunas que não serão utilizadas na análise 
 funcionarios <- df %>% select(-c("Cod_Cargo", "Cod_Nacionalidade", "Cod_Raca", "Cod_Situacao",
                                  "Cod_Escala", "Cod_C_custo", "Cod_T_Contrato", "Cod_Estado_Civil",
                                  "Cod_Escolaridade", "Cod_Causa_Afastamento"))
 
 # Transformando as colunas do tipo data
 funcionarios$Data_Admissao <- as.Date(funcionarios$Data_Admissao, format = "%d/%m/%Y")
 funcionarios$Data_Afastamento <- as.Date(funcionarios$Data_Afastamento, format = "%d/%m/%Y")
 funcionarios$Data_Salario <- as.Date(funcionarios$Data_Salario, format = "%d/%m/%Y")
 funcionarios$Data_Nascimento <- as.Date(funcionarios$Data_Nascimento, format = "%d/%m/%Y")
 
 # Criando a coluna ano de admissão e ano de desligamento
 # dados_rh_v2$Mes <- months(dados_rh_v2$Data_Admissao) %>% stri_trans_totitle()
 funcionarios$Ano_Adm <- year(funcionarios$Data_Admissao)
 funcionarios$Ano_Desl <-  year(funcionarios$Data_Afastamento)
 
 # Criando faixa etária 
 funcionarios$Idade <- 2022 - year(funcionarios$Data_Nascimento) 
 
 funcionarios <- funcionarios %>% 
    mutate(Faixa_Etaria = case_when(
       Idade >= 17 & Idade <= 24 ~ "15-24",
       Idade > 24 & Idade <= 34 ~ "25-34",
       Idade > 34 & Idade <= 44 ~ "35-44",
       Idade > 44 & Idade <= 54 ~ "45-54",
       Idade > 54 & Idade <= 100 ~ "Acima de 55"))
 
 # Removendo a coluna Idade pois não será mais útil
 funcionarios$Idade <- NULL
 
 
 #     Dataset Financeiro 
 
 # Renomeando as duas colunas que tem acentos
 financeiro <- rename(financeiro, Mes_Pag = Mês.Pag.)
 financeiro <- rename(financeiro, Descricao = Descrição)
 
 # Tipo dos dados
 str(financeiro)
 
 # Limpando e transformando a coluna Valor
 financeiro$Valor <- gsub("\\.","", financeiro$Valor) 
 financeiro$Valor <- gsub("\\,",".", financeiro$Valor) 
 
 financeiro$Valor <- as.numeric(financeiro$Valor)
 
 # Criando uma coluna Categoria 
 financeiro <- financeiro %>% 
   mutate(Categoria = case_when(
     str_detect(Descricao, "Extra") ~ "Extras",
     str_detect(Descricao, "Norm") ~ "Normais",
     str_detect(Descricao, "Falta") ~ "Faltas",
     str_detect(Descricao, "Atest") ~ "Atestados",
     str_detect(Descricao, "Atras") ~ "Atrasos",
     str_detect(Descricao, "Fér") ~ "Férias"))
 
 financeiro$Mes_Pag <- as.factor(financeiro$Mes_Pag)
 financeiro$Mes_Pag <-  ordered(financeiro$Mes_Pag, levels = c("fev/20", "mar/20", "abr/20", 
                                                               "mai/20", "jun/20", "jul/20"))
 
 # Unindo os datasets
 financeiro <- financeiro %>% 
    full_join(funcionarios, by = c('Matricula' = 'Cod_Funcionario'))
