# install.packages(stringi)
library(stringi)
library(tidyverse)

# Função para remover acentos

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

# Ajustar nome
ajustar_nomes=function(x){
  x%>%
    stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
    stringi:: stri_trans_totitle () %>%            # Converte a primeira letra para maisculo
    rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("[/' '.()]", "_") %>% #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
    stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
  
}

# Removendo acentos de colunas
removendo_acentos <- function(variables){
  if(is.character(variables)){
    return(rm_accent(variables))
  } else {
      return(variables)
    }
  
}


# Função para transformar para minúsculo

string_minusculo <- function(variables) {
  if (is.character(variables)) {
    return(tolower(variables))
  } else {
    return(variables)
  }
}

