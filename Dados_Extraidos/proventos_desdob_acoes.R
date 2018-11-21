# Sites tentatos
# https://br.advfn.com  : faltam datas de pagamento, como em SAPR11
# https://www.infomoney.com.br/ciahering-hgtx3/proventos não tem histórico longo, como em HGTX3
# https://www.bussoladoinvestidor.com.br/guia-empresas/empresa/sapr11/proventos naõ tem proventos de alguns papéis como SAPR11
# https://meusdividendos.com não tem a data de pagamento dos proventos


##############################################
# Carregando lista de UDFs e bibliotecas
##############################################
source("C:/Users/Youse/Desktop/Projetos/UDF/UDF_list.R")
# source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")
UDF_require("plyr")

# setwd("C:/Users/Diego/Desktop/Data_Science/Investimentos")
setwd("C:/Users/Youse/Desktop/Outros/Investimentos/Investimentos")




##################################
# Iniciar a conexão
##################################
rD <- rsDriver(port = 4568L, browser = "chrome")
remDr <- rD$client 

#### Lista de ações
lista_Ac <- c("ITUB3","ITUB4","GRND3","WEGE3",
              "ABEV3","ODPV3","MDIA3","BBDC3",
              "BBDC4","EGIE3","LREN3","PSSA3",
              "ITSA3","ITSA4","FLRY3","KROT3",
              "CIEL3","ARZZ3","RADL3","UGPA3",
              "LINX3","EZTC3","IRBR3","BBSE3",
              "B3SA3","HYPE3","TAEE3","TAEE4",
              "TAEE11","ABCB4","HGTX3","ESTC3",
              "BBAS3","CVCB3","PARD3","HAPV3",
              "MPLU3","SAPR3","SAPR4","SAPR11",
              "ENBR3","QUAL3","FRAS3","CGRA3",
              "CGRA4","MULT3","QGEP3","WIZS3",
              "CARD3","SANB3","SANB4","SANB11",
              "CAML3","BIDI4","EQTL3","ALUP3",
              "ALUP4","ALUP11","VIVT3","VIVT4",
              "SBSP3","UNIP3","UNIP5","UNIP6",
              "VALE3",
              "CRFB3","SULA4","SULA11",
              "SEER3","SNSL3","NATU3","MRVE3",
              "CSMG3","GNDI3","TIET3","TIET4",
              "TIET11","RENT3","SMLS3","BRSR3",
              "BRSR6","CCRO3","CGAS3","CGAS5",
              "VULC3","FESA3","FESA4","SLCE3",
              "TOTS3","LEVE3","MOVI3","GUAR3",
              "GUAR4","TUPY3","CSAN3","LCAM3",
              "ALPA3","ALPA4","MGLU3","PTBL3",
              "POMO3","POMO4","SMTO3","PTNT4")



# Criando a tabela de cotações
ini_dt <- as.Date("2008-01-01")
fim_dt <- as.Date("2018-10-20")
vec_date <- seq.Date(ini_dt,fim_dt,1)
df_prov <- data.frame("Data"=vec_date)



# Criando tabela consolidade
df_prov <- NULL
df_desd <- NULL




##### Fazendo login no site da guiaInvest
# Criando a URL para desdobramentos
url <- "https://www.guiainvest.com.br/login/default.aspx"

#### Carregando a página
remDr$navigate(url)





for(j in lista_Ac){
  
  # j <- "IRBR3"
  

    # Print do papel
    print(paste("Papel: ",j,", ",which(lista_Ac == j),"/",length(lista_Ac),sep=""))
  
    # Criando a URL para desdobramentos
    url <- paste("https://www.meusdividendos.com/empresa/",substr(j, 1, 4),sep="")
    
    #### Carregando a página
    remDr$navigate(url)
    
    
    
    
    #### Selecionando a aba Proventos
    aba_prov <- remDr$findElement(using = "xpath", "//*[@id='main-wrapper']/div/section[2]/div/div[2]/div[2]/ul/li[4]/a")
    
    #### Clicando no botão
    aba_prov$clickElement()
    
    
    #### Selecionando a tabela de grupamentos e desdobramentos
    aux_desd <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(xpath = "//*[@id='dividendos']/div[3]/div/div/div[2]/table") %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame()
    
    
    df_desd <- rbind(df_desd,aux_desd)
    
    # Eliminando as linhas sem data de pagamento
    df_desd <- df_desd[!is.na(df_desd$Data.com) & df_desd$Data.com != "",]
    
    # Deduplicando
    df_desd <- df_desd[!duplicated(df_desd),]
    
    
    
    
    
    
    
    # Criando mais uma URL
    url <- paste("https://www.guiainvest.com.br/provento/default.aspx?sigla=",j,"&proventodinheiropage=1",sep="")
    
    #### Carregando a página
    remDr$navigate(url)
    
    # Temos que capturar a quantidade de linhas do histórico antes de capturar a tabela de proventos
    aux <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(xpath = "//*[@id='ctl00_ctl00_cphConteudo_cphConteudo_ProventoDinheiro1_RadGridProventoDinheiro_ctl00']") %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame()
    
    # Capturando o número de linhas e calculando o número de páginas
    num_linhas <- as.numeric(gsub(".*Registros [[:digit:]]+ - [[:digit:]]+ de ","",aux[1,1]))
    
    
    if(is.na(num_linhas)){
      num_paginas <- 1
    }else{
      num_paginas <- ceiling(num_linhas/10)  
    }
    
    

    # Loop para extração dos dados
    for(i in seq(num_paginas)){
      
      # Print da página
      print(paste("          Página: ",i,sep=""))
      
      if(i>1){
        
        # Criadno a URL de proventos
        url <- paste("https://www.guiainvest.com.br/provento/default.aspx?sigla=",j,"&proventodinheiropage=",i,sep="")
        
        #### Carregando a página
        remDr$navigate(url)
      }
      
      #### Selecionando a tabela histórica de proventos
      aux_prov <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes(xpath = "//*[@id='ctl00_ctl00_cphConteudo_cphConteudo_ProventoDinheiro1_RadGridProventoDinheiro_ctl00']") %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame()  %>%
        mutate(Papel = j)
      
      if(is.na(num_linhas)){
        df_prov <- rbind(df_prov,subset(aux_prov, select = -c(Aprovação)))
      }else{
        df_prov <- rbind(df_prov,subset(aux_prov, subset =  row.names(aux_prov)>2, select = -c(Aprovação,NA.)))
      }

    }
    
    
    # Eliminando as linhas sem data de pagamento
    df_prov <- df_prov[!is.na(df_prov$Pagamento) & df_prov$Pagamento != "",]
  
    # Deduplicando
    df_prov <- df_prov[!duplicated(df_prov),]
    
    
    # Sleep
    Sys.sleep(1.5)
    
    
}



##################################
# Tratando as bases
##################################


##### df_desd
df_desd <- subset(df_desd, select = -Aprovação)
names(df_desd) <- c("Papel","Tipo","DataCom","Fator")
df_desd$DataCom <- as.Date(df_desd$DataCom,"%d/%m/%Y")

df_desd$Fator1 <- round(as.numeric(gsub(" .*","",df_desd$Fator)),0)
df_desd$Fator2 <- round(as.numeric(gsub(".* para ","",df_desd$Fator)),0)

# head(df_desd)
# table(df_desd$Fator)

##### df_prov
names(df_prov) <- c("Tipo","DataCom","Valor","DataPagamento","Papel")

df_prov <- df_prov[df_prov$Valor!="Nenhuma informação encontrada.",]


df_prov$DataCom <- as.Date(df_prov$DataCom,"%d/%m/%y")
df_prov$DataPagamento <- as.Date(df_prov$DataPagamento,"%d/%m/%y")

df_prov$Valor <- as.numeric(gsub(",","\\.",gsub("R\\$","",df_prov$Valor)))
# df_prov$Valor[is.na(a)]
# head(a)
# head(df_prov)

##################################
# Salvando a base
##################################

save(df_prov,file="./Dados_Extraidos/proventos.RData")
save(df_desd,file="./Dados_Extraidos/desdobramentos.RData")


##################################
# Encerrando a conexão
##################################

# Parar o cliente
remDr$close()
# Parar o servidor
rD$server$stop()
