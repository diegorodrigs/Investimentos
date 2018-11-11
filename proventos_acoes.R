# Sites
# https://br.advfn.com  : faltam datas de pagamento, como em SAPR11
# https://www.infomoney.com.br/ciahering-hgtx3/proventos não tem histórico longo, como em HGTX3
# https://www.bussoladoinvestidor.com.br/guia-empresas/empresa/sapr11/proventos naõ tem proventos de alguns papéis como SAPR11
# https://meusdividendos.com não tem a data de pagamento dos proventos





# source("C:/Users/Youse/Desktop/Projetos/UDF/UDF_list.R")
source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")
UDF_require("plyr")

setwd("C:/Users/Diego/Desktop/Investimento")

##################################
# Iniciar a conexão
##################################
rD <- rsDriver(port = 4444L, browser = "chrome",)
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
              "VALE3")


# Criando a tabela de cotações
vec_date <- seq.Date(as.Date("2013-01-01"),as.Date("2018-10-20"),1)
df_prov <- data.frame("Data"=vec_date)
# df_prov <- as.data.frame(cbind(data.frame("Data"=vec_date)
#                 ,
#                 as.data.frame(matrix( rep(NA,length(lista_Ac)*length(vec_date)),ncol =length(lista_Ac), dimnames = list(seq(length(vec_date)), lista_Ac  )   ))
#                 ))

# head(df_prov)


# Criando tabela consolidade
tab_prov_hist <- NULL
tab_desd <- NULL



for(j in lista_Ac){
  
    # j <- lista_Ac[1]
  
  
    # url <- "https://br.advfn.com/bolsa-de-valores/bovespa/itau-unibanco-ITUB3/dividendos"
    # url <- "https://www.meusdividendos.com/empresa/HGTX"
    url <- paste("https://www.meusdividendos.com/empresa/",substr(j, 1, 4),sep="")
    
    #### Carregando a pÃ¡gina
    remDr$navigate(url)
    
    
    
    
    #### Selecionando a aba Proventos
    aba_prov <- remDr$findElement(using = "xpath", "//*[@id='main-wrapper']/div/section[2]/div/div[2]/div[2]/ul/li[4]/a")
    
    #### Clicando no botão
    aba_prov$clickElement()
    
    
    
    
    
    
    
    #### Selecionando a tabela de proventos - 2018
    # tab_prov_2018 <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
    #   rvest::html_nodes(xpath = "//*[@id='dividendos']/div[1]/div/div/div[2]/table") %>%
    #   rvest::html_table(fill = TRUE) %>%
    #   as.data.frame()
    
    
    #### Selecionando a tabela de grupamentos e desdobramentos
    aux_desd <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(xpath = "//*[@id='dividendos']/div[3]/div/div/div[2]/table") %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame()
    
    
    tab_desd <- rbind(tab_desd,aux_desd)
    
    # Eliminando as linhas sem data de pagamento
    tab_desd <- tab_desd[!is.na(tab_desd$Data.com) & tab_desd$Data.com != "",]
    
    # Deduplicando
    tab_desd <- tab_desd[!duplicated(tab_desd),]
    
    
    
    
    
    
    
    
    # url <- "https://br.advfn.com/bolsa-de-valores/bovespa/itau-unibanco-ITUB3/dividendos"
    # url <- "https://www.guiainvest.com.br/provento/default.aspx?sigla=hgtx3&proventodinheiropage=1"
    url <- paste("https://www.guiainvest.com.br/provento/default.aspx?sigla=",j,"&proventodinheiropage=1",sep="")
    
    #### Carregando a pÃ¡gina
    remDr$navigate(url)
    
    # Temos que capturar a quantidade de linhas do histórico antes de capturar a tabela de proventos
    aux <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(xpath = "//*[@id='ctl00_ctl00_cphConteudo_cphConteudo_ProventoDinheiro1_RadGridProventoDinheiro_ctl00']") %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame()
    
    num_linhas <- as.numeric(gsub(".*Registros [[:digit:]]+ - [[:digit:]]+ de ","",aux[1,1]))
    num_paginas <- ceiling(num_linhas/10)
    

    
    
    
    
    
    for(i in seq(num_paginas)){
      
      if(i>1){
        
        # url <- paste("https://www.guiainvest.com.br/provento/default.aspx?sigla=hgtx3&proventodinheiropage=",i,sep="")
        # url <- "https://www.guiainvest.com.br/provento/default.aspx?sigla=hgtx3&proventodinheiropage=1"
        url <- paste("https://www.guiainvest.com.br/provento/default.aspx?sigla=",j,"&proventodinheiropage=",i,sep="")
        
        #### Carregando a pÃ¡gina
        remDr$navigate(url)
      }
      
      #### Selecionando a tabela histórica de proventos
      aux_prov <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes(xpath = "//*[@id='ctl00_ctl00_cphConteudo_cphConteudo_ProventoDinheiro1_RadGridProventoDinheiro_ctl00']") %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame()  %>%
        mutate(Papel = j)
      
      tab_prov_hist <- rbind(tab_prov_hist,subset(aux_prov, subset =  row.names(aux_prov)>2, select = -c(Aprovação,NA.)))
      # subset(tab_prov_hist,select = -c(Aprovação,NA.))
      
    }
    
    
    # Eliminando as linhas sem data de pagamento
    tab_prov_hist <- tab_prov_hist[!is.na(tab_prov_hist$Pagamento) & tab_prov_hist$Pagamento != "",]
  
    # Deduplicando
    tab_prov_hist <- tab_prov_hist[!duplicated(tab_prov_hist),]
    
    
}








##################################
# Salvando a base
##################################

save(tab_prov_hist,file="proventos.RData")
save(tab_desd,file="desdobramentos.RData")


##################################
# Encerrando a conexão
##################################
remDr$close()
