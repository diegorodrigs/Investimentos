# Sites tentatos
# https://br.advfn.com  : faltam datas de pagamento, como em SAPR11
# https://www.infomoney.com.br/ciahering-hgtx3/proventos não tem histórico longo, como em HGTX3
# https://www.bussoladoinvestidor.com.br/guia-empresas/empresa/sapr11/proventos naõ tem proventos de alguns papéis como SAPR11
# https://meusdividendos.com não tem a data de pagamento dos proventos


##############################################
# Carregando lista de UDFs e bibliotecas
##############################################
source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")
UDF_require("plyr")

setwd("C:/Users/Diego/Desktop/Data_Science/Investimentos")



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
              "VALE3")


# Criando a tabela de cotações
vec_date <- seq.Date(as.Date("2013-01-01"),as.Date("2018-10-20"),1)
df_prov <- data.frame("Data"=vec_date)



# Criando tabela consolidade
tab_prov_hist <- NULL
tab_desd <- NULL




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
    
    
    tab_desd <- rbind(tab_desd,aux_desd)
    
    # Eliminando as linhas sem data de pagamento
    tab_desd <- tab_desd[!is.na(tab_desd$Data.com) & tab_desd$Data.com != "",]
    
    # Deduplicando
    tab_desd <- tab_desd[!duplicated(tab_desd),]
    
    
    
    
    
    
    
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
        tab_prov_hist <- rbind(tab_prov_hist,subset(aux_prov, select = -c(Aprovação)))
      }else{
        tab_prov_hist <- rbind(tab_prov_hist,subset(aux_prov, subset =  row.names(aux_prov)>2, select = -c(Aprovação,NA.)))
      }

    }
    
    
    # Eliminando as linhas sem data de pagamento
    tab_prov_hist <- tab_prov_hist[!is.na(tab_prov_hist$Pagamento) & tab_prov_hist$Pagamento != "",]
  
    # Deduplicando
    tab_prov_hist <- tab_prov_hist[!duplicated(tab_prov_hist),]
    
    
    # Sleep
    Sys.sleep(1.5)
    
    
}



##################################
# Tratando as bases
##################################


##### tab_desd
tab_desd <- subset(tab_desd, select = -Aprovação)
names(tab_desd) <- c("Papel","Tipo","DataCom","Fator")
tab_desd$DataCom <- as.Date(tab_desd$DataCom,"%d/%m/%Y")

tab_desd$Fator1 <- round(as.numeric(gsub(" .*","",tab_desd$Fator)),0)
tab_desd$Fator2 <- round(as.numeric(gsub(".* para ","",tab_desd$Fator)),0)

# head(tab_desd)
# table(tab_desd$Fator)

##### tab_prov_hist
names(tab_prov_hist) <- c("Tipo","DataCom","Valor","DataPagamento","Papel")

tab_prov_hist <- tab_prov_hist[tab_prov_hist$Valor!="Nenhuma informação encontrada.",]


tab_prov_hist$DataCom <- as.Date(tab_prov_hist$DataCom,"%d/%m/%y")
tab_prov_hist$DataPagamento <- as.Date(tab_prov_hist$DataPagamento,"%d/%m/%y")

tab_prov_hist$Valor <- as.numeric(gsub(",","\\.",gsub("R\\$","",tab_prov_hist$Valor)))
# tab_prov_hist$Valor[is.na(a)]
# head(a)
# head(tab_prov_hist)

##################################
# Salvando a base
##################################

save(tab_prov_hist,file="./Dados_Extraidos/proventos.RData")
save(tab_desd,file="./Dados_Extraidos/desdobramentos.RData")


##################################
# Encerrando a conexão
##################################

# Parar o cliente
remDr$close()
# Parar o servidor
rD$server$stop()
