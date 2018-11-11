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

url <- "https://br.advfn.com/bolsa-de-valores/bovespa/itau-unibanco-ITUB3/dividendos"

#### Carregando a pÃ¡gina
remDr$navigate(url)


lista_Em <- lista_Ac



gsub("([[:alnum:]]{4})","\\1",lista_Ac)





for(i in seq(length(lista_Ac))){

    print(paste("Iteração ",i,"/",length(lista_Ac),sep=""))
    
  #### Selecionando a caixa de escrita
  buscar_caixa <- remDr$findElement(using = "xpath", "//*[@id='symbol_entry']")
  
  #### Escrevendo na caixa de busca
  buscar_caixa$clearElement()
  # buscar_caixa$sendKeysToElement(list("BOV:ITUB4"))
  buscar_caixa$sendKeysToElement(list(paste("BOV:",lista_Ac[i],sep="")))
  
  
  #### Selecionando o botão de busca
  buscar_botao <- remDr$findElement(using = "xpath", "//*[@id='symbol_ok']")
  
  #### Clicando no botão
  buscar_botao$clickElement()
  
  
  #### Tabela de proventos
  tab_prov <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
    rvest::html_nodes(xpath = "//*[@id='id_dividends']") %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame()
    
    
    
    Sys.sleep(2)
    
  
    
    
    #### Tratando a tabela de proventos
    tab_prov_trat <- tab_prov[,c("Data.de.Pagamento","Tipo.de.Ação","Valor")]
    names(tab_prov_trat) <- c("Data_Pgto","Tipo","Valor_Pgto")

    txt_orig <- c(" Jan "," Fev "," Mar "," Abr "," Mai "," Jun "," Jul "," Ago "," Set "," Out "," Nov "," Dez ")
    txt_dest <- c(paste("/0",seq(9),"/",sep=""),paste("/",seq(10,12),"/",sep=""))
    
    for(j in seq(length(txt_orig))){
      tab_prov_trat$Data_Pgto <- gsub(txt_orig[j],txt_dest[j],tab_prov_trat$Data_Pgto)
    }
    
    tab_prov_trat$Data_Pgto <- as.Date(tab_prov_trat$Data_Pgto,"%d/%m/%Y")
    
    tab_prov_trat$Valor_Pgto <- as.numeric(gsub(",","\\.",tab_prov_trat$Valor_Pgto))

    # Filtrando o tipo de ação certa
    if(gsub("[[:alpha:]]{4}","",lista_Ac[i]) %in% c(3)){
      
      tab_prov_trat <- tab_prov_trat[tab_prov_trat$Tipo=="Ordinária",]
      
    }else if(gsub("[[:alpha:]]{4}","",lista_Ac[i]) %in% c(4,5,6)){
      
    }
      
      
    
    
    
    # head(tab_prov_trat)
    
    
    
    
    
    
    
    
    
    
    # head(tab_prov_trat)
    
    #### Cruzando as tabelas
    df_prov <- merge(df_prov,tab_prov_trat,by="Data",by.y="Data_Pgto",all.x=T)
    
    #### Renomeando a coluna
    names(df_prov)[i+1] <- lista_Ac[i]
    
    # head(df_prov)
    # names(df_prov)
    # 
    # sum(!is.na(df_prov$ITUB3))
  
}


# head(df_prov)

a <- sapply(df_prov,function(x){ sum(!is.na(x))/length(x)})
a[a==0]

# #### FIltrando a tabela
# df_prov <- df_prov[!is.na(df_prov$ITUB3) & df_prov$Data >= "2013-10-20",]
# 
# # a <- df_prov[df_prov$Data >= "2013-01-01",]
# 
# #### QUais ações não estavam presentes desde o início? 
# acoes_inicio <- sapply(df_prov,function(x){ sum(!is.na(x))/length(x)})
# acoes_inicio <- names(acoes_inicio[acoes_inicio>=1])
# 
# 
# #### Mantendo apenas as ações que estavam listadas desde o início
# df_prov <- df_prov[acoes_inicio]


# head(df_prov)


# plot(df_prov$Data,df_prov$QUAL3)


##################################
# Salvando a base
##################################

save(df_prov,file="proventos.RData")


##################################
# Encerrando a conexão
##################################
remDr$close()
