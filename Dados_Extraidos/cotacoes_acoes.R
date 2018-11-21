source("C:/Users/Youse/Desktop/Projetos/UDF/UDF_list.R")
# source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")

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
df_cot <- data.frame("Data"=vec_date)
# df_cot <- as.data.frame(cbind(data.frame("Data"=vec_date)
#                 ,
#                 as.data.frame(matrix( rep(NA,length(lista_Ac)*length(vec_date)),ncol =length(lista_Ac), dimnames = list(seq(length(vec_date)), lista_Ac  )   ))
#                 ))

# head(df_cot)


for(i in seq(length(lista_Ac))){

    print(paste("Iteração ",i,"/",length(lista_Ac),sep=""))
    
    # Definindo a URL para o robÃ´
    # url <- "http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=hgtx3.SA&beginDay=1&beginMonth=1&beginYear=2013&endDay=30&endMonth=10&endYear=2018&page=1&size=2000"
    # url <- paste("http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=",lista_Ac[i],".SA&beginDay=1&beginMonth=1&beginYear=2013&endDay=30&endMonth=10&endYear=2018&page=1&size=2000",sep="")
    url <- paste("http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=",lista_Ac[i],
                 ".SA&beginDay=",as.numeric(format(ini_dt,"%d")),"&beginMonth=",as.numeric(format(ini_dt,"%m")),"&beginYear=",as.numeric(format(ini_dt,"%Y")),
                 "&endDay=",as.numeric(format(fim_dt,"%d")),"&endMonth=",as.numeric(format(fim_dt,"%m")),"&endYear=",as.numeric(format(fim_dt,"%Y")),"&page=1&size=",round(as.numeric(fim_dt-ini_dt)*1.2,0),sep="")
    
    
    ##################################
    # Web Scrapper - COTAÇÕES UOL
    ##################################
    
    #### Carregando a pÃ¡gina
    remDr$navigate(url)
    
    
    #### Tabela de ações
    tab_cot_acoes_aux <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(xpath = "//*[@id='tblInterday']") %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame()
    
    #### Pegando as colunas necessárias
    tab_cot_acoes <- tab_cot_acoes_aux[c("Data.Hora","Cotação")]
    
    #### transformando no formato data
    tab_cot_acoes$Data.Hora <- as.Date(tab_cot_acoes$Data.Hora,"%d/%m/%Y")
    
    #### substituindo a vírgula
    tab_cot_acoes$Cotação <- as.numeric(gsub(",","\\.",tab_cot_acoes$Cotação))
    
    
    # head(tab_cot_acoes)
    
    #### Cruzando as tabelas
    df_cot <- merge(df_cot,tab_cot_acoes,by.x="Data",by.y="Data.Hora",all.x=T)
    
    #### Renomeando a coluna
    names(df_cot)[i+1] <- lista_Ac[i]
    
    # head(df_cot)
    # names(df_cot)
    # 
  
}


# head(df_cot)

# #### FIltrando a tabela
# df_cot <- df_cot[!is.na(df_cot$ITUB3) & df_cot$Data >= "2013-10-20",]
# 
# # a <- df_cot[df_cot$Data >= "2013-01-01",]
# 
# #### QUais ações não estavam presentes desde o início? 
# acoes_inicio <- sapply(df_cot,function(x){ sum(!is.na(x))/length(x)})
# acoes_inicio <- names(acoes_inicio[acoes_inicio>=1])
# 
# 
# #### Mantendo apenas as ações que estavam listadas desde o início
# df_cot <- df_cot[acoes_inicio]


# head(df_cot)


# plot(df_cot$Data,df_cot$QUAL3)


##################################
# Salvando a base
##################################

save(df_cot,file="./Dados_Extraidos/cotacoes.RData")


##################################
# Encerrando a conexão
##################################

# Parar o cliente
remDr$close()
# Parar o servidor
rD$server$stop()
