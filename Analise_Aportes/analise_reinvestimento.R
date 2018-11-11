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


##########################
# Carregando os dados
##########################

load(file="./Dados_Extraidos/cotacoes.RData")
load(file="./Dados_Extraidos/proventos.RData")
load(file="./Dados_Extraidos/desdobramentos.RData")




##########################
# Tratando os dados
##########################


#### Filtrando pelo tempo
# Vamos trabalhar apenas com dados entre 01/01/2013 até 31/12/2017: 5 anos
df_cot <- df_cot[df_cot$Data >= "2013-01-01" & df_cot$Data <= "2017-12-31",]
df_desd <- df_desd[df_desd$DataCom >= "2013-01-01" & df_desd$DataCom <= "2017-12-31",]
df_prov <- df_prov[df_prov$DataCom >= "2013-01-01" & df_prov$DataCom <= "2017-12-31",] # Usamos o DataCom no filtro ao invés do DataPagamento pq 
                                                                                       # se o DataCom for antes da data limite inferior, o cliente
                                                                                       # ainda não teria ações então não poderia receber os dividendos.


# Removendo dias sem pregão no início da janela de tempo
per_ac_dt <- apply(df_cot[2:ncol(df_cot)],1,function(x){ sum( !is.na(x) )/length(x) })
table(a,useNA="ifany")
df_cot <- df_cot[per_ac_dt>=0.2,]
# head(df_cot)


# Removendo as colunas sem operação em janeiro de 2013
prench_col <- sapply(df_cot[1:5,],function(x){any(!is.na(x))})
df_cot <- df_cot[prench_col]
# sum(prench_col>0.7)/length(prench_col)


# Algumas ações apresentam missings quando o pregão funcionou. Vamos usar a cotação de fechamento do 
# dia anterior. Caso também seja missing, usaremos do dia posterior
aux <- sapply(df_cot,function(x){
  
  # Criando uma base com cotação original, anterior e posterior
  df_aux <- data.frame("Orig"=x,"Ante_D1"=lag(x,n = 1),"Post_D1"=lead(x,n = 1),"Ante_D2"=lag(x,n = 2),"Post_D2"=lead(x,n = 2))
  
  # Retornando a primeira cotação diferente de missing
  
})


x <- df_cot[,"UNIP3"]
df_aux <- data.frame("Orig"=x,"Ante_D1"=lag(x,n = 1),"Post_D1"=lead(x,n = 1),"Ante_D2"=lag(x,n = 2),"Post_D2"=lead(x,n = 2))
retu <- apply(df_aux,1,function(y){
  y[which(!is.na(y))[1]]
})

head(data.frame(x,retu),50)
head(df_aux,10)

head(df_aux)


# Visualizando % de ações presentes ao longo do tempo
per_ac_dt <- apply(df_cot[2:ncol(df_cot)],1,function(x){ sum( !is.na(x) )/length(x) })
plot(df_cot$Data,per_ac_dt)
tail(df_cot)

#### QUais ações não estavam presentes desde o início? 
acoes_inicio <- sapply(df_cot,function(x){ sum(!is.na(x))/length(x)})
acoes_inicio <- names(acoes_inicio[acoes_inicio>=1])


#### Mantendo apenas as ações que estavam listadas desde o início
df_cot <- df_cot[acoes_inicio]