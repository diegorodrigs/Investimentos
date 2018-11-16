##############################################
# Carregando lista de UDFs e bibliotecas
##############################################
source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")
UDF_require("dplyr")

setwd("C:/Users/Diego/Desktop/Data_Science/Investimentos")


##########################
# Carregando os dados
##########################

load(file="./Dados_Extraidos/cotacoes.RData")
load(file="./Dados_Extraidos/proventos.RData")
load(file="./Dados_Extraidos/desdobramentos.RData")


##########################
# Carregando funções
##########################

source("./Analise_Aportes/funcoes.R")




######################################################################################################################################
######################################################################################################################################
#                                                       Tratando os dados
######################################################################################################################################
######################################################################################################################################

#### Filtrando pelo tempo
# Vamos trabalhar apenas com dados entre 01/01/2013 até 31/12/2017: 5 anos
df_cot <- df_cot[df_cot$Data >= "2013-01-01" & df_cot$Data <= "2017-12-31",]
df_desd <- df_desd[df_desd$DataCom >= "2013-01-01" & df_desd$DataCom <= "2017-12-31",]
df_prov <- df_prov[df_prov$DataCom >= "2013-01-01" & df_prov$DataCom <= "2017-12-31",] # Usamos o DataCom no filtro ao invés do DataPagamento pq 
                                                                                       # se o DataCom for antes da data limite inferior, o cliente
                                                                                       # ainda não teria ações então não poderia receber os dividendos.


# Removendo dias sem pregão no início da janela de tempo
per_ac_dt <- apply(df_cot[2:ncol(df_cot)],1,function(x){ sum( !is.na(x) )/length(x) })
# table(a,useNA="ifany")
df_cot <- df_cot[per_ac_dt>=0.2,]
# head(df_cot)


# Removendo as colunas sem operação em janeiro de 2013
prench_col <- sapply(df_cot[1:5,],function(x){any(!is.na(x))})
df_cot <- df_cot[prench_col]
# sum(prench_col>0.7)/length(prench_col)



# Algumas ações apresentam missings quando o pregão funcionou. Vamos usar a cotação de fechamento do 
# dia anterior. Caso também seja missing, usaremos do dia posterior
df_cot2 <- as.data.frame(sapply(df_cot,function(x){
  
  if(class(x)=="Date"){
    return(as.Date(x))
  }else{
    # Criando uma base com cotação original, anterior e posterior
    df_aux <- data.frame("Orig"=x,"Ante_D1"=lag(x,n = 1),"Post_D1"=lead(x,n = 1),"Ante_D2"=lag(x,n = 2),"Post_D2"=lead(x,n = 2),stringsAsFactors = F)
    
    # Retornando a primeira cotação diferente de missing
    retur <- apply(df_aux,1,function(y){
      outp <- y[which(!is.na(y))[1]]
      return(outp)
    })
    
    # Retornando o retur
    return(retur)
  }
}))


df_cot2 <- as.data.frame(sapply(df_cot2,function(x){
  
  if(class(x)=="Date"){
    return(as.Date(x))
  }else{
    # Criando uma base com cotação original, anterior e posterior
    df_aux <- data.frame("Orig"=x,"Ante_D1"=lag(x,n = 1),"Post_D1"=lead(x,n = 1),"Ante_D2"=lag(x,n = 2),"Post_D2"=lead(x,n = 2),stringsAsFactors = F)
    
    # Retornando a primeira cotação diferente de missing
    retur <- apply(df_aux,1,function(y){
      outp <- y[which(!is.na(y))[1]]
      return(outp)
    })
    
    # Retornando o retur
    return(retur)
  }
}))

df_cot2$Data <- as.Date(df_cot2$Data,origin="1970-01-01")
head(df_cot2)
UDF_var_domain(df_cot2)



# Capturando as variáveis que ainda tem missings
var_miss <- UDF_var_fill(df_cot2)
var_miss <- names(var_miss[var_miss<1])

df_cot3 <- df_cot2
# UDF_var_domain(df_cot2)
# df_cot3[seq(2,ncol(df_cot3))] <- sapply(df_cot3[seq(2,ncol(df_cot3))],function(x){as.numeric(as.character(head(x)))})
# head(df_cot3[2:6])

# Preenchendo com a última cotação não-missing
for(var_m in var_miss){
  for(i in seq(nrow(df_cot3)-1)){
    # print(paste(var_m,i,sep=", "))
    x <- c(df_cot3[i+1,var_m],df_cot3[i,var_m])
    df_cot3[i+1,var_m] <- coalesce(x,max(x,na.rm=T))[1]
  }
}


# UDF_var_fill(df_cot3)
# 
# plot(df_cot3$FRAS3)

# UDF_var_domain(df_cot3)

######################################################################################################################################
######################################################################################################################################
#                                                       Criando Variáveis
######################################################################################################################################
######################################################################################################################################


##### Dia útil do mês
df_cot3$Data <- as.Date(df_cot3$Data)
df_cot3$AnoMes <- UDF_var_month(df_cot3$Data,form = "%Y-%m")


df_cot3 <- df_cot3 %>% group_by(AnoMes) %>% mutate(DiaUtil = row_number()) %>% as.data.frame()

# head(df_cot3[c("Data","AnoMes","DiaUtil")],30)

# Histograma dos dias úteis
table(df_cot3$DiaUtil,useNA = "ifany")







######################################################################################################################################
######################################################################################################################################
#                                                       SIMULAÇÃO
######################################################################################################################################
######################################################################################################################################


################################
# Definições
################################

##### Parâmetros
qtde_papeis <- 5
aporte_inicial <- 10000
aporte_mensal <- 2000


##### Pesos para decidir aporte
vec_pesos = c("Valorizacao"=0.5,"Distrib"=0.5)


##### Estratégias
    # Reinveste dividendos?
    estr_rei_div <- "S2" # "N1": Não; "S1": Sim, no papel que a originou; "S2": Sim, em qualquer papel
    # Quantidade de ações aportadas por mês
    estr_qtd_apo <- 1

##### Seleções aleatórias
set.seed(84)
dia_util <- sample(seq(18),1) #14
papeis_carteira <- sample(names(df_cot3[2:ncol(df_cot3)]),qtde_papeis) # "RADL3" "SBSP3" "SANB3" "PSSA3" "ITSA3"
TAM <- length(papeis_carteira)



################################
# Preparando as bases
################################

df_cot4 <- df_cot3[df_cot3$DiaUtil==dia_util,c("Data",papeis_carteira)]
df_prov2 <- df_prov[df_prov$Papel %in% papeis_carteira,]
df_desd2 <- df_desd[df_desd$Papel %in% papeis_carteira,]
# head(df_desd)
# head(df_cot4)

# UDF_var_domain(df_cot4)
# sapply()


# Criando a base para simulação
df_sim <- df_cot4
row.names(df_sim) <- seq(nrow(df_sim))
for(colu in seq(TAM)){
  x <- data.frame(rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)),rep(0,nrow(df_cot4)))
  names(x) <- c(paste("Custo",papeis_carteira[colu],sep="_"),paste("Qtde",papeis_carteira[colu],sep="_"),paste("PM",papeis_carteira[colu],sep="_"),
                paste("ValorMercado",papeis_carteira[colu],sep="_"),paste("Valorizacao",papeis_carteira[colu],sep="_"),paste("Concentracao",papeis_carteira[colu],sep="_"),
                paste("Provent",papeis_carteira[colu],sep="_"))
  df_sim <- cbind(df_sim,x)  
}

df_sim$Montante_Prove <- 0
df_sim$Saldo_Restante <- 0
df_sim$Valor_Carteira <- 0
df_sim$Custo_Carteira <- 0

# head(df_sim)


# Gerando o aporte inicial
apo_ini_por_ppl <- aporte_inicial/TAM
# for(colu in seq(TAM)){
  
  # # Preenchendo a quantidade de ações
  # df_sim[1,paste("Qtde",papeis_carteira[colu],sep="_")] <- floor(apo_ini_por_ppl/df_sim[1,papeis_carteira[colu]])
  # 
  # # Preenchendo o custo
  # df_sim[1,paste("Custo",papeis_carteira[colu],sep="_")] <- round(df_sim[1,paste("Qtde",papeis_carteira[colu],sep="_")]*df_sim[1,papeis_carteira[colu]],2)
  # 
  # # Preenchendo o PM
  # df_sim[1,paste("PM",papeis_carteira[colu],sep="_")] <- df_sim[1,papeis_carteira[colu]]
  # 
  # # Preenchendo o Valor de Mercado
  # df_sim[1,paste("ValorMercado",papeis_carteira[colu],sep="_")] <- df_sim[1,paste("Custo",papeis_carteira[colu],sep="_")] 
  # 
  # # Preenchendo a Valorização
  # df_sim[1,paste("Valorizacao",papeis_carteira[colu],sep="_")] <- round((df_sim[1,paste("ValorMercado",papeis_carteira[colu],sep="_")]-df_sim[1,paste("Custo",papeis_carteira[colu],sep="_")])/df_sim[1,paste("Custo",papeis_carteira[colu],sep="_")],2)
  # 
  # # Preenchendo a concentração no papel dentro da carteira
  # df_sim[1,paste("Concentracao",papeis_carteira[colu],sep="_")] <- round(df_sim[1,paste("ValorMercado",papeis_carteira[colu],sep="_")]/sum(df_sim[1,grep("ValorMercado",names(df_sim),value=T)]),2)
  # 
  # # Preenchendo o montante de proventos recebidos para o dado papel no dado mês
  # df_sim[1,paste("Provent",papeis_carteira[colu],sep="_")] <- 0
  
# }





##############################################
# Preenchendo os dados do primeiro mês
##############################################

# Preenchendo a quantidade de ações
df_sim[1,paste("Qtde",papeis_carteira,sep="_")] <- floor(apo_ini_por_ppl/df_sim[1,papeis_carteira])

# Preenchendo o custo
df_sim[1,paste("Custo",papeis_carteira,sep="_")] <- round(df_sim[1,paste("Qtde",papeis_carteira,sep="_")]*df_sim[1,papeis_carteira],2)

# Preenchendo o PM
df_sim[1,paste("PM",papeis_carteira,sep="_")] <- df_sim[1,papeis_carteira]

# Preenchendo o Valor de Mercado
df_sim[1,paste("ValorMercado",papeis_carteira,sep="_")] <- df_sim[1,paste("Custo",papeis_carteira,sep="_")]

# Preenchendo a Valorização
df_sim[1,paste("Valorizacao",papeis_carteira,sep="_")] <- round((df_sim[1,paste("ValorMercado",papeis_carteira,sep="_")]-df_sim[1,paste("Custo",papeis_carteira,sep="_")])/df_sim[1,paste("Custo",papeis_carteira,sep="_")],2)

# Preenchendo a concentração no papel dentro da carteira
df_sim[1,paste("Concentracao",papeis_carteira,sep="_")] <- round(df_sim[1,paste("ValorMercado",papeis_carteira,sep="_")]/sum(df_sim[1,grep("ValorMercado",names(df_sim),value=T)]),2)

# Preenchendo o montante de proventos recebidos para o dado papel no dado mês
df_sim[1,paste("Provent",papeis_carteira,sep="_")] <- 0


df_sim$Montante_Prove <- 0
df_sim[1,"Saldo_Restante"] <- aporte_inicial - sum(df_sim[grep("Custo",names(df_sim),value=T)])
df_sim$Valor_Carteira[1] <- sum(df_sim[1,paste("ValorMercado",papeis_carteira,sep="_")])
df_sim$Custo_Carteira[1] <- sum(df_sim[1,paste("Custo",papeis_carteira,sep="_")])

# head(df_sim)

# sum(df_sim[1,grep("Concentracao",names(df_sim),value=T)])







# Gerando os aportes mensais
for(i in seq(2,nrow(df_sim))){
  
  
  # Criando os vetores auxiliares
  vec_prev_qtde <- df_sim[i-1,paste("Qtde",papeis_carteira,sep="_")]
  vec_valo_prov <- rep(0,TAM); names(vec_valo_prov) <- paste("Proven",papeis_carteira,sep="_")
  vec_valo_apor <- rep(0,TAM); names(vec_valo_apor) <- paste("Aporte",papeis_carteira,sep="_")
  
  
  
  ############################################################
  # Função de desdobramentos, grupamentos e bonificações 
  ############################################################ 
  vec_prev_qtde <- desdobramentos(vec_prev_qtde,papeis_carteira,df_sim$Data[i],df_sim$Data[i-1])
  
  ############################################################
  # Função de proventos
  ############################################################   
  vec_valo_prov <- desdobramentos(vec_valo_prov,papeis_carteira,df_sim$Data[i],df_sim$Data[i-1])
  
  ############################################################
  # Função de aportes
  ############################################################   
  
  # Calculando o valor atual
  valor_atual <- round(vec_prev_qtde*df_sim[i,papeis_carteira],2)
  # Calculando o custo atual
  custo_atual <- df_sim[i-1,paste("Custo",papeis_carteira,sep="_")]
  # Calculando a Valorização atual
  valorizacao_atual <- round((valor_atual-custo_atual)/custo_atual,4)
  # Calculando a distribuição atual
  distrib_atual <- round(valor_atual/sum(valor_atual),2)
  
  
  # Chamando a função para calcular quanto aportar em cada ação
  vec_valo_apor <- aportes(vec_valo_apor,vec_valo_prov,papeis_carteira,valorizacao_atual,distrib_atual,aporte_mensal,df_sim$Saldo_Restante[i-1],vec_pesos) 
  

  ###########################################################
  # Comprando ações
  ########################################################### 
  
  # Calculando quantidade de papéis adquiridos
  pap_adq <- floor(vec_valo_apor/df_sim[i,papeis_carteira])

    
  ###########################################################
  # Preenchendo as informações do mês após as compras
  ########################################################### 
  
  # Preenchendo a quantidade de ações após compra
  df_sim[i,paste("Qtde",papeis_carteira,sep="_")] <- vec_prev_qtde + pap_adq
  
  # Preenchendo o custo
  df_sim[i,paste("Custo",papeis_carteira,sep="_")] <- df_sim[i-1,paste("Custo",papeis_carteira,sep="_")] + round(pap_adq*df_sim[i,papeis_carteira],2)
  
  # Preenchendo o PM
  df_sim[i,paste("PM",papeis_carteira,sep="_")] <- round(df_sim[i,paste("Custo",papeis_carteira,sep="_")]/df_sim[i,paste("Qtde",papeis_carteira,sep="_")],2)
  
  # Preenchendo o Valor de Mercado
  df_sim[i,paste("ValorMercado",papeis_carteira,sep="_")] <- round(df_sim[i,paste("Qtde",papeis_carteira,sep="_")]*df_sim[i,papeis_carteira],2)
  
  # Preenchendo a Valorização
  df_sim[i,paste("Valorizacao",papeis_carteira,sep="_")] <- round((df_sim[i,paste("ValorMercado",papeis_carteira,sep="_")]-df_sim[i,paste("Custo",papeis_carteira,sep="_")])/df_sim[i,paste("Custo",papeis_carteira,sep="_")],2)
  
  # Preenchendo a concentração no papel dentro da carteira
  df_sim[i,paste("Concentracao",papeis_carteira,sep="_")] <- round(df_sim[i,paste("ValorMercado",papeis_carteira,sep="_")]/sum(df_sim[i,grep("ValorMercado",names(df_sim),value=T)]),2)    
  
  # Preenchendo o montante de proventos recebidos para o dado papel no dado mês
  df_sim[i,paste("Provent",papeis_carteira,sep="_")] <- vec_valo_prov
  
  # Calculando o montante dos proventos
  df_sim$Montante_Prove[i] <- sum(vec_valo_prov)
  
  # Calculando o saldo restante
  df_sim$Saldo_Restante[i] <- (aporte_mensal + df_sim$Saldo_Restante[i-1] + sum(vec_valo_prov)) - sum(round(pap_adq*df_sim[i,papeis_carteira],2))
  
  # Valor consolidado da carteira
  df_sim$Valor_Carteira[i] <- sum(df_sim[i,paste("ValorMercado",papeis_carteira,sep="_")])
  
  # CUsto consolidado da carteira
  df_sim$Custo_Carteira[i] <- sum(df_sim[i,paste("Custo",papeis_carteira,sep="_")])

}

df_sim[format(df_sim$Data,"%Y")=="2016",c("Data",grep("SANB3",names(df_sim),value=T))]
df_sim[format(df_sim$Data,"%Y")=="2016",]

# 2014-06-23
# SANB3

# df_desd2

# head(df_sim,5)
# # plot(df_sim$Saldo_Restante)
plot(x=df_sim$Data,y=df_sim$Valor_Carteira,col="blue",type="l")
lines(x=df_sim$Data,y=df_sim$Custo_Carteira,col="red")
# 
# tail(df_sim)
# 
# plot(x=df_sim$Data,y=df_sim$Montante_Prove)
