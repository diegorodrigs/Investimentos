##############################################
# Carregando lista de UDFs e bibliotecas
##############################################
# source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")
source("C:/Users/Youse/Desktop/Projetos/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")
UDF_require("dplyr")

# setwd("C:/Users/Diego/Desktop/Data_Science/Investimentos")
setwd("C:/Users/Youse/Desktop/Outros/Investimentos/Investimentos")



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
TAM <- qtde_papeis
aporte_inicial <- 10000
aporte_mensal <- 2000
num_carteiras <- 200




##### Estratégias
    # Reinveste dividendos?
        # "N1": Não; 
        # "S1": Sim, no papel que a originou + Usa pesos para score
        # "S2": Sim, em qualquer papel + Usa pesos para score
        # "S3": Sim, em qualquer papel + Aporta igualmente em todas as ações
    estr_rei_div <- "S3" 
    # Quantidade de ações aportadas por mês
    estr_qtd_apo <- 1

    
##### Definindo o dia de aporte e criando as carteiras aleatórias

set.seed(249)
dia_util <- sample(seq(18),1) # 3
        
# Criando vetor com seeds
vec_seeds <- sample(seq(1000),num_carteiras)

df_carteiras <- data.frame(    matrix(rep(0,length(vec_seeds)*TAM),ncol = TAM,byrow = T)      )
names(df_carteiras) <- paste("Papel",seq(TAM),sep="")
head(df_carteiras)

for(h in seq(length(vec_seeds))){
  # Definindo a seed
  set.seed(vec_seeds[h])
  papeis_carteira <- sample(names(df_cot3[2:(ncol(df_cot3)-2)]),qtde_papeis) # "RADL3" "SBSP3" "SANB3" "PSSA3" "ITSA3"
  df_carteiras[h,] <- papeis_carteira
}

# row.names(df_carteiras) <- seq(nrow(df_carteiras))
# df_carteiras <- as.data.frame(df_carteiras,row.names = F)

# df_carteiras <- UDF_var_factor_to_char(df_carteiras)

df_carteiras$Valor <- 0
df_carteiras$Custo <- 0
df_carteiras$Valorizacao <- 0
df_carteiras$Pesos <- ""

# head(df_carteiras)

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

df_carteiras_total <- NULL
df_sim_total <- NULL


if(estr_rei_div != "S3"){
  vec_valor_peso <- seq(0,1,0.1)
}else{
  vec_valor_peso <- 1
}

for(p in vec_valor_peso){
  
  ##### Pesos para decidir aporte
  vec_pesos = c("Valorizacao"= p,"Distrib"= 1-p)
  
    for(t in seq(nrow(df_carteiras))){
      
      print(paste("p = ",p,"; t = ",t,sep=""))
    
      # t <- 9
      
      ################################
      # Preparando as bases
      ################################
      
      # Definindo os papeis presentes na carteira da iteração
      papeis_carteira <- as.matrix(df_carteiras[t,seq(TAM)])
      
      df_cot4 <- df_cot3[df_cot3$DiaUtil==dia_util,c("Data",papeis_carteira)]
      df_prov2 <- df_prov[df_prov$Papel %in% papeis_carteira,]
      df_desd2 <- df_desd[df_desd$Papel %in% papeis_carteira,]
    
      # head(df_cot4)
      # head(df_prov2)  
      # head(df_desd2)  
      
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
      
      
      # Gerando o aporte inicial por ação
      apo_ini_por_ppl <- aporte_inicial/TAM
      
      
      
      ##############################################
      # Preenchendo os dados do primeiro mês
      ##############################################
      
      df_sim <- primeiro_aporte(df_sim,apo_ini_por_ppl,papeis_carteira)
      # (df,apo_por_ppl,papel)
      # head(df_sim)
      
      
      ##############################################
      # Preenchendo os dados dos meses seguintes
      ##############################################
      
      df_sim <- demais_aportes(df_sim,papeis_carteira,aporte_mensal,vec_pesos)
      
    
      ##################################################
      # Preenchendo os dados da tabela com carteiras
      ##################################################
      
      df_carteiras$Valor[t] <- df_sim$Valor_Carteira[nrow(df_sim)]
      df_carteiras$Custo[t] <- df_sim$Custo_Carteira[nrow(df_sim)]
      df_carteiras$Valorizacao[t] <- df_sim$Valorizacao_Carteira[nrow(df_sim)]
      df_carteiras$Pesos[t] <- paste(names(vec_pesos),vec_pesos,sep=" = ",collapse = ", ")  
      
    }
  
  df_carteiras_total <- rbind(df_carteiras_total,df_carteiras)
  df_sim_total <- rbind(df_sim_total,df_sim)
  
}


##############################################
# Salvando a base de saída
##############################################

# # Reinveste dividendos?
# estr_rei_div <- "S2" # "N1": Não; "S1": Sim, no papel que a originou; "S2": Sim, em qualquer papel
# # Quantidade de ações aportadas por mês
# estr_qtd_apo <- 1
save(df_carteiras_total,df_sim_total,file=paste("./Analise_Aportes/Output/ediv_",estr_rei_div,"_eqtd_",estr_qtd_apo,".RData",sep=""))



##############################################
# Fazendo uma análise rápida
##############################################
boxplot(df_carteiras_total$Valorizacao ~ df_carteiras_total$Pesos,
        main = "Boxplot da Valorização por cenário de pesos")


as.data.frame(do.call(rbind,
                      tapply(df_carteiras_total$Valorizacao,df_carteiras_total$Pesos, 
                                   function(x){c("Média"=mean(x),"Mediana"=median(x))}
                             )
                      )
              )

hist(df_carteiras_total$Valorizacao)
range(df_carteiras_total$Valorizacao)
df_carteiras_total[c(which.min(df_carteiras_total$Valorizacao),which.max(df_carteiras_total$Valorizacao)),]


# head(df_carteiras_total)
# tail(df_carteiras_total)
# df_carteiras_total
# df_sim_total
# 
# vec_pesos
# names(vec_pesos)
# 
# 
# # tail(df_sim)
# head(df_carteiras)
# 
# boxplot(df_carteiras$Valorizacao)


# sum(df_sim$Montante_Prove)
# sum(df_sim$Provent_MULT3)
# 
# df_sim[grep("Concentr",names(df_sim),value=T)]





# boxplot(df_sim$Valorizacao_Carteira)

# head(df_sim[1:10])
# 
# df_sim[format(df_sim$Data,"%Y")=="2017",c("Data",grep("RADL3",names(df_sim),value=T))]
# df_sim[format(df_sim$Data,"%Y")=="2016",]

# 2014-06-23
# SANB3

# df_desd2

# head(df_sim,5)
# # plot(df_sim$Saldo_Restante)
plot(x=df_sim$Data,y=df_sim$Valor_Carteira,col="blue",type="l")
lines(x=df_sim$Data,y=df_sim$Custo_Carteira,col="red")

# df_sim$Valorizacao_Carteira



# write.table(df_sim[c("Data","Valor_Carteira","Custo_Carteira",grep("Valorizacao",names(df_sim),value=T))],
#             file="./Analise_Aportes/teste.csv",
#             sep=";",row.names = F)

# 
# tail(df_sim)
# 
# plot(x=df_sim$Data,y=df_sim$Montante_Prove)
