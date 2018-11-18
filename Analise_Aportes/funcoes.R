############################################################
# Normalizando
############################################################

normalizando <- function(vec){

  mini <- min(vec)
  posit <- vec - mini
  normal <- round(posit/(max(posit)-min(posit)),4)

  return(normal)
      
}



############################################################
# Escalonando
############################################################


escalonando <- function(vec){
  
  escal <- vec/sum(vec)
  
  return(escal)
  
}




############################################################
# Função de desdobramentos, grupamentos e bonificações 
############################################################

desdobramentos <- function(vec_pr_qt,papel,dt,prev_dt){
  
  # desdob <- df_desd2[df_desd2$Papel==papeis_carteira[colu] & df_desd2$DataCom <= dt & df_desd2$DataCom > prev_dt,]
  desdob <- df_desd2[df_desd2$DataCom <= dt & df_desd2$DataCom > prev_dt,]
  
  if(nrow(desdob)>0){
    
    for(j in seq(nrow(desdob))){
      
      colu <- which(papel == desdob$Papel[j])
      
      if(desdob$Tipo[j]=="Bonificação"){
        vec_pr_qt[colu] <- round(vec_pr_qt[colu]*(1 + (desdob$Fator1[j]/100)),0)
      }
      
      if(desdob$Tipo[j] %in% c("Grupamento","Desdobramento")){
        vec_pr_qt[colu] <- round(vec_pr_qt[colu]*( desdob$Fator2[j]/desdob$Fator1[j]   ),0)
      }  
      
    }
    
  }
  
  return(vec_pr_qt)
  
}

# vec_prev_qtde
# a <- desdobramentos(vec_prev_qtde,df_sim$Data[i],df_sim$Data[i-1])
# a





###########################################################
# Verificando se houve pagamento de dividendos neste mês
########################################################### 

proventos <- function(df,vec_vl_pr,papel,dt,prev_dt){
  
  # pg_prov <- df_prov2[df_prov2$Papel %in% papeis_carteira,]
  pg_prov <- df_prov2[df_prov2$DataPagamento <= dt & df_prov2$DataPagamento > prev_dt,]
  
  if(nrow(pg_prov)>0){
    
    for(j in seq(nrow(pg_prov))){
      
      # Definindo o papel
      colu <- which(papel == pg_prov$Papel[j])
      # Encontrando a data mais próxima da data-com
      inde <- max(which(df$Data <= pg_prov$DataCom[j]))
      # Calculando a quantidade de papéis na data-com
      pg_prov$Qtde_Ppl[j] <- df[inde,paste("Qtde",papeis_carteira[colu],sep="_")]       
      # Calculando o montante de provento a ser recebido
      vec_vl_pr[colu] <- round(pg_prov$Qtde_Ppl[j]*pg_prov$Valor[j],2)
      
    }
  }
  
  return(vec_vl_pr)
  
}



###########################################################
# Definindo o valor dos aportes
########################################################### 

# vec_valo_apor <- aportes(vec_valo_apor,papeis_carteira,valorizacao_atual,distrib_atual,aporte_mensal)

aportes <- function(vec_vl_ap,vec_vl_pr,papel,valorizacao,distrib,aporte_ms,saldo_res,pesos){
  
  TAM = length(papel)
  
  ##### Estratégias
  # Reinveste dividendos?
  # estr_rei_div  "N1": Não; "S1": Sim, no papel que a originou; "S2": Sim, em qualquer papel
  # Quantidade de ações aportadas por mês
  # estr_qtd_apo <- 1
  
  ###################################################
  # Valor disponível para aportar em cada ação
  ###################################################
  
  if(estr_rei_div %in% c("S2","S3")){
    valor_disp <- aporte_ms + sum(vec_vl_pr) + saldo_res
    vec_vl_pr <- vec_vl_pr*0
  }else if(estr_rei_div=="S1"){
    valor_disp <- aporte_ms + saldo_res
  }else if(estr_rei_div=="N1"){
    valor_disp <- aporte_ms + saldo_res
    vec_vl_pr <- vec_vl_pr*0    
  }
  
  
  if(estr_rei_div != "S3"){
  
      ###################################################
      # Ordenando ações para aporte
      ###################################################  
      
      score <- pesos["Valorizacao"]*normalizando(-valorizacao) + pesos["Distrib"]*normalizando(- (distrib-(1/TAM)) )
      names(score) <- papel
      score <- normalizando(score)
      
      # score <- sort(score,decreasing = T)
      # print(score)
      
      
      ###################################################
      # Definindo as ações para aporte
      ###################################################    
      
      lim_minimo <- min(sort(score,decreasing = T)[estr_qtd_apo])
      acoes_aporte <- escalonando(score*1*(score>=lim_minimo))
      
  }else{
      acoes_aporte <- escalonando(rep(1,TAM))
  }
  
  ###################################################
  # Definindo vetor de aportes
  ###################################################  
  
  valor_aportes <- acoes_aporte*valor_disp + vec_vl_pr
  
  return(valor_aportes)
  
}



###########################################################
# Função para o primeiro aporte
########################################################### 

# df = df_sim
# apo_por_ppl = apo_ini_por_ppl
# papel = papeis_carteira

primeiro_aporte <- function(df,apo_por_ppl,papel){
  
  # Preenchendo a quantidade de ações
  df[1,paste("Qtde",papel,sep="_")] <- floor(apo_por_ppl/df[1,papel])
  
  # Preenchendo o custo
  df[1,paste("Custo",papel,sep="_")] <- round(df[1,paste("Qtde",papel,sep="_")]*df[1,papel],2)
  
  # Preenchendo o PM
  df[1,paste("PM",papel,sep="_")] <- df[1,papel]
  
  # Preenchendo o Valor de Mercado
  df[1,paste("ValorMercado",papel,sep="_")] <- df[1,paste("Custo",papel,sep="_")]
  
  # Preenchendo a Valorização
  df[1,paste("Valorizacao",papel,sep="_")] <- round((df[1,paste("ValorMercado",papel,sep="_")]-df[1,paste("Custo",papel,sep="_")])/df[1,paste("Custo",papel,sep="_")],2)
  
  # Preenchendo a concentração no papel dentro da carteira
  df[1,paste("Concentracao",papel,sep="_")] <- round(df[1,paste("ValorMercado",papel,sep="_")]/sum(df[1,grep("ValorMercado",names(df),value=T)]),2)
  
  # Preenchendo o montante de proventos recebidos para o dado papel no dado mês
  df[1,paste("Provent",papel,sep="_")] <- 0
  
  
  df$Montante_Prove <- 0
  df[1,"Saldo_Restante"] <- (apo_por_ppl*length(papel)) - sum(df[grep("Custo",names(df),value=T)])
  df$Valor_Carteira[1] <- sum(df[1,paste("ValorMercado",papel,sep="_")])
  df$Custo_Carteira[1] <- sum(df[1,paste("Custo",papel,sep="_")])
  
  return(df)
  
}




###########################################################
# Função para os aportes dos meses seguintes
########################################################### 

demais_aportes <- function(df,papel,apo_mensal,vec_pes){
  
  # Gerando os aportes mensais
  for(i in seq(2,nrow(df))){
    # for(i in seq(2,44)){
    
    # print(paste("i = ",i,sep=""))
    
    
    # Criando os vetores auxiliares
    vec_prev_qtde <- df[i-1,paste("Qtde",papel,sep="_")]
    # df[seq(1,i-1),paste("Qtde",papel,sep="_")]
    # grep("Qtde",names(df),value=T)
    vec_valo_prov <- rep(0,TAM); names(vec_valo_prov) <- paste("Proven",papel,sep="_")
    vec_valo_apor <- rep(0,TAM); names(vec_valo_apor) <- paste("Aporte",papel,sep="_")
    
    
    
    ############################################################
    # Função de desdobramentos, grupamentos e bonificações 
    ############################################################ 
    vec_prev_qtde <- desdobramentos(vec_prev_qtde,papel,df$Data[i],df$Data[i-1])
    
    ############################################################
    # Função de proventos
    ############################################################   
    vec_valo_prov <- proventos(df,vec_valo_prov,papel,df$Data[i],df$Data[i-1])
    
    ############################################################
    # Função de aportes
    ############################################################   
    
    # Calculando o valor atual
    valor_atual <- round(vec_prev_qtde*df[i,papel],2)
    # Calculando o custo atual
    custo_atual <- df[i-1,paste("Custo",papel,sep="_")]
    # Calculando a Valorização atual
    valorizacao_atual <- round((valor_atual-custo_atual)/custo_atual,4)
    # Calculando a distribuição atual
    distrib_atual <- round(valor_atual/sum(valor_atual),4)
    
    
    # Chamando a função para calcular quanto aportar em cada ação
    vec_valo_apor <- aportes(vec_valo_apor,vec_valo_prov,papel,valorizacao_atual,distrib_atual,apo_mensal,df$Saldo_Restante[i-1],vec_pes) 
    
    
    ###########################################################
    # Comprando ações
    ########################################################### 
    
    # Calculando quantidade de papéis adquiridos
    pap_adq <- floor(vec_valo_apor/df[i,papel])
    
    
    ###########################################################
    # Preenchendo as informações do mês após as compras
    ########################################################### 
    
    # Preenchendo a quantidade de ações após compra
    df[i,paste("Qtde",papel,sep="_")] <- vec_prev_qtde + pap_adq
    
    # Preenchendo o custo
    df[i,paste("Custo",papel,sep="_")] <- df[i-1,paste("Custo",papel,sep="_")] + round(pap_adq*df[i,papel],2)
    
    # Preenchendo o PM
    df[i,paste("PM",papel,sep="_")] <- round(df[i,paste("Custo",papel,sep="_")]/df[i,paste("Qtde",papel,sep="_")],2)
    
    # Preenchendo o Valor de Mercado
    df[i,paste("ValorMercado",papel,sep="_")] <- round(df[i,paste("Qtde",papel,sep="_")]*df[i,papel],2)
    
    # Preenchendo a Valorização
    df[i,paste("Valorizacao",papel,sep="_")] <- round((df[i,paste("ValorMercado",papel,sep="_")]-df[i,paste("Custo",papel,sep="_")])/df[i,paste("Custo",papel,sep="_")],2)
    
    # Preenchendo a concentração no papel dentro da carteira
    df[i,paste("Concentracao",papel,sep="_")] <- round(df[i,paste("ValorMercado",papel,sep="_")]/sum(df[i,grep("ValorMercado",names(df),value=T)]),2)    
    
    # Preenchendo o montante de proventos recebidos para o dado papel no dado mês
    df[i,paste("Provent",papel,sep="_")] <- vec_valo_prov
    
    # Calculando o montante dos proventos
    df$Montante_Prove[i] <- sum(vec_valo_prov)
    
    # Calculando o saldo restante
    df$Saldo_Restante[i] <- (apo_mensal + df$Saldo_Restante[i-1] + sum(vec_valo_prov)) - sum(round(pap_adq*df[i,papel],2))
    
    # Valor consolidado da carteira
    df$Valor_Carteira[i] <- sum(df[i,paste("ValorMercado",papel,sep="_")])
    
    # Custo consolidado da carteira
    df$Custo_Carteira[i] <- sum(df[i,paste("Custo",papel,sep="_")])
    
  }
  
  # Calculando a valorização da carteira
  df$Valorizacao_Carteira <- (df$Valor_Carteira - df$Custo_Carteira)/df$Custo_Carteira
  
  
  return(df)
  
}

