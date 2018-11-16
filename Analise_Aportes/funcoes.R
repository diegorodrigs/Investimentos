############################################################
# Normalizando
############################################################

vec = score[seq(estr_qtd_apo)]

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

proventos <- function(vec_vl_pr,papel,dt,prev_dt){
  
  # pg_prov <- df_prov2[df_prov2$Papel==papeis_carteira[colu] & df_prov2$DataPagamento <= df_sim$Data[i] & df_prov2$DataPagamento > df_sim$Data[i-1],]
  pg_prov <- df_prov2[df_prov2$DataPagamento <= dt & df_prov2$DataPagamento > prev_dt,]
  
  if(nrow(pg_prov)>0){
    
    for(j in seq(nrow(pg_prov))){
      
      # Definindo o papel
      colu <- which(papel == pg_prov$Papel[j])
      # Encontrando a data mais próxima da data-com
      inde <- max(which(df_sim$Data <= pg_prov$DataCom[j]))
      # Calculando a quantidade de papéis na data-com
      pg_prov$Qtde_Ppl[j] <- df_sim[inde,paste("Qtde",papeis_carteira[colu],sep="_")]       
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
  
  if(estr_rei_div=="S2"){
    valor_disp <- aporte_ms + sum(vec_vl_pr) + saldo_res
    vec_vl_pr <- vec_vl_pr*0
  }else if(estr_rei_div=="S1"){
    valor_disp <- aporte_ms + saldo_res
  }else if(estr_rei_div=="N1"){
    valor_disp <- aporte_ms + saldo_res
    vec_vl_pr <- vec_vl_pr*0    
  }
  
  
  ###################################################
  # Ordenando ações para aporte
  ###################################################  
  
  score <- pesos*normalizando(-valorizacao)*normalizando(- (distrib-(1/TAM)) )
  names(score) <- papel
  
  # score <- sort(score,decreasing = T)
  
  
  ###################################################
  # Definindo as ações para aporte
  ###################################################    
  
  lim_minimo <- min(sort(score,decreasing = T)[estr_qtd_apo])
  acoes_aporte <- escalonando(score*1*(score>=lim_minimo))
  
  
  ###################################################
  # Definindo vetor de aportes
  ###################################################  
  
  valor_aportes <- acoes_aporte*valor_disp + vec_vl_pr
  
  return(valor_aportes)
  
}
