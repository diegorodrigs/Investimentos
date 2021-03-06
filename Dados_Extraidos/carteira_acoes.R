source("C:/Users/Youse/Desktop/Projetos/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")



##################################
# Iniciar a conexão
##################################
rD <- rsDriver(port = 4444L, browser = "chrome")
remDr <- rD$client 

# Definindo a URL para o robô
url_MD <- "https://www.meusdividendos.com/"
url_EM <- "https://publicacoes.empiricus.com.br/minhas-assinaturas/vacas-leiteiras/publicacao/o-que-comprar-vacas-leiteiras"


##################################
# Web Scrapper - MEUS DIVIDENDOS
##################################

#### Carregando a página
remDr$navigate(url_MD)

#### Capturando o botão de login
login_button <- remDr$findElement(using = "xpath", "//*[@id='main-wrapper']/header/nav/div[2]/ul/ul/li/a")

#### Clicando no botão de login
login_button$clickElement()

#### Capturando o botão de login do google
login_google_button <- remDr$findElement(using = "xpath", "//*[@id='titulo']/a[2]")

#### Clicando no botão de login
login_google_button$clickElement() 


#########################################################################################################################
#########################################################################################################################
#                                                   >>> FAZER LOGIN <<<                                                 #
#########################################################################################################################
#########################################################################################################################


#### Capturando o botão de portfólio
portfolio_button <- remDr$findElement(using = "xpath", "//*[@id='subnavbar']/ul/li[2]/a")        

#### Clicando no botão de portfólio
portfolio_button$clickElement() 

#### Tabela de ações
tab_md_acoes <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='portfolio']/div[2]/div[1]/div/div[2]/div/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()


#### Tabela de FIIs
tab_md_fiis <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='portfolio']/div[2]/div[2]/div/div[2]/div/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()


#### Tratando as colunas
tab_md_acoes <- tab_md_acoes[,c(-1,-11)]
names(tab_md_acoes) <- c("Codigo", "Qtd", "Preco_Atual", "Cotacao_52s", "VPA", "PM", 
                         "Valor_Atual", "Custo_Atual", "Ativo")
tab_md_acoes$Cotacao_Min_52s <- gsub("/.*","",tab_md_acoes$Cotacao_52s)
tab_md_acoes$Cotacao_Max_52s <- gsub(".*/","",tab_md_acoes$Cotacao_52s)
tab_md_acoes$Ticker <- gsub("(.+[0-9]+).*","\\1",tab_md_acoes$Codigo)
tab_md_acoes$Nome <- gsub("(.+[0-9]+)(.*)","\\2",tab_md_acoes$Codigo)

tab_md_acoes <- tab_md_acoes[setdiff(names(tab_md_acoes),c("Codigo","Cotacao_52s"))]

vars <- c("Preco_Atual", "Cotacao_Min_52s", "Cotacao_Max_52s","VPA", "PM", "Valor_Atual", "Custo_Atual", "Ativo")
tab_md_acoes[vars] <- sapply(tab_md_acoes[vars],function(x){
                            as.numeric(
                                      gsub("%","",gsub(",","\\.",gsub("\\.","",x)))
                                      )
                            })


tab_md_acoes$Ativo <- as.numeric(tab_md_acoes$Ativo)/100

tab_md_acoes



##################################
# Web Scrapper - EMPIRICUS
##################################

#### Carregando a página
remDr$navigate(url_EM)


#########################################################################################################################
#########################################################################################################################
#                                                   >>> FAZER LOGIN <<<                                                 #
#########################################################################################################################
#########################################################################################################################


#### Tabela de vacas
tab_vacas <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='app']/div[1]/div/div[2]/div/div/div[3]/div/div[2]/div/div/div/table[1]") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame() %>%
  mutate(Tipo = 'Vacas')


#### Tabela de bezerras
tab_bezerras <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='app']/div[1]/div/div[2]/div/div/div[3]/div/div[2]/div/div/div/table[2]") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame() %>%
  mutate(Tipo = 'Bezerra')
  

#### Empilhando as tabelas
tab_empi_acoes <- rbind(tab_vacas[-1,],tab_bezerras[-1,])
names(tab_empi_acoes) <- c("Entrada", "Ranking", "Ticker", "Preco_Atual_Emp", "Preco_Teto", 
                           "Periodicidade", "Tipo")

tab_empi_acoes <- tab_empi_acoes[setdiff(names(tab_empi_acoes),c("Entrada"))]


#### Tratando as colunas
tab_empi_acoes$Ticker <- gsub("\\*","",tab_empi_acoes$Ticker)
tab_empi_acoes$Preco_Atual_Emp <- as.numeric(gsub(",","\\.",gsub("R\\$ ","",tab_empi_acoes$Preco_Atual_Emp)))
tab_empi_acoes$Preco_Teto <- as.numeric(gsub(",","\\.",gsub("R\\$ ","",tab_empi_acoes$Preco_Teto)))
tab_empi_acoes$Ranking <- as.numeric(tab_empi_acoes$Ranking)

# names(tab_empi_acoes)[names(tab_empi_acoes)=="Ativo"] <- "Ticker"





##################################
# Manejando os dados
##################################

# Cruzando as bases
df <- merge(tab_md_acoes,tab_empi_acoes,by="Ticker",all=T)

# Ordenando com base no ranking
df <- df[order(df$Ranking),]


###########################
# Tratando missings
###########################


# Quantidade de papéis
df$Qtd <- ifelse(is.na(df$Qtd),0,df$Qtd)

# Preço Atual
df$Preco_Atual <- ifelse(is.na(df$Preco_Atual),df$Preco_Atual_Emp,df$Preco_Atual)

# PM
df$PM <- ifelse(is.na(df$PM),df$Preco_Atual,df$PM)

# Valor_Atual 
df$Valor_Atual <- ifelse(is.na(df$Valor_Atual),0,df$Valor_Atual)

# Custo_Atual
df$Custo_Atual <- ifelse(is.na(df$Custo_Atual),0,df$Custo_Atual)

# Ativo
df$Ativo <- ifelse(is.na(df$Ativo),0,df$Ativo)



###########################################################################


#### Calculando a distribuição
# % que uma ação deve ter a mais que a ação subsequente na ordem de prioridade
ratio <- 1.20
coefi <- log(ratio)

# Quantidade de ações
quant <- nrow(df)

# Distribuição DESEJADA para cada ação
distr <- exp(coefi*seq(quant))
df$distr_desejada <- rev(distr/sum(distr))

# Distribuição ATUAL para cada ação
df$distr_atual <- df$Valor_Atual/sum(df$Valor_Atual,na.rm = T)
# df




#### Ajustando a flag de Entrada
df$Flag_Entrada <- ifelse(df$Ticker %in% c("WIZS3","EZTC3") | df$Preco_Atual>df$Preco_Teto,"NÃO","SIM")
# df




#### Calculando o DY do preço atual
# Ativos vacas
pr_teto_dy_vacas <- 0.05

# Ativos Bezerras
pr_teto_dy_bezer <- 0.025

# Calculando o DY estimado com o preço atual baseado no preço teto

# O preço teto foi estipulado para obter um DY de 5% para 
# vacas e 2.5% para bezerras
df$DY_Pr_Atual <- ifelse(df$Tipo == "Vacas",pr_teto_dy_vacas*df$Preco_Teto/df$Preco_Atual,
                         ifelse(df$Tipo == "Bezerra",pr_teto_dy_bezer*df$Preco_Teto/df$Preco_Atual,NA))



#######################################################################################################

#### Calculando o nível de oportunidade

# O nível de oportunidade leva em consideração 3 pontos:
#     - distância para distribuição desejada
#     - quão descontada está a ação comparada ao prêmio médio
#     - DY estimado para a cotação atual (esta condição recebe maior peso)


# Normalização dos critérios
aux1 <- df$distr_desejada-df$distr_atual; dist_distrib <- (aux1-min(aux1,na.rm=T))/(max(aux1,na.rm=T)-min(aux1,na.rm=T))
aux2 <- 1-df$Ativo; dist_ativo <- (aux2-min(aux2,na.rm=T))/(max(aux2,na.rm=T)-min(aux2,na.rm=T))
aux3 <- df$DY_Pr_Atual^2; dist_dy <- (aux3-min(aux3,na.rm=T))/(max(aux3,na.rm=T)-min(aux3,na.rm=T))

# plot(aux1,dist_distrib)
# plot(df$Ticker,dist_ativo)
# plot(df$DY_Pr_Atual,dist_dy)

# Calculando o critério final
df$oport <- ifelse(df$Flag_Entrada=="SIM",dist_distrib*dist_ativo*dist_dy,0)
# df$oport <- ifelse(df$Flag_Entrada=="SIM",pmax(round((df$distr_desejada-df$distr_atual)*(df$Diff_Val_Perc)*(-10^4),0),0),-1)
# df
df$oport <- round(100*df$oport,0)

# Quantas ações faltam para atingir a distribuição desejada
df$quant_rest <- 
  ifelse(df$Flag_Entrada=="SIM",
         round(((df$distr_desejada*sum(df$Valor_Atual,na.rm=T) - df$Valor_Atual)/(1-df$distr_desejada))/df$Preco_Atual,2)
         ,0)

# df

##########################################################################################################

#### Preparando a base para mostrar
df_out <- df[order(df$oport,decreasing = T),c("Ticker", "Tipo","Flag_Entrada", "Ranking", "Preco_Atual", "Preco_Teto",
                                          "Ativo", "distr_atual", "distr_desejada","DY_Pr_Atual", "quant_rest",
                                          "oport")]
df_out






##################################
# Encerrando a conexão
##################################
remDr$close()
