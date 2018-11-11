source("C:/Users/Diego/Desktop/Data_Science/UDF/UDF_list.R")

UDF_require("reshape2")
UDF_require("RSelenium")
UDF_require("rvest")
UDF_require("xml2")
UDF_require("tidyverse")



##################################
# Iniciar a conexÃ£o
##################################
rD <- rsDriver(port = 4444L, browser = "chrome")
remDr <- rD$client 

# Definindo a URL para o robÃ´
url_FU <- "https://www.fundamentus.com.br/buscaavancada.php"



##################################
# Web Scrapper - MEUS DIVIDENDOS
##################################

#### Carregando a pÃ¡gina
remDr$navigate(url_FU)

#### Capturando o botÃ£o de login
buscar_button <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[2]/form/input")

#### Clicando no botÃ£o de login
buscar_button$clickElement()


#### Tabela de indicadores
tab_indic <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='resultado']") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()





###################################
# Convertendo vars para numérico
###################################

# head(tab_indic)

# tab_indic$P.L <- as.numeric(tab_indic$P.L)

vari <- c("Cotação", "P.L", "P.VP", "PSR", "Div.Yield", "P.Ativo", 
          "P.Cap.Giro", "P.EBIT", "P.Ativ.Circ.Liq", "EV.EBIT", "Mrg.Ebit", 
          "Mrg..Líq.", "Liq..Corr.", "ROIC", "ROE", "Liq.2meses", "Patrim..Líq", 
          "Dív.Brut..Patrim.", "Cresc..Rec.5a")


tab_conv <- tab_indic

for(i in vari){

  a1 <- gsub(",","\\.", gsub("\\.","", tab_indic[,i] ))
  
  if(any(grepl("\\%",a1))){
    a2 <- as.numeric(gsub("%","",a1))/100
  }else{
    a2 <- as.numeric(a1)
  }
  
  tab_conv[,i] <- a2
  
}

# UDF_var_domain(tab_conv)

# head(tab_conv)




#################################################################################################################

###################################
#### Filtrando a base
###################################

tab_filt <- tab_conv[tab_conv$P.L > 0.01 & tab_conv$Patrim..Líq >= 1e+09 & tab_conv$Patrim..Líq <= 3e+09 & tab_conv$Liq.2meses > 5e+05 & tab_conv$Cresc..Rec.5a > 0,]
# head(tab_filt)

tab_filt[order(tab_filt$Div.Yield,decreasing = T),]


##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################



# Definindo a URL para o robÃ´
# url_VG <- "https://vicenteguimaraes.penserico.com/dashboard/cp.pr?e=TPIS3"

url_BI <- "https://www.bussoladoinvestidor.com.br/guia-empresas/empresa/PETR4/indicadores"


##################################
# INDICADORES
##################################

#### Carregando a pÃ¡gina
remDr$navigate(url_BI)

#### Capturando tabela de preços
tabela_precos <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='indicators']/div/div[1]/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()

#### Capturando tabela qtde ações
tabela_qtde_acoes <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='indicators']/div/div[2]/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()

#### Capturando tabela de preços
tabela_precos <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='indicators']/div/div[1]/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()

#### Capturando tabela de preços
tabela_precos <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='indicators']/div/div[1]/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()






##################################
# BALANÇO
##################################

#### Definindo a url
url_BI_bal <- "https://www.bussoladoinvestidor.com.br/guia-empresas/empresa/qual3/balanco"

#### Carregando a pÃ¡gina
remDr$navigate(url_BI_bal)

#### Lista de anos disponíveis
vec_ano <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='balance']/div[1]/div/div[1]/select/option") %>%
  rvest::html_attr(name = "label") %>%
  as.numeric()
# as.data.frame()


# Selecionando o ano desejado
lista_ano <- remDr$findElement(using = "xpath", "//*[@id='balance']/div[1]/div/div[1]/select/option[@value = 'number:2015']")
lista_ano$clickElement()

# Capturando a informação
tab_balanco <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(xpath = "//*[@id='balance']/div[2]/table") %>%
  rvest::html_table(fill = TRUE) %>%
  as.data.frame()  %>%
  t() %>%  
  as.data.frame()  

a <- as.data.frame(sapply(tab_balanco,as.character))
a
names(a) <- as.character(a[1,])
class(tab_balanco)


#### Clicando no botÃ£o de login
buscar_button$clickElement()


#### Tabela de indicadores






