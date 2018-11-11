#### FIltrando a tabela
df_cot <- df_cot[!is.na(df_cot$ITUB3) & df_cot$Data >= "2013-10-20",]

# a <- df_cot[df_cot$Data >= "2013-01-01",]

#### QUais ações não estavam presentes desde o início? 
acoes_inicio <- sapply(df_cot,function(x){ sum(!is.na(x))/length(x)})
acoes_inicio <- names(acoes_inicio[acoes_inicio>=1])


#### Mantendo apenas as ações que estavam listadas desde o início
df_cot <- df_cot[acoes_inicio]