#filtra propostas da modalidade Convênio ou Contrato de Repasse dos anos entre 2016 e 2022 - Brasil
propostasBrasil <-  
  filter(propostas, ano >= 2016 &  ano <= 2022 & 
           modalidade %in% c("CONVENIO", "CONTRATO DE REPASSE")
  ) 

propostasBrasil2 <-  
  filter(propostas, ano >= 2016 &  ano <= 2022 & 
           modalidade %in% c("CONVENIO", "CONTRATO DE REPASSE", "CONVENIO OU CONTRATO DE REPASSE" )
  ) 

convenios <- 
  filter(convenios, 
           !(situacao_convenio %in% c('Cancelado', 'Convênio Rescindido',
                                      'Convênio Anulado'))
  ) 

##junção de convênios com propostas pelo ID_PROPOSTA
convenio_proposta <- inner_join(convenios,propostasBrasil,by="id_proposta")
convenio_proposta2 <- inner_join(convenios,propostasBrasil2,by="id_proposta")

#pegar apenas os históricos das Propostas a serem analisados
hist_convenio_proposta <- semi_join(historico, convenio_proposta,by="id_proposta")

##obter data do cadastro da proposta
hist_convenio_proposta_cadastro <- filter(hist_convenio_proposta, situacao_historico=='PROPOSTA_CADASTRADA')

dt_cadastro_proposta <- hist_convenio_proposta_cadastro %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_cadastro_proposta <- rename(dt_cadastro_proposta, dt_cadastro_proposta = min)

##obter data início análise da proposta - obter 1a ocorrência
hist_convenio_proposta_inicio_analise <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PLANO_TRABALHO_EM_ANALISE', 'PROPOSTA_COMPLEMENTADA_EM_ANALISE', 'PROPOSTA_ENVIADA_ANALISE')) 

dt_inicio_analise <- hist_convenio_proposta_inicio_analise %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_inicio_analise <- rename(dt_inicio_analise, dt_inicio_analise = min)

##obter data aprovação PT - obter 1a ocorrência
hist_convenio_proposta_aprov_plano_trabalho <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PLANO_TRABALHO_APROVADO')) 

dt_aprovacao_plano <- hist_convenio_proposta_aprov_plano_trabalho %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aprovacao_plano <- rename(dt_aprovacao_plano, dt_aprovacao_plano = min)

##obter data previsão  Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_aguardando_pc <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('AGUARDANDO_PRESTACAO_CONTAS')) 

dt_aguardando_prestacao_contas <- hist_convenio_proposta_aguardando_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aguardando_prestacao_contas <- rename(dt_aguardando_prestacao_contas, dt_aguardando_prestacao_contas = min)

##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_envio_pc <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_ENVIADA_ANALISE')) 

dt_envio_prestacao_contas <- hist_convenio_proposta_envio_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_envio_prestacao_contas <- rename(dt_envio_prestacao_contas, dt_envio_prestacao_contas = min)


##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_analise_prestacao_contas <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_EM_ANALISE')) 

dt_analise_prestacao_contas <- hist_convenio_proposta_analise_prestacao_contas %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_analise_prestacao_contas <- rename(dt_analise_prestacao_contas, dt_analise_prestacao_contas = min)

##obter data fim Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_fim_prestacao_contas <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_APROVADA', 'PRESTACAO_CONTAS_APROVADA_COM_RESSALVAS' , 'PRESTACAO_CONTAS_REJEITADA', 'PRESTACAO_CONTAS_CONCLUIDA')) 

dt_fim_prestacao_contas <- hist_convenio_proposta_fim_prestacao_contas %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_fim_prestacao_contas <- rename(dt_fim_prestacao_contas, dt_fim_prestacao_contas = min)

#juntar todas as datas no dataframe convenio_proposta
convenio_proposta_juncao   <- 
  left_join(convenio_proposta,dt_cadastro_proposta,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_inicio_analise,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_aprovacao_plano,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_aguardando_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_envio_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_analise_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_fim_prestacao_contas,by="id_proposta") 

##Calcular o tempo em dias de cada etapa
converte_data = function (a) {
    return (strptime(a, "%d/%m/%Y %H:%M:%S", tz = "GMT"))
}


temp <- convenio_proposta_juncao %>%
  mutate(
    tempo_elaboracao_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_inicio_analise), converte_data(dt_cadastro_proposta), unit="days")))
    ,tempo_analise_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_aprovacao_plano), converte_data(dt_inicio_analise), unit="days")))
    ,tempo_prestacao_contas_enviado_analise_dias = as.numeric(round(difftime(converte_data(dt_envio_prestacao_contas), converte_data(dt_aguardando_prestacao_contas), unit="days")))
    ,tempo_prestacao_contas_em_analise_dias = as.numeric(round(difftime(converte_data(dt_fim_prestacao_contas), converte_data(dt_analise_prestacao_contas), unit="days")))
  )



#Análise do tempo_elaboracao_plano_trabalho_dias -> foi verificado que existem aprovações com a hora zerada  no memso dia da cadastro da proposta - nesse caso o tempo deve ser 0 ao invés de -1
#Análise do tempo_elaboracao_plano_trabalho_dias -> foram  verificadas 8 propostas com tempo menor do que 1. Ao analisar os dados foi verificado que a data de crição da proposta está posterior a data de análise da proposta 
                                                    #dados inconsistentes




#df2 <- na.omit(df)
#temp <- temp[!is.na(temp$tempo_elaboracao_plano_trabalho_dias),] 
temp$tempo_elaboracao_plano_trabalho_dias <- replace(temp$tempo_elaboracao_plano_trabalho_dias, temp$tempo_elaboracao_plano_trabalho_dias == -1 , 0)
temp$tempo_elaboracao_plano_trabalho_dias <- replace(temp$tempo_elaboracao_plano_trabalho_dias, temp$tempo_elaboracao_plano_trabalho_dias < -1 , NA)
tempo_positivo_elab_plano <- 
  filter(temp, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias)
mean(tempo_positivo_elab_plano$tempo_elaboracao_plano_trabalho_dias)
min(tempo_positivo_elab_plano$tempo_elaboracao_plano_trabalho_dias)
max(tempo_positivo_elab_plano$tempo_elaboracao_plano_trabalho_dias)
sd(tempo_positivo_elab_plano$tempo_elaboracao_plano_trabalho_dias)


#tempo médio por convenete
#totalConvenente <- distinct(tempo_positivo_elab_plano, convenente)
#totalConveniosConvenete <- tempo_positivo_elab_plano %>% count(convenente)

##Média por convenente
mediaElabPlanPorConvenente <- tempo_positivo_elab_plano %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_elaboracao_plano_trabalho_dias),
               list(mean =mean))

temp$tempo_analise_plano_trabalho_dias <- replace(temp$tempo_analise_plano_trabalho_dias, temp$tempo_analise_plano_trabalho_dias == -1 , 0)
temp$tempo_analise_plano_trabalho_dias <- replace(temp$tempo_analise_plano_trabalho_dias, temp$tempo_analise_plano_trabalho_dias < -1 , NA)
tempo_positivo_aprov_plano <- 
  filter(temp, tempo_analise_plano_trabalho_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_analise_plano_trabalho_dias)
mean(tempo_positivo_aprov_plano$tempo_analise_plano_trabalho_dias)
min(tempo_positivo_aprov_plano$tempo_analise_plano_trabalho_dias)
max(tempo_positivo_aprov_plano$tempo_analise_plano_trabalho_dias)


#tempo médio por concedente
#totalConvenente <- distinct(tempo_positivo_elab_plano, convenente)
#totalConveniosConvenete <- tempo_positivo_elab_plano %>% count(convenente)

##Média por concedente
mediaAprovPlanPorConcedente <- tempo_positivo_aprov_plano %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_analise_plano_trabalho_dias),
               list(mean =mean))


#retirado os tempos negativos - antecipação de PC ou inconsistência na base ???
temp$tempo_prestacao_contas_enviado_analise_dias <- replace(temp$tempo_prestacao_contas_enviado_analise_dias, temp$tempo_prestacao_contas_enviado_analise_dias == -1 , 0)
temp$tempo_prestacao_contas_enviado_analise_dias <- replace(temp$tempo_prestacao_contas_enviado_analise_dias, temp$tempo_prestacao_contas_enviado_analise_dias < -1 , NA)
tempo_positivo_envio_pc <- 
  filter(temp, tempo_prestacao_contas_enviado_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_enviado_analise_dias)
mean(tempo_positivo_envio_pc$tempo_prestacao_contas_enviado_analise_dias)
min(tempo_positivo_envio_pc$tempo_prestacao_contas_enviado_analise_dias)
max(tempo_positivo_envio_pc$tempo_prestacao_contas_enviado_analise_dias)

#tempo médio por convenete
#totalConvenente <- distinct(tempo_positivo_envio_pc, convenente)
#totalConveniosConvenente <- tempo_positivo_envio_pc %>% count(convenente)

##Média por convenente
mediaEnvioPCConvenente <- tempo_positivo_envio_pc %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_prestacao_contas_enviado_analise_dias),
               list(mean =mean))

#retirado os tempos negativos - inconsistência na base ???
temp$tempo_prestacao_contas_em_analise_dias <- replace(temp$tempo_prestacao_contas_em_analise_dias, temp$tempo_prestacao_contas_em_analise_dias == -1 , 0)
temp$tempo_prestacao_contas_em_analise_dias <- replace(temp$tempo_prestacao_contas_em_analise_dias, temp$tempo_prestacao_contas_em_analise_dias < -1 , NA)
tempo_positivo_fim_pc <- 
  filter(temp, tempo_prestacao_contas_em_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_em_analise_dias)
mean(tempo_positivo_fim_pc$tempo_prestacao_contas_em_analise_dias)
min(tempo_positivo_fim_pc$tempo_prestacao_contas_em_analise_dias)
max(tempo_positivo_fim_pc$tempo_prestacao_contas_em_analise_dias)

#tempo médio por concedente
#totalConvenente <- distinct(tempo_positivo_elab_plano, convenente)
#totalConveniosConvenete <- tempo_positivo_elab_plano %>% count(convenente)

##Média por concedente
mediaFimPC <- tempo_positivo_fim_pc %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_prestacao_contas_em_analise_dias),
               list(mean =mean))

# commad + shift + c
dados_transferegov <- temp %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio,
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas,
         tempo_elaboracao_plano_trabalho_dias, tempo_analise_plano_trabalho_dias, tempo_prestacao_contas_enviado_analise_dias, tempo_prestacao_contas_em_analise_dias)
# 
# 
 dados_transferegov_final <- dados_transferegov[order(dados_transferegov$ano,dados_transferegov$concedente, dados_transferegov$convenente ),]
# 
# #total de concedentes
# totalConcedentes <- distinct(dados_transferegov_final, concedente)
# 
# #total de convenios por orgao e ano
# totalConveniosOrgaoAno <- dados_transferegov_final %>% count(concedente, ano) 
# 
# #total de convenios por orgao
 totalConveniosOrgao <- dados_transferegov_final %>% count(concedente) 
 
 

 # #obter os 10 concedentes que repassaram maior volume financeiro 
 # dados_transferegov_final$valor_repasse_convenio = as.double(sub(",", ".", dados_transferegov_final$valor_repasse_convenio, fixed = TRUE))
 # 
 # somaRepasseConcedentes <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente), FUN=sum)
 # 
 # somaRepasseConcedentes <- somaRepasseConcedentes[order(somaRepasseConcedentes$x, decreasing=TRUE), ][1:10, ]
 # 
 # somaRepasseConcedentes$x <- as.character(somaRepasseConcedentes$x)
 # 
 # dados_transferegov_top_10_concedentes <- dados_transferegov_final %>% filter(concedente %in% c(somaRepasseConcedentes$concedente ))
 # 
 # #dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
 # #em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 
 # tempo_positivo_elab_plano_top10 <- 
 #   filter(dados_transferegov_top_10_concedentes, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
 #   select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
 #          dt_cadastro_proposta, dt_inicio_analise, 
 #          tempo_elaboracao_plano_trabalho_dias)
 # 
 # 
 # tempo_positivo_elab_plano <- 
 #   filter(temp, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
 #   select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
 #          dt_cadastro_proposta, dt_inicio_analise, 
 #          tempo_elaboracao_plano_trabalho_dias)
 # 
 
 #renomear x


# 
# #total de concedentes com mais de 100 convênios
 totalConveniosOrgaoMaior100 <-
 filter(totalConveniosOrgao, n > 100) 
# 
# #10 concedentes com maior número de con vênios
# dezMaioresRepassadores <- filter(totalConveniosOrgao, n > 1950) 
# 
# #calcular média dos tempos por concedente
# #colMeans(dados$V1) e mean(dados$V1)
# 
# concedenteMinCidades <- dados_transferegov_final %>% filter(concedente == 'MINISTERIO DAS CIDADES')
# concedenteMinCidadesMediaAprovPT <- dados_transferegov_final %>% filter(concedente == 'MINISTERIO DAS CIDADES')  %>% mean(dados_transferegov_final$tempo_analise_plano_trabalho_dias)

# situacoes <- distinct(convenio_proposta, situacao_convenio)
# 
# PC <-  
#   filter(convenio_proposta, ano >= 2016 &  ano <= 2022 & 
#            situacao_convenio %in% c('Prestação de Contas Aprovada',
#                                     'Prestação de Contas Aprovada com Ressalvas',
#                                     'Prestação de Contas Comprovada em Análise',
#                                     'Prestação de Contas Concluída',
#                                     'Prestação de Contas em Análise',
#                                     'Prestação de Contas em Complementação',
#                                     'Prestação de Contas enviada para Análise',
#                                     'Prestação de Contas Iniciada Por Antecipação',
#                                     'Prestação de Contas Rejeitada') 
#   ) 
