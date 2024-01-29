#obter os 10 concedentes que repassaram maior volume financeiro 
dados_transferegov_final$valor_repasse_convenio = as.double(sub(",", ".", dados_transferegov_final$valor_repasse_convenio, fixed = TRUE))


somaRepasseConcedentes <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente), FUN=sum)

somaRepasseConcedentes <- somaRepasseConcedentes[order(somaRepasseConcedentes$x, decreasing=TRUE), ][1:10, ]

somaRepasseConcedentes$x <- as.character(somaRepasseConcedentes$x)

dados_transferegov_top_10_concedentes <- dados_transferegov_final %>% filter(concedente %in% c(somaRepasseConcedentes$concedente ))

#próximos passos - tempo por modalidade
# somaModalidade <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente, modalidade=dados_transferegov_final$modalidade), FUN=sum)
# somaModalidadeOrder <- somaModalidade[order(somaModalidade$x , decreasing=TRUE), ]
# dados_transferegov_top_10_modalidade <- somaModalidadeOrder %>% filter(concedente %in% c(somaRepasseConcedentes$concedente ))
# dados_transferegov_top_10_modalidade <- dados_transferegov_top_10_modalidade[order(dados_transferegov_top_10_modalidade$concedente, dados_transferegov_top_10_modalidade$modalidade,  decreasing=TRUE), ]


#dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
#em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 
tempo_positivo_elab_plano_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias)


tempo_positivo_aprov_plano_top10  <- 
  filter(dados_transferegov_top_10_concedentes, tempo_analise_plano_trabalho_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_analise_plano_trabalho_dias)

tempo_positivo_envio_pc_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_prestacao_contas_enviado_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_enviado_analise_dias)

tempo_positivo_fim_pc_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_prestacao_contas_em_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_em_analise_dias)

##top 10 concedentes

#Tempo médio de apresentação do plano de trabalho (convenente) 
mediaElabPlanPorConvenente_top_10 <- tempo_positivo_elab_plano_top10 %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_elaboracao_plano_trabalho_dias),
               list(mean =mean))
mediaElabPlanPorConvenente_top_10 <- rename(mediaElabPlanPorConvenente_top_10, tempo_medio_elaboracao_plano = mean)

#Tempo médio  de análise do plano de trabalho  (mandatária e concedente)
mediaAprovPlanPorConcedente_top10 <- tempo_positivo_aprov_plano_top10 %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_analise_plano_trabalho_dias),
               list(mean =mean))
mediaAprovPlanPorConcedente_top10 <- rename(mediaAprovPlanPorConcedente_top10, tempo_medio_aprovacao_plano = mean)

#Tempo médio de prestação de contas para  enviado análise (convenente)
mediaEnvioPCConvenente_top10 <- tempo_positivo_envio_pc_top10 %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_prestacao_contas_enviado_analise_dias),
               list(mean =mean))
mediaEnvioPCConvenente_top10 <- rename(mediaEnvioPCConvenente_top10, tempo_medio_envio_pc = mean)

#Tempo médio  de prestação de contas em análise (concedente)
mediaFimPCConcedente_top10 <- tempo_positivo_fim_pc_top10 %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_prestacao_contas_em_analise_dias),
               list(mean =mean))
mediaFimPCConcedente_top10 <- rename(mediaFimPCConcedente_top10, tempo_medio_fim_pc = mean)

visaoMediaTop10Concedente   <- 
  left_join(somaRepasseConcedentes,mediaAprovPlanPorConcedente_top10,by="concedente") 

visaoMediaTop10Concedente   <- 
  left_join(visaoMediaTop10Concedente,mediaFimPCConcedente_top10,by="concedente") 

visaoMediaTop10Concedente <- rename(visaoMediaTop10Concedente, 'Valor Repasse Total' = x)
visaoMediaTop10Concedente <- rename(visaoMediaTop10Concedente, 'Tempo Médio de análise do plano de trabalho ' = tempo_medio_aprovacao_plano)
visaoMediaTop10Concedente <- rename(visaoMediaTop10Concedente, 'Tempo Médio de prestação de contas em análise' = tempo_medio_fim_pc)


# #total de convenios por convenente com mais de 50 propostas (dentro dos top 10 concedentes) - corresponde a 16.40% do total de convênios do top 10
totalConveniosConvenente <- dados_transferegov_top_10_concedentes %>% count(convenente) 
totalConveniosConvenente <- totalConveniosConvenente[order(totalConveniosConvenente$n, decreasing=TRUE), ][1:117, ]
dados_transferegov_top_100_convenentes <- dados_transferegov_top_10_concedentes %>% filter(convenente %in% c(totalConveniosConvenente$convenente ))

mediaElabPlanPorConvenente_top_10 <- mediaElabPlanPorConvenente_top_10  %>% filter(convenente %in% c(totalConveniosConvenente$convenente ))
mediaEnvioPCConvenente_top10 <- mediaEnvioPCConvenente_top10  %>% filter(convenente %in% c(totalConveniosConvenente$convenente ))

visaoMediaTop118Convenente   <- 
  left_join(totalConveniosConvenente,mediaElabPlanPorConvenente_top_10,by="convenente") 

visaoMediaTop118Convenente   <- 
  left_join(visaoMediaTop118Convenente,mediaEnvioPCConvenente_top10,by="convenente") 


visaoMediaTop118Convenente <- rename(visaoMediaTop118Convenente, 'Total Propostas' = n)
visaoMediaTop118Convenente <- rename(visaoMediaTop118Convenente, 'Tempo Médio de apresentação do plano de trabalho' = tempo_medio_elaboracao_plano)
visaoMediaTop118Convenente <- rename(visaoMediaTop118Convenente, 'Tempo Médio de envio da prestação de contas para análise' = tempo_medio_envio_pc)
