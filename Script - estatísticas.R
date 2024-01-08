
#Total de propostas das modalidades Convênio ou contrato de repasse - 315725
count(propostasBrasil)

#Total de convênios - 90617 (desconsiderados convênhos nas seguintes situações 'Cancelado', 'Convênio Rescindido', 'Convênio Anulado')
count(convenio_proposta)

#Total de concedentes - 107
count(distinct(dados_transferegov, concedente))

#Total de convênios por órgãos - ano / orgao / valor total
totalConveniosOrgaoAno <- dados_transferegov %>% count(concedente, ano) 

#Tempo médio de apresentação do plano de trabalho (convenente) 
mediaElabPlanPorConvenente <- tempo_positivo_elab_plano %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_elaboracao_plano_trabalho_dias),
               list(mean =mean))

#Tempo médio  de análise do plano de trabalho  (mandatária e concedente)
mediaAprovPlanPorConcedente <- tempo_positivo_aprov_plano %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_analise_plano_trabalho_dias),
               list(mean =mean))

#Tempo médio de prestação de contas para  enviado análise (convenente)
mediaEnvioPCConvenente <- tempo_positivo_envio_pc %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_prestacao_contas_enviado_analise_dias),
               list(mean =mean))

#Tempo médio  de prestação de contas em análise (concedente)
mediaFimPC <- tempo_positivo_fim_pc %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_prestacao_contas_em_analise_dias),
               list(mean =mean))
