#dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
#em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 
##ANALISE ELABORACAO PLANO

listaConvenetes <- c('SAO PAULO',
                     'BELO HORIZONTE',
                     'SALVADOR',
                     'RECIFE',
                     'CURITIBA',
                     'RIO BRANCO',
                     'MACAPA',
                     'TERESINA',
                     'GOIANIA',
                     'CAMPO GRANDE',
                     'PORTO ALEGRE',
                     'BRASILIA',
                     'NATAL',
                     'PORTO VELHO',
                     'VITORIA',
                     'BOA VISTA',
                     'JOAO PESSOA',
                     'RIO DE JANEIRO',
                     'CAMPINAS',
                     'ARACAJU',
                     'BELEM',
                     'CUIABA',
                     'LONDRINA',
                     'PALMAS',
                     'FLORIANOPOLIS',
                     'CACHOEIRO DE ITAPEMIRIM',
                     'MACEIO',
                     'MANAUS',
                     'CRUZEIRO DO SUL',
                     'SAO LUIS',
                     'FORTALEZA')


tempo_positivo_elab_plano_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias, sigla)

## Comparar com média
tempo_positivo_elab_plano_top10 <- tempo_positivo_elab_plano_top10 %>%
  mutate(
    excedeu_media = case_when(
      tempo_elaboracao_plano_trabalho_dias <= 39.66647 ~ 'N',
      tempo_elaboracao_plano_trabalho_dias > 39.66647 ~ 'S'
    )
  )

tempo_positivo_elab_plano_top10_31 <-
  filter(tempo_positivo_elab_plano_top10,  convenente %in% listaConvenetes) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias, excedeu_media, sigla)

tempo_positivo_elab_plano_top10_31_excedeu <- 
  filter(tempo_positivo_elab_plano_top10_31, excedeu_media == 'S') %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias, excedeu_media, sigla)

##ANALISE ELABORACAO PLANO FIM!

##ENVIO PC INICIO

tempo_positivo_envio_pc_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_prestacao_contas_enviado_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_enviado_analise_dias, atraso_dias, sigla, data_limite_pc)

## Comparar com média
tempo_positivo_envio_pc_top10 <- tempo_positivo_envio_pc_top10 %>%
  mutate(
    excedeu_media = case_when(
      tempo_prestacao_contas_enviado_analise_dias <= 111.0009 ~ 'N',
      tempo_prestacao_contas_enviado_analise_dias > 111.0009 ~ 'S'
    )
  )


tempo_positivo_envio_pc_top10_31 <-
  filter(tempo_positivo_envio_pc_top10,  convenente %in% listaConvenetes) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio,
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas,
         tempo_prestacao_contas_enviado_analise_dias, excedeu_media, atraso_dias, sigla, data_limite_pc)


tempo_positivo_envio_pc_top10_31_atrasou_excedeu <-
  filter(tempo_positivo_envio_pc_top10_31, excedeu_media == 'S' & atraso_dias > 0) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio,
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas,
         tempo_prestacao_contas_enviado_analise_dias, excedeu_media, atraso_dias, sigla, data_limite_pc)


tempo_positivo_envio_pc_top10_atrasou_excedeu_restante_convenente <- 
  filter(tempo_positivo_envio_pc_top10_31_atrasou_excedeu,  !(convenente %in% listaConvenetes) ) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_enviado_analise_dias, excedeu_media, atraso_dias, sigla, data_limite_pc)


#vou analisar os que tiveram atraso de mais de 180 dias
##ENVIO PC FIM!!


##top 31 CONVENENTES

#Tempo médio de apresentação do plano de trabalho (convenente) 
mediaElabPlanPorConvenente_top_10 <- tempo_positivo_elab_plano_top10 %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_elaboracao_plano_trabalho_dias),
               list(mean =mean))
mediaElabPlanPorConvenente_top_10 <- rename(mediaElabPlanPorConvenente_top_10, tempo_medio_elaboracao_plano = mean)


#Tempo médio de prestação de contas para  enviado análise (convenente)
mediaEnvioPCConvenente_top10 <- tempo_positivo_envio_pc_top10 %>%                                      
  group_by(convenente) %>%
  summarise_at(vars(tempo_prestacao_contas_enviado_analise_dias),
               list(mean =mean))
mediaEnvioPCConvenente_top10 <- rename(mediaEnvioPCConvenente_top10, tempo_medio_envio_pc = mean)



# #total de convenios por convenente com mais de 50 propostas (dentro dos top 10 concedentes) - corresponde a 16.40% do total de convênios do top 10
totalConveniosConvenente <- dados_transferegov_top_10_concedentes %>% count(convenente) 
totalConveniosConvenente31 <- totalConveniosConvenente[order(totalConveniosConvenente$n, decreasing=TRUE), ][1:31, ]
#dados_transferegov_top_100_convenentes <- dados_transferegov_top_10_concedentes %>% filter(convenente %in% c(totalConveniosConvenente$convenente ))



#write.csv(totalConveniosConvenente, "31convenentes.csv", row.names = FALSE)
#totalConveniosConvenente31 <- totalConveniosConvenente[order(totalConveniosConvenente$n, decreasing=TRUE), ][1:31, ]

mediaElabPlanPorConvenente_top_10 <- mediaElabPlanPorConvenente_top_10  %>% filter(convenente %in% c(totalConveniosConvenente31$convenente ))
mediaEnvioPCConvenente_top10 <- mediaEnvioPCConvenente_top10  %>% filter(convenente %in% c(totalConveniosConvenente31$convenente ))

visaoMediaTop31Convenente   <- 
  left_join(totalConveniosConvenente31,mediaElabPlanPorConvenente_top_10,by="convenente") 

visaoMediaTop31Convenente   <- 
  left_join(visaoMediaTop31Convenente,mediaEnvioPCConvenente_top10,by="convenente") 


visaoMediaTop31Convenente <- rename(visaoMediaTop31Convenente, 'Total Propostas' = n)
visaoMediaTop31Convenente <- rename(visaoMediaTop31Convenente, 'Tempo Médio de apresentação do plano de trabalho' = tempo_medio_elaboracao_plano)
visaoMediaTop31Convenente <- rename(visaoMediaTop31Convenente, 'Tempo Médio de envio da prestação de contas para análise' = tempo_medio_envio_pc)


temp <- convenio_proposta_juncao %>%
  mutate(
    tempo_elaboracao_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_inicio_analise), converte_data(dt_cadastro_proposta), unit="days")))
    ,tempo_analise_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_aprovacao_plano), converte_data(dt_inicio_analise), unit="days")))
    ,tempo_prestacao_contas_enviado_analise_dias = as.numeric(round(difftime(converte_data(dt_envio_prestacao_contas), converte_data(dt_aguardando_prestacao_contas), unit="days")))
    ,tempo_prestacao_contas_em_analise_dias = as.numeric(round(difftime(converte_data(dt_fim_prestacao_contas), converte_data(dt_analise_prestacao_contas), unit="days")))
  )




