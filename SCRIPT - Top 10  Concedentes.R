#obter os 10 concedentes que repassaram maior volume financeiro 
dados_transferegov_final$valor_repasse_convenio = as.double(sub(",", ".", dados_transferegov_final$valor_repasse_convenio, fixed = TRUE))

somaRepasseConcedentes <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente), FUN=sum)

somaRepasseConcedentes <- somaRepasseConcedentes[order(somaRepasseConcedentes$x, decreasing=TRUE), ][1:10, ]

somaRepasseConcedentes$x <- as.character(somaRepasseConcedentes$x)

dados_transferegov_top_10_concedentes <- dados_transferegov_final %>% filter(concedente %in% c(somaRepasseConcedentes$concedente ))

dados_transferegov_top_10_concedentes <- dados_transferegov_top_10_concedentes %>%
  mutate(
    sigla = case_when(
      concedente == "COMPANHIA DE DESENV. DO VALE DO SAO FRANCISCO" ~ "CODEVASF",
      concedente == "FUNDACAO NACIONAL DE SAUDE" ~ "FNS",
      concedente == "MINISTERIO DA DEFESA" ~ "MD",
      concedente == "MINISTERIO DA JUSTICA E SEGURANCA PUBLICA" ~ "MJSP",
      concedente == "MINISTERIO DA SAUDE" ~ "MS",
      concedente == "MINISTERIO DAS CIDADES" ~ "MCID",
      concedente == "MINISTERIO DO ESPORTE" ~ "MESP",
      concedente == "MINISTERIO DO TURISMO" ~ "MTUR",
      concedente == "MINISTÉRIO DA AGRICULTURA E PECUÁRIA" ~ "MAPA",
      concedente == "MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL" ~ "MDR"
    )
  )

dados_transferegov_top_10_concedentes <- dados_transferegov_top_10_concedentes %>%
  mutate(
    mandataria = case_when(
      modalidade == "CONVENIO" ~ "N",
      modalidade == "CONTRATO DE REPASSE" ~ "S"
    )
  )

#dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
#em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 

##ANALISE PLANO INICIO
tempo_positivo_aprov_plano_top10  <- 
  filter(dados_transferegov_top_10_concedentes, tempo_analise_plano_trabalho_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_analise_plano_trabalho_dias, sigla)

## Comparar com média
tempo_positivo_aprov_plano_top10 <- tempo_positivo_aprov_plano_top10 %>%
  mutate(
    excedeu_media = case_when(
      tempo_analise_plano_trabalho_dias <= 85.45967 ~ 'N',
      tempo_analise_plano_trabalho_dias > 85.45967 ~ 'S'
    )
  )

tempo_positivo_aprov_plano_top10_excedeu <- 
  filter(tempo_positivo_aprov_plano_top10, excedeu_media == 'S') %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_analise_plano_trabalho_dias, sigla, excedeu_media)



# INICIO ANALISE PC
tempo_positivo_fim_pc_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_prestacao_contas_em_analise_dias >= -1) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, 
         dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_em_analise_dias, sigla, data_limite_pc)

## Comparar com média
tempo_positivo_fim_pc_top10 <- tempo_positivo_fim_pc_top10 %>%
  mutate(
    excedeu_media = case_when(
      tempo_prestacao_contas_em_analise_dias <= 281.9773 ~ 'N',
      tempo_prestacao_contas_em_analise_dias > 281.9773 ~ 'S'
    )
  )

tempo_positivo_fim_pc_top10_excedeu <- 
  filter(tempo_positivo_fim_pc_top10, excedeu_media == 'S') %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_em_analise_dias, sigla, excedeu_media, data_limite_pc)

# INICIO ANALISE PC FIM!!!

##top 10 concedentes


#Tempo médio  de análise do plano de trabalho  (mandatária e concedente)
mediaAprovPlanPorConcedente_top10 <- tempo_positivo_aprov_plano_top10 %>%                                      
  group_by(concedente) %>%
  summarise_at(vars(tempo_analise_plano_trabalho_dias),
               list(mean =mean))
mediaAprovPlanPorConcedente_top10 <- rename(mediaAprovPlanPorConcedente_top10, tempo_medio_aprovacao_plano = mean)


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



# temp <- convenio_proposta_juncao %>%
#   mutate(
#     tempo_elaboracao_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_inicio_analise), converte_data(dt_cadastro_proposta), unit="days")))
#     ,tempo_analise_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_aprovacao_plano), converte_data(dt_inicio_analise), unit="days")))
#     ,tempo_prestacao_contas_enviado_analise_dias = as.numeric(round(difftime(converte_data(dt_envio_prestacao_contas), converte_data(dt_aguardando_prestacao_contas), unit="days")))
#     ,tempo_prestacao_contas_em_analise_dias = as.numeric(round(difftime(converte_data(dt_fim_prestacao_contas), converte_data(dt_analise_prestacao_contas), unit="days")))
#   )



totalConveniosOrgaoAno_top10 <- dados_transferegov_top_10_concedentes %>% count(concedente, sigla, ano) 


totalRepasseOrgaoAno_top10 <- dados_transferegov_top_10_concedentes %>%                                      
  group_by(concedente, sigla, ano) %>%
  summarise(total = mean(valor_repasse_convenio, na.rm = TRUE))





