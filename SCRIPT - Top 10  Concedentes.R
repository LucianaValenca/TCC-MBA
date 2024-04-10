#obter os 10 concedentes que repassaram maior volume financeiro 
dados_transferegov_final$valor_repasse_convenio = as.double(sub(",", ".", dados_transferegov_final$valor_repasse_convênio, fixed = TRUE))

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


#dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
#em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 
# Aqui foram retirados os NA
##ANALISE PLANO INICIO
tempo_positivo_aprov_plano_top10  <- 
  filter(dados_transferegov_top_10_concedentes, tempo_analise_plano_trabalho_dias >= 1) %>%
  select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_analise_plano_trabalho_dias, sigla)



#ANALISE PC
tempo_positivo_fim_pc_top10 <- 
  filter(dados_transferegov_top_10_concedentes, tempo_prestacao_contas_em_analise_dias >= 1) %>%
  select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, 
         dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_em_analise_dias, sigla, data_limite_pc)

totalConveniosOrgaoAcumulado_top10 <- dados_transferegov_top_10_concedentes %>% count(concedente, sigla) 


##top 10 CONCEDENTES
