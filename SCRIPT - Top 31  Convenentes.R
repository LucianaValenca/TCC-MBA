#dos 90617 convênios 81527 são dos top 10 concedentes = 89,96%
#em decorrência desse percentual também serão selecionados os principais convenentes desses concedentes 
##ANALISE ELABORACAO PLANO

convenentes <- c('SAO PAULO',
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

#ELABORACAO_PLANO
tempo_positivo_elab_plano_top10_31 <-
  filter(dados_transferegov_top_10_concedentes,  convenente %in% convenentes, tempo_elaboracao_plano_trabalho_dias >= 1) %>%
  select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias, sigla)




##ENVIO PC INICIO
tempo_positivo_envio_pc_top10_31 <- 
  filter(dados_transferegov_top_10_concedentes,  convenente %in% convenentes, tempo_prestacao_contas_enviado_analise_dias >= 1) %>%
  select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
        dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_prestacao_contas_enviado_analise_dias, atraso_dias, sigla, data_limite_pc)




##top 31 CONVENENTES
