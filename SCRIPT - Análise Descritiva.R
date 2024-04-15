
# tempo_elaboracao_plano_trabalho_dias
(médias = tapply(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, tempo_positivo_elab_plano_top10_31$ano, mean))

# tempo_analise_plano_trabalho_dias
(médias = tapply(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, tempo_positivo_aprov_plano_top10$ano, mean))


#tempo_prestacao_contas_enviado_analise_dias
(médias = tapply(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, tempo_positivo_envio_pc_top10_31$ano, mean))

#tempo_prestacao_contas_em_analise_dias
(médias = tapply(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, tempo_positivo_fim_pc_top10$ano, mean))

#para tabelas de apoio - CONCEDENTES
#MS
conc <- tempo_positivo_aprov_plano_top10 %>% filter(sigla == 'MS')
(médias = tapply(conc$tempo_analise_plano_trabalho_dias, conc$ano, mean))

#MD
conc <- tempo_positivo_fim_pc_top10 %>% filter(sigla == 'MD')
(médias = tapply(conc$tempo_prestacao_contas_em_analise_dias, conc$ano, mean))

#MCID
conc <- tempo_positivo_fim_pc_top10 %>% filter(sigla == 'MCID')
(médias = tapply(conc$tempo_prestacao_contas_em_analise_dias, conc$ano, mean))


#para tabelas de apoio - CONVENENTES
# Rio de Janeiro
conv <- tempo_positivo_elab_plano_top10_31 %>% filter(convenente == 'RIO DE JANEIRO')
(médias = tapply(conv$tempo_elaboracao_plano_trabalho_dias, conv$ano, mean))

#SP
conv <- tempo_positivo_elab_plano_top10_31 %>% filter(convenente == 'SAO PAULO')
(médias = tapply(conv$tempo_elaboracao_plano_trabalho_dias, conv$ano, mean))

#BH
conv <- tempo_positivo_elab_plano_top10_31 %>% filter(convenente == 'BELO HORIZONTE')
(médias = tapply(conv$tempo_elaboracao_plano_trabalho_dias, conv$ano, mean))

#SAO LUS
conv <- tempo_positivo_envio_pc_top10_31 %>% filter(convenente == 'SAO LUIS')
(médias = tapply(conv$tempo_prestacao_contas_enviado_analise_dias, conv$ano, mean))

#JOAO PESSOA
conv <- tempo_positivo_envio_pc_top10_31 %>% filter(convenente == 'JOAO PESSOA')
(médias = tapply(conv$tempo_prestacao_contas_enviado_analise_dias, conv$ano, mean))

#SALVADOR
conv <- tempo_positivo_envio_pc_top10_31 %>% filter(convenente == 'SALVADOR')
(médias = tapply(conv$tempo_prestacao_contas_enviado_analise_dias, conv$ano, mean))
