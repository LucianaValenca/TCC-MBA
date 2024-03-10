# tempo_elaboracao_plano_trabalho_dias
# CODEVASF-COMPANHIA DE DESENV. DO VALE DO SAO FRANCISCO
# MD - MINISTERIO DA DEFESA -   média subiu
# MJSP-MINISTERIO DA JUSTICA E SEGURANCA PUBLICA - média subiu
# MS - MINISTERIO DA SAUDE ???
# MCID-MINISTERIO DAS CIDADES 
# MAPA-MINISTÉRIO DA AGRICULTURA E PECUÁRIA
# MDR-MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL
# FNS- FUNDACAO NACIONAL DE SAUDE 
# MESP-MINISTERIO DO  - SUBIU
# MTUR-MINISTERIO DO TURISMO

#DEFESA
graficoConv <- tempo_positivo_elab_plano_top10_31 %>% 
  filter(sigla == "MD") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

graficoConv


#MJSP
graficoConv <- tempo_positivo_elab_plano_top10_31 %>% 
  filter(sigla == "MJSP") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

graficoConv

#MESP
graficoConv <- tempo_positivo_elab_plano_top10_31 %>% 
  filter(sigla == "MESP") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

graficoConv







# tempo_prestacao_contas_enviado_analise_dias
# CODEVASF-COMPANHIA DE DESENV. DO VALE DO SAO FRANCISCO
# MD - MINISTERIO DA DEFESA
# MJSP-MINISTERIO DA JUSTICA E SEGURANCA PUBLICA
# MS - MINISTERIO DA SAUDE
# MCID-MINISTERIO DAS CIDADES
# MAPA-MINISTÉRIO DA AGRICULTURA E PECUÁRIA 
# MDR-MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL
# FNS- FUNDACAO NACIONAL DE SAUD
# MESP-MINISTERIO DO ESPORTE - MESP
# MTUR-MINISTERIO DO TURISMO

#ATRASO EM DIAS


#MESP
graficoConv <- tempo_positivo_envio_pc_top10_31 %>% 
  filter(sigla == "MESP") %>%  
  group_by(ano) %>% 
  summarise(media = mean(atraso_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

graficoConv











