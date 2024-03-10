# CODEVASF-COMPANHIA DE DESENV. DO VALE DO SAO FRANCISCO
# MD - MINISTERIO DA DEFESA  média subiu AQUI
# MJSP-MINISTERIO DA JUSTICA E SEGURANCA PUBLICA
# MS - MINISTERIO DA SAUDE
# MCID-MINISTERIO DAS CIDADES
# MAPA-MINISTÉRIO DA AGRICULTURA E PECUÁRIA
# MDR-MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL
# FNS- FUNDACAO NACIONAL DE SAUDE -   média subiu AQUI
# MESP-MINISTERIO DO ESPORTE
# MTUR-MINISTERIO DO TURISMO - média subiu AQUI


# tempo_analise_plano_trabalho_dias
# MD - MINISTERIO DA DEFESA
# MDR-MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL
# FNS- FUNDACAO NACIONAL DE SAUDE
# MESP-MINISTERIO DO ESPORTE


#linhas
g1 <- tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == "MD") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  ggtitle ('Convenente - Ministério da Defesa')+
  labs(x = "Ano",
          y = "Tempo Médio Aporvação Plano de Trabalho (em dias)")+
  geom_line()


g1

g2 <- tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == "MDR") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  ggtitle ('Convenente - Ministério do Desenvolvimento Regional')+
  labs(x = "Ano",
       y = "Tempo Médio Aporvação Plano de Trabalho (em dias)")+
  geom_line()


g2

g3 <- tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == "FNS") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  ggtitle ('Convenente - Fundo Nacional de Saúde')+
  labs(x = "Ano",
       y = "Tempo Médio Aporvação Plano de Trabalho (em dias)")+
  geom_line()


g3

g4 <- tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == "MESP") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  ggtitle ('Convenente - Ministério dps Esportes')+
  labs(x = "Ano",
       y = "Tempo Médio Aporvação Plano de Trabalho (em dias)")+
  geom_line()


g4

tempo_positivo_aprov_plano_top10_excedeu %>% 
  filter(sigla %in% c('MAPA','MCID')) %>%  
ggplot(aes(x = tempo_analise_plano_trabalho_dias,fill = sigla)) +
  geom_histogram(color = "black", binwidth = 50)+
  scale_x_continuous(breaks = seq(from = 0,to = 400,by = 100), limits = c(0,400)) +
  facet_grid(sigla ~ .) +
  labs(y = 'Frequência') +
  scale_fill_manual(values=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3" ,"#FDB462" ,"#B3DE69", "#FCCDE5" ,"#D9D9D9" ,"#BC80BD")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = 'top',
        axis.line = element_line(colour = "black"))


# tempo_prestacao_contas_em_analise_dias
# FNS- FUNDACAO NACIONAL DE SAUDE -   média subiu AQUI
# MESP-MINISTERIO DO ESPORTE - média subiu AQUI
# MTUR-MINISTERIO DO TURISMO - média subiu AQUI


#linhas
g5 <- tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == "FNS") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

g5

g6 <- tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == "MESP") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

g6

g7 <- tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == "MTUR") %>%  
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_line()

g7


tempo_positivo_fim_pc_top10_excedeu %>% 
  filter(sigla %in% c('MAPA','MCID', 'MD', 'MS')) %>%  
ggplot(aes(x = tempo_prestacao_contas_em_analise_dias,fill = sigla)) +
  geom_histogram(color = "black", binwidth = 50)+
  scale_x_continuous(breaks = seq(from = 300,to = 500,by = 100), limits = c(300,500)) +
  facet_grid(sigla ~ .) +
  labs(y = 'Frequência') +
  scale_fill_manual(values=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = 'top',
        axis.line = element_line(colour = "black"))


