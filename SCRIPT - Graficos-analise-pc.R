##geral

Anos <- tempo_positivo_fim_pc_top10 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 294.43, colour="blue") +
  ggtitle ('Tempo Médio de Análise da Prestação de Contas - Anos de 2016 a 2022')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2018,  y=320, label="Média 294.43", size = 4) 
  


# Ano16 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2016) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 419,43, colour="blue") +
#   annotate("text", x="FNS",  y=475, label="Média 419,43", size = 4)+
#    ggtitle ('Gráfico 2 - Ano 2016 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano17 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2017) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 363,86, colour="blue") +
#   annotate("text", x="FNS",  y=415, label="    Média 363,86",size = 4)+
#   ggtitle ('Gráfico 3 - Ano 2017 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano18 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2018) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 237.60, colour="blue") +
#   annotate("text", x="FNS",  y=265, label="  Média 237.60" ,size = 4)+
#   ggtitle ('Gráfico 4 - Ano 2018 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# 
# Ano19 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2019) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 173.13, colour="blue") +
#   annotate("text", x="FNS",  y=195, label="Média 173.13", size = 4)+
#   ggtitle ('Gráfico 5 - Ano 2019 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano20 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2020) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 98.97, colour="blue") +
#   annotate("text", x="FNS",  y=115, label="Média 98.97", size = 4)+
#   ggtitle ('Gráfico 6 -Ano 2020 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano21 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2021) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 	46.14, colour="blue") +
#   annotate("text", x="MAPA",  y=55, label="Média 46.14" ,size = 4)+
#   ggtitle ('Gráfico 7 - Ano 2021 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano22 <- tempo_positivo_fim_pc_top10 %>% 
#   filter(ano == 2022) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 	61.47, colour="blue") +
#   annotate("text", x="MAPA",  y=70, label="Média 	61.47", size = 4)+
#   ggtitle ('Gráfico 8 - Ano 2022 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# 	#Ano16 + Ano17 + Ano18 + Ano19 + Ano20 + Ano21 + Ano22  
# 
# ((
#   # gráfico
#   (# primeira coluna 
#     (Anos / Ano17 / Ano19 / Ano21) |
#       # segunda coluna
#       Ano16 / Ano18 / Ano20 / Ano22) +
#     # alterando a largura
#     plot_layout(widths = c(1, 1)
#     )
# ) +
#     #
#     plot_annotation(
#       title = "Tempo Médio de Análise da Prestação de Contas - Anos de 2016 a 2022"
#     ))
# 

tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == 'MCID') %>% 
  group_by(ano) %>%  
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 294.43, colour="blue") +
  ggtitle ('Tempo Médio de Análise da Prestação de Contas - MCID')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2019,  y=327, label="Média 294.43", size = 4) 

tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == 'MD') %>% 
  group_by(ano) %>%  
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 294.43, colour="blue") +
  ggtitle ('Tempo Médio de Análise da Prestação de Contas - MD')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2019,  y=319, label="Média 294.43", size = 4) 