##geral

Anos <- tempo_positivo_aprov_plano_top10 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
  ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - Anos de 2016 a 2022')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 
  


# Ano16 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2016) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 147.78, colour="blue") +
#   annotate("text", x="MDR",  y=158, label="Média 147.78", size = 4)+
#    ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - Ano 2016 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano17 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2017) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 98.82, colour="blue") +
#   annotate("text", x="MD",  y=132, label="    Média 98.82",size = 4)+
#   ggtitle ('Gráfico 3 - Ano 2017 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()

# Ano18 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2018) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 62.17, colour="blue") +
#   annotate("text", x="MCID",  y=70, label="  Média 62.17" ,size = 4)+
#   ggtitle ('Gráfico 4 - Ano 2018 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()


Ano19 <- tempo_positivo_aprov_plano_top10 %>% 
  filter(ano == 2019) %>% 
  group_by(sigla) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = sigla, y = media)) +
  geom_hline(yintercept = 101.32, colour="blue") +
  annotate("text", x="MD",  y=122, label="Média 101.32", size = 4)+
  ggtitle ('Gráfico 5 - Ano 2019 por Concedente')+
  labs(x = "Concedente",
       y = "Tempo Médio (em dias)")+
  geom_col()

# Ano20 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2020) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 72.03, colour="blue") +
#   annotate("text", x="MD",  y=83, label="Média 72.03", size = 4)+
#   ggtitle ('Gráfico 6 -Ano 2020 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano21 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2021) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 	67.94, colour="blue") +
#   annotate("text", x="MD",  y=77, label="Média 67.94" ,size = 4)+
#   ggtitle ('Gráfico 7 - Ano 2021 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()
# 
# Ano22 <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(ano == 2022) %>% 
#   group_by(sigla) %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = sigla, y = media)) +
#   geom_hline(yintercept = 	54.10, colour="blue") +
#   annotate("text", x="MD",  y=63, label="Média 54.10", size = 4)+
#   ggtitle ('Gráfico 8 - Ano 2022 por Concedente')+
#   labs(x = "Concedente",
#        y = "Tempo Médio (em dias)")+
#   geom_col()

	#Ano16 + Ano17 + Ano18 + Ano19 + Ano20 + Ano21 + Ano22  

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
#       title = "Tempo Médio de Aprovação Plano de Trabalho - Anos de 2016 a 2022"
#     ))

# gmesp <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(sigla == 'MESP') %>% 
#   group_by(ano)  %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = ano, y = media)) +
#   ggtitle ('Gráfico MESP - Anos de 2016 a 2022')+
#   labs(x = "Ano",
#        y = "Tempo Médio (em dias)")+
#   geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1))
# #repasse por ano mesp
# 
# gmesp
# 
# 
# mesp <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente, ano=dados_transferegov_final$ano), FUN=sum)
# 
# mesp <- mesp %>% filter(concedente == 'MINISTERIO DO ESPORTE')
# 
# countConvMesp <- dados_transferegov_final %>% filter(concedente == 'MINISTERIO DO ESPORTE')
#   
# count(countMesp, ano)
# 
# gmapa <- tempo_positivo_aprov_plano_top10 %>% 
#   filter(sigla == 'MAPA') %>% 
#   group_by(ano)  %>% 
#   summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = ano, y = media)) +
#   ggtitle ('Gráfico MAPA - Anos de 2016 a 2022')+
#   labs(x = "Ano",
#        y = "Tempo Médio (em dias)")+
#   geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1))
# #repasse por ano mesp
# 
# gmapa
# 
# mapa <- aggregate(dados_transferegov_final$valor_repasse_convenio, by=list(concedente=dados_transferegov_final$concedente, ano=dados_transferegov_final$ano), FUN=sum)
# 
# mapa <- mapa %>% filter(concedente == 'MINISTÉRIO DA AGRICULTURA E PECUÁRIA')
# 
# countConvMapa <- dados_transferegov_final %>% filter(concedente == 'MINISTÉRIO DA AGRICULTURA E PECUÁRIA')
# 
# count(countConvMapa, ano)
# 

#MAPA, MCID, MESP e MS

tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == 'MAPA') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
  ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - MAPA')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 

tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == 'MCID') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
  ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - MCID')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 

tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == 'MESP') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
  ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - MESP')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 

tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == 'MS') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
  ggtitle ('Tempo Médio de Aprovação Plano de Trabalho - MS')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 