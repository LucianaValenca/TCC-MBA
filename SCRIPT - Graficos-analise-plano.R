##geral

tempo_positivo_aprov_plano_top10 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
   labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 


#MAPA, MCID, MESP e MS

tempo_positivo_aprov_plano_top10 %>% 
  filter(sigla == 'MAPA') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 87.48, colour="blue") +
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
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=94, label="Média 87.48", size = 4) 