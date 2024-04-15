
tempo_positivo_elab_plano_top10_31 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2016,  y=62, label="Média 57.76", size = 4) 


tempo_positivo_elab_plano_top10_31 %>% 
  filter(convenente == 'RIO DE JANEIRO') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2021,  y=70, label="Média 57.76", size = 4) 

tempo_positivo_elab_plano_top10_31 %>% 
  filter(convenente == 'SAO PAULO') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2021,  y=70, label="Média 57.76", size = 4) 

tempo_positivo_elab_plano_top10_31 %>% 
  filter(convenente == 'BELO HORIZONTE') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2021,  y=65, label="Média 57.76", size = 4) 

