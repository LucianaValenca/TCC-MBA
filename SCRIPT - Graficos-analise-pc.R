
tempo_positivo_fim_pc_top10 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 294.43, colour="blue") +
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2018,  y=320, label="Média 294.43", size = 4) 
  


tempo_positivo_fim_pc_top10 %>% 
  filter(sigla == 'MCID') %>% 
  group_by(ano) %>%  
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 294.43, colour="blue") +
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
   labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2019,  y=319, label="Média 294.43", size = 4) 