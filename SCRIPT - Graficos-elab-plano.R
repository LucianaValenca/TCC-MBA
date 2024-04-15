
# ##geral
# 
# centro <- c('GOIANIA'
#             ,'CAMPO GRANDE'
#             ,'BRASILIA'
#             ,'CUIABA')
# 
# nordeste <- c('SALVADOR'
# ,'RECIFE'
# ,'TERESINA'
# ,'NATAL'
# ,'JOAO PESSOA'
# ,'ARACAJU'
# ,'MACEIO'
# ,'SAO LUIS'
# ,'FORTALEZA')
# 
# norte <- c( 'RIO BRANCO'
#             ,'MACAPA'
#             ,'PORTO VELHO'
#             ,'BOA VISTA'
#             ,'BELEM'
#             ,'PALMAS'
#             ,'MANAUS'
#             ,'CRUZEIRO DO SUL')
# 
# sudeste <-   c('SAO PAULO'
# , 'BELO HORIZONTE'
# , 'VITORIA'
# , 'RIO DE JANEIRO'
# , 'CAMPINAS'
# , 'CACHOEIRO DE ITAPEMIRIM')
# 
# sul <- c( 'CURITIBA'
# ,'PORTO ALEGRE'
# ,'LONDRINA'
# ,'FLORIANOPOLIS')


Anos <- tempo_positivo_elab_plano_top10_31 %>% 
  group_by(convenente) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = convenente, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  ggtitle ('Gráfico 1 - Anos de 2016 a 2022')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1, colour = "black"), axis.text.y = element_text(colour = "black")) +
  geom_col()
 # + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
 #  annotate("text", x=2016,  y=62, label="Média 57.76", size = 4) 
 #  


tempo_positivo_elab_plano_top10_31 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  ggtitle ('Tempo Médio de Apresentação do Plano de Trabalho - Anos de 2016 a 2022')+
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
  ggtitle ('Tempo Médio de Apresentação do Plano de Trabalho - Rio de Janeiro')+
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
  ggtitle ('Tempo Médio de Apresentação do Plano de Trabalho - São Paulo')+
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
  ggtitle ('Tempo Médio de Apresentação do Plano de Trabalho - Belo Horizonte')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2021,  y=65, label="Média 57.76", size = 4) 

tempo_positivo_elab_plano_top10_31 %>% 
  filter(convenente == 'VITORIA') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 57.76, colour="blue") +
  ggtitle ('Tempo Médio de Apresentação do Plano de Trabalho - Vitória')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=65, label="Média 57.76", size = 4) 