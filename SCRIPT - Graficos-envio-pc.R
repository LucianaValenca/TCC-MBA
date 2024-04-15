
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




tempo_positivo_envio_pc_top10_31 %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 178.19, colour="blue") +
  ggtitle ('Tempo Médio de Envio da Prestação de Contas para Análise - Anos de 2016 a 2022')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  annotate("text", x=2017,  y=190, label="Média 178.19", size = 4) 


tempo_positivo_envio_pc_top10_31 %>% 
  filter(convenente == 'SAO LUIS') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 178.19, colour="blue") +
  ggtitle ('Tempo Médio de Envio da Prestação de Contas para Análise - São Luís')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=192, label="Média 178.19", size = 4) 

tempo_positivo_envio_pc_top10_31 %>% 
  filter(convenente == 'JOAO PESSOA') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 178.19, colour="blue") +
  ggtitle ('Tempo Médio de Envio da Prestação de Contas para Análise - João Pessoa')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=192, label="Média 178.19", size = 4) 

tempo_positivo_envio_pc_top10_31 %>% 
  filter(convenente == 'SALVADOR') %>% 
  group_by(ano) %>% 
  summarise(media = mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media)) +
  geom_hline(yintercept = 178.19, colour="blue") +
  ggtitle ('Tempo Médio de Envio da Prestação de Contas para Análise - Salvador')+
  labs(x = "Ano",
       y = "Tempo Médio (em dias)")+
  geom_col() + scale_x_continuous(breaks=seq(2016, 2022, 1)) +
  #scale_y_continuous(breaks=seq(0, 150, 25)) +
  annotate("text", x=2018,  y=192, label="Média 178.19", size = 4) 