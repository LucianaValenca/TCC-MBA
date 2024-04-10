#filtra propostas da modalidade Convênio ou Contrato de Repasse dos anos entre 2016 e 2022 - Brasil
propostasBrasil <-  
  filter(propostas, ano >= 2016 &  ano <= 2022 & 
           modalidade %in% c("CONVENIO", "CONTRATO DE REPASSE")
  ) 


convenios <- 
  filter(convenios, 
           !(situação_convênio %in% c('Cancelado', 'Convênio Rescindido',
                                      'Convênio Anulado'))
  ) 

##junção de convênios com propostas pelo ID_PROPOSTA
convenio_proposta <- inner_join(convenios,propostasBrasil,by="id_proposta")


#pegar apenas os históricos das Propostas a serem analisados
hist_convenio_proposta <- semi_join(historico, convenio_proposta,by="id_proposta")

##obter data do cadastro da proposta
hist_convenio_proposta_cadastro <- filter(hist_convenio_proposta, situação=='PROPOSTA_CADASTRADA')

dt_cadastro_proposta <- hist_convenio_proposta_cadastro %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_cadastro_proposta <- rename(dt_cadastro_proposta, dt_cadastro_proposta = min)

##obter data início análise da proposta - obter 1a ocorrência
hist_convenio_proposta_inicio_analise <-  
  filter(hist_convenio_proposta, situação %in% c('PLANO_TRABALHO_EM_ANALISE', 'PROPOSTA_COMPLEMENTADA_EM_ANALISE', 'PROPOSTA_ENVIADA_ANALISE')) 

dt_inicio_analise <- hist_convenio_proposta_inicio_analise %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_inicio_analise <- rename(dt_inicio_analise, dt_inicio_analise = min)

##obter data aprovação PT - obter 1a ocorrência
hist_convenio_proposta_aprov_plano_trabalho <-  
  filter(hist_convenio_proposta, situação %in% c('PLANO_TRABALHO_APROVADO')) 

dt_aprovacao_plano <- hist_convenio_proposta_aprov_plano_trabalho %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aprovacao_plano <- rename(dt_aprovacao_plano, dt_aprovacao_plano = min)

##obter data previsão  Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_aguardando_pc <-  
  filter(hist_convenio_proposta, situação %in% c('AGUARDANDO_PRESTACAO_CONTAS')) 

dt_aguardando_prestacao_contas <- hist_convenio_proposta_aguardando_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aguardando_prestacao_contas <- rename(dt_aguardando_prestacao_contas, dt_aguardando_prestacao_contas = min)

##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_envio_pc <-  
  filter(hist_convenio_proposta, situação %in% c('PRESTACAO_CONTAS_ENVIADA_ANALISE')) 

dt_envio_prestacao_contas <- hist_convenio_proposta_envio_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_envio_prestacao_contas <- rename(dt_envio_prestacao_contas, dt_envio_prestacao_contas = min)


##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_analise_prestacao_contas <-  
  filter(hist_convenio_proposta, situação %in% c('PRESTACAO_CONTAS_EM_ANALISE')) 

dt_analise_prestacao_contas <- hist_convenio_proposta_analise_prestacao_contas %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_analise_prestacao_contas <- rename(dt_analise_prestacao_contas, dt_analise_prestacao_contas = min)

##obter data fim Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_fim_prestacao_contas <-  
  filter(hist_convenio_proposta, situação %in% c('PRESTACAO_CONTAS_APROVADA', 'PRESTACAO_CONTAS_APROVADA_COM_RESSALVAS' , 'PRESTACAO_CONTAS_REJEITADA', 'PRESTACAO_CONTAS_CONCLUIDA')) 

dt_fim_prestacao_contas <- hist_convenio_proposta_fim_prestacao_contas %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_fim_prestacao_contas <- rename(dt_fim_prestacao_contas, dt_fim_prestacao_contas = min)

#juntar todas as datas no dataframe convenio_proposta
convenio_proposta_juncao   <- 
  left_join(convenio_proposta,dt_cadastro_proposta,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_inicio_analise,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_aprovacao_plano,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_aguardando_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_envio_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_analise_prestacao_contas,by="id_proposta") 

convenio_proposta_juncao   <- 
  left_join(convenio_proposta_juncao,dt_fim_prestacao_contas,by="id_proposta") 


temp <- convenio_proposta_juncao %>%
  mutate(
    tempo_elaboracao_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_inicio_analise), converte_data(dt_cadastro_proposta), unit="days")))
    ,tempo_analise_plano_trabalho_dias = as.numeric(round(difftime(converte_data(dt_aprovacao_plano), converte_data(dt_inicio_analise), unit="days")))
    ,tempo_prestacao_contas_enviado_analise_dias = as.numeric(round(difftime(converte_data(dt_envio_prestacao_contas), converte_data(dt_aguardando_prestacao_contas), unit="days")))
    ,tempo_prestacao_contas_em_analise_dias = as.numeric(round(difftime(converte_data(dt_fim_prestacao_contas), converte_data(dt_analise_prestacao_contas), unit="days")))
  )

# Após o cálculo do Tempo de apresentação do plano de trabalho, foram identificados tempos negativos para 183 convênios. Na análise dos dados, verificou-se que, para 173 convênios, a análise da proposta iniciou no mesmo dia do cadastro da proposta, porém a hora da análise está zerada na base de dados. Para corrigir essa inconsistência, o tempo foi atualizado para 1 dia. Em relação aos 10 convênios restantes, verificou-se que a análise da proposta iniciou antes do dia do cadastro da proposta. Por isso, esses convênios foram removidos do conjunto de dados.
temp$tempo_elaboracao_plano_trabalho_dias <- replace(temp$tempo_elaboracao_plano_trabalho_dias, temp$tempo_elaboracao_plano_trabalho_dias == -1 , 1)
temp$tempo_elaboracao_plano_trabalho_dias <- replace(temp$tempo_elaboracao_plano_trabalho_dias, temp$tempo_elaboracao_plano_trabalho_dias == 0 , 1)

#retira#
# tempo_positivo_elab_plano <- 
#   filter(temp, tempo_elaboracao_plano_trabalho_dias >= 0) %>%
#   select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
#          dt_cadastro_proposta, dt_inicio_analise, 
#          tempo_elaboracao_plano_trabalho_dias)


# No cálculo do Tempo de análise do plano de trabalho, foram identificados tempos negativos para 2741 convênios. Na análise dos dados, verificou-se que, para 1241 convênios, a aprovação da proposta ocorreu na mesma data de análise da proposta, porém a hora da aprovação está zerada na base de dados. Para corrigir essa inconsistência, o tempo foi atualizado para 1 dia. Em relação aos 1501 convênios restantes, verificou-se que a data de aprovação da proposta ocorreu antes da data de início da análise da proposta. Por isso, esses convênios foram removidos do conjunto de dados.

temp$tempo_analise_plano_trabalho_dias <- replace(temp$tempo_analise_plano_trabalho_dias, temp$tempo_analise_plano_trabalho_dias == -1 , 1)
temp$tempo_analise_plano_trabalho_dias <- replace(temp$tempo_analise_plano_trabalho_dias, temp$tempo_analise_plano_trabalho_dias == 0 , 1)

#retira#
# tempo_positivo_aprov_plano <- 
#   filter(temp, tempo_analise_plano_trabalho_dias >= -1) %>%
#   select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
#          dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
#          tempo_analise_plano_trabalho_dias)


#retirado os tempos negativos - antecipação de PC ou inconsistência na base ???
temp$tempo_prestacao_contas_enviado_analise_dias <- replace(temp$tempo_prestacao_contas_enviado_analise_dias, temp$tempo_prestacao_contas_enviado_analise_dias == -1 , 1)
temp$tempo_prestacao_contas_enviado_analise_dias <- replace(temp$tempo_prestacao_contas_enviado_analise_dias, temp$tempo_prestacao_contas_enviado_analise_dias == 0 , 1)



#retira#
# tempo_positivo_envio_pc <- 
#   filter(temp, tempo_prestacao_contas_enviado_analise_dias >= -1) %>%
#   select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
#          data_limite_pc, dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
#          tempo_prestacao_contas_enviado_analise_dias)

temp <- temp %>%
  mutate(
    atraso_dias = as.numeric(round(difftime(converte_data(dt_envio_prestacao_contas), converte_data2(data_limite_pc), unit="days")))
   )


#retirado os tempos 'negativos' - inconsistência na base ???
temp$tempo_prestacao_contas_em_analise_dias <- replace(temp$tempo_prestacao_contas_em_analise_dias, temp$tempo_prestacao_contas_em_analise_dias == -1 , 0)
temp$tempo_prestacao_contas_em_analise_dias <- replace(temp$tempo_prestacao_contas_em_analise_dias, temp$tempo_prestacao_contas_em_analise_dias == 0 , 1)

#retirar
# tempo_positivo_fim_pc <- 
#   filter(temp, tempo_prestacao_contas_em_analise_dias >= -1) %>%
#   select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
#          dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
#          tempo_prestacao_contas_em_analise_dias)

dados_transferegov <- temp %>%
  select(ano, concedente, convenente, valor_repasse_convênio, modalidade, id_proposta, número, convênio, situação_convênio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas,
         tempo_elaboracao_plano_trabalho_dias, tempo_analise_plano_trabalho_dias, tempo_prestacao_contas_enviado_analise_dias, tempo_prestacao_contas_em_analise_dias,
         atraso_dias, data_limite_pc)
 dados_transferegov_final <- dados_transferegov[order(dados_transferegov$ano,dados_transferegov$concedente, dados_transferegov$convenente ),]
 
 
 rm(temp)
 
 rm(hist_convenio_proposta_cadastro)
 rm(hist_convenio_proposta_aprov_plano_trabalho)
 rm(hist_convenio_proposta_envio_pc)
 rm(hist_convenio_proposta_aguardando_pc)
 rm(hist_convenio_proposta_inicio_analise)
 rm(hist_convenio_proposta_analise_prestacao_contas)
 rm(hist_convenio_proposta_fim_prestacao_contas)
 
 rm(dados_transferegov)
 rm(dt_cadastro_proposta)
 rm(dt_inicio_analise)
 rm(dt_aprovacao_plano)
 rm(dt_envio_prestacao_contas)
 rm(dt_analise_prestacao_contas)
 rm(dt_fim_prestacao_contas)
 rm(dt_aguardando_prestacao_contas)
 
 
 