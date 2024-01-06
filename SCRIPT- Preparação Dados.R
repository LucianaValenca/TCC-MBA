#filtra propostas da modalidade Convênio ou Contrato de Repasse dos anos entre 2016 e 2022 - Brasil
propostasBrasil <-  
  filter(propostas, ano >= 2016 &  ano <= 2022 & 
           modalidade %in% c("CONVENIO", "CONTRATO DE REPASSE") &
           !(situacao_proposta %in% c('CONVENIO_ANULADO', 'CONVENIO_RESCINDIDO',
                                 'ASSINATURA_PENDENTE_REGISTRO_TV_SIAFI'))
  ) 

convenios <- 
  filter(convenios, 
           !(situacao_convenio %in% c('Cancelado', 'Convênio Rescindido',
                                      'Cancelado'))
  ) 

##junção de convênios com propostas pelo ID_PROPOSTA
convenio_proposta <- inner_join(convenios,propostasBrasil,by="id_proposta")

#pegar apenas os históricos das Propostas a serem analisados
hist_convenio_proposta <- semi_join(historico, convenio_proposta,by="id_proposta")

##obter data do cadastro da proposta
hist_convenio_proposta_cadastro <- filter(hist_convenio_proposta, situacao_historico=='PROPOSTA_CADASTRADA')

dt_cadastro_proposta <- hist_convenio_proposta_cadastro %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_cadastro_proposta <- rename(dt_cadastro_proposta, dt_cadastro_proposta = min)

##obter data início análise da proposta - obter 1a ocorrência
hist_convenio_proposta_inicio_analise <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PLANO_TRABALHO_EM_ANALISE', 'PROPOSTA_COMPLEMENTADA_EM_ANALISE', 'PROPOSTA_ENVIADA_ANALISE')) 

dt_inicio_analise <- hist_convenio_proposta_inicio_analise %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_inicio_analise <- rename(dt_inicio_analise, dt_inicio_analise = min)

##obter data aprovação PT - obter 1a ocorrência
hist_convenio_proposta_aprov_plano_trabalho <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PLANO_TRABALHO_APROVADO')) 

dt_aprovacao_plano <- hist_convenio_proposta_aprov_plano_trabalho %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aprovacao_plano <- rename(dt_aprovacao_plano, dt_aprovacao_plano = min)

##obter data previsão  Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_aguardando_pc <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('AGUARDANDO_PRESTACAO_CONTAS')) 

dt_aguardando_prestacao_contas <- hist_convenio_proposta_aguardando_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_aguardando_prestacao_contas <- rename(dt_aguardando_prestacao_contas, dt_aguardando_prestacao_contas = min)

##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_envio_pc <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_ENVIADA_ANALISE')) 

dt_envio_prestacao_contas <- hist_convenio_proposta_envio_pc %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_envio_prestacao_contas <- rename(dt_envio_prestacao_contas, dt_envio_prestacao_contas = min)


##obter data envio análise Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_analise_prestacao_contas <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_EM_ANALISE')) 

dt_analise_prestacao_contas <- hist_convenio_proposta_analise_prestacao_contas %>%                                      
  group_by(id_proposta) %>%
  summarise_at(vars(data_hora),
               list(min =min))

#renomear coluna min
dt_analise_prestacao_contas <- rename(dt_analise_prestacao_contas, dt_analise_prestacao_contas = min)

##obter data fim Prestação de Contas - obter 1a ocorrência
hist_convenio_proposta_fim_prestacao_contas <-  
  filter(hist_convenio_proposta, situacao_historico %in% c('PRESTACAO_CONTAS_APROVADA', 'PRESTACAO_CONTAS_APROVADA_COM_RESSALVAS' , 'PRESTACAO_CONTAS_REJEITADA', 'PRESTACAO_CONTAS_CONCLUIDA')) 

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

##Calcular o tempo em dias de cada etapa
converte_data = function (a) {
    return (strptime(a, "%d/%m/%Y %H:%M:%S", tz = "GMT"))
}


temp <- convenio_proposta_juncao %>%
  mutate(
    tempo_elaboracao_plano_trabalho = round(difftime(converte_data(dt_inicio_analise), converte_data(dt_cadastro_proposta), unit="days")) 
    ,tempo_analise_plano_trabalho = round(difftime(converte_data(dt_aprovacao_plano), converte_data(dt_inicio_analise), unit="days"))
    ,tempo_prestacao_contas_enviado_analise = round(difftime(converte_data(dt_envio_prestacao_contas), converte_data(dt_aguardando_prestacao_contas), unit="days"))
    ,tempo_prestacao_contas_em_analise = round(difftime(converte_data(dt_fim_prestacao_contas), converte_data(dt_envio_prestacao_contas), unit="days"))
  )


dados_transferegov <- temp %>%
  select(ano, repassador, recebedor, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, dt_aprovacao_plano, dt_aguardando_prestacao_contas, dt_envio_prestacao_contas, dt_analise_prestacao_contas, dt_fim_prestacao_contas, 
         tempo_elaboracao_plano_trabalho, tempo_analise_plano_trabalho, tempo_prestacao_contas_enviado_analise, tempo_prestacao_contas_em_analise)


dados_transferegov_final <- dados_transferegov[order(dados_transferegov$ano,dados_transferegov$repassador, dados_transferegov$recebedor ),]
