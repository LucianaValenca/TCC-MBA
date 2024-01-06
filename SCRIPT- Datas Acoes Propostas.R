#filtra propostas da modalidade Convênio ou Contrato de Repasse dos anos entre 2016 e 2022 - Brasil
propostasBrasil <-  
  filter(propostas, ANO_PROP >= 2016 &  ANO_PROP <= 2022 & 
           MODALIDADE %in% c("CONVENIO", "CONTRATO DE REPASSE") &
           !(SIT_PROPOSTA %in% c('CONVENIO_ANULADO', 'CONVENIO_RESCINDIDO',
                                 'ASSINATURA_PENDENTE_REGISTRO_TV_SIAFI'))
  ) 


##junção de convênios com propostas pelo ID_PROPOSTA
convenio_proposta <- inner_join(convenios,propostasBrasil,by="ID_PROPOSTA")

#pegar apenas os históricos das Propostas / Convênio a serem analisados
hist_convenio_proposta <- semi_join(historico, convenio_proposta,by="ID_PROPOSTA")

##obter data do cadastro da proposta
hist_convenio_proposta_prop_cad <- filter(hist_convenio_proposta, HISTORICO_SIT=='PROPOSTA_CADASTRADA')

dt_cadastro_proposta <- hist_convenio_proposta_prop_cad %>%                                      
  group_by(ID_PROPOSTA) %>%
  summarise_at(vars(DIA_HISTORICO_SIT),
               list(min =min))

##verificar convenios 101722 - historico PROPOSTA_CADASTRADA - 88750
#propostas_sem_hist_prop_cad <- anti_join(convenio_proposta, hist_convenio_proposta_prop_cad ,by="ID_PROPOSTA")


##obter data início análise da proposta - obter 1a ocorrência
hist_convenio_proposta_prop_ini_analise <-  
  filter(hist_convenio_proposta, HISTORICO_SIT %in% c('PLANO_TRABALHO_EM_ANALISE', 'PROPOSTA_COMPLEMENTADA_EM_ANALISE', 'PROPOSTA_ENVIADA_ANALISE')) 

dt_inicio_analise <- hist_convenio_proposta_prop_ini_analise %>%                                      
  group_by(ID_PROPOSTA) %>%
  summarise_at(vars(DIA_HISTORICO_SIT),
               list(min =min))


##obter data aprovação PT - obter 1a ocorrência
hist_convenio_proposta_prop_aprov_plano <-  
  filter(hist_convenio_proposta, HISTORICO_SIT %in% c('PLANO_TRABALHO_APROVADO')) 

dt_aprovacao_plano <- hist_convenio_proposta_prop_aprov_plano %>%                                      
  group_by(ID_PROPOSTA) %>%
  summarise_at(vars(DIA_HISTORICO_SIT),
               list(min =min))


