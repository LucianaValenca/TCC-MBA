##Carregar os dados de todas as propostas do TransfereGov - 1019812
propostas_completo <- read.csv("planilhas de dados/siconv_proposta.csv", sep=";" , dec=".")


##Carregar os dados das colunas que serão utilizadas - 1019812
propostas <- select(propostas_completo, "ID_PROPOSTA","UF_PROPONENTE","MUNIC_PROPONENTE","COD_ORGAO_SUP", "DESC_ORGAO_SUP","NR_PROPOSTA","ANO_PROP","COD_ORGAO","DESC_ORGAO","MODALIDADE","SIT_PROPOSTA", "VL_REPASSE_PROP")

##aqui só estão as situações das propostas
situacoes <- dplyr::distinct(dplyr::select(propostas_completo, "SIT_PROPOSTA"))

#filtra propostas dos anos entre 2016 e 2022 - Norte e Nordeste
propostasNorteNordeste <-  
  filter(propostas, ANO_PROP >= 2016 &  ANO_PROP <= 2022 & 
           MODALIDADE %in% c('CONVENIO', 'CONTRATO DE REPASSE') &
           UF_PROPONENTE %in% c('AM','PA','AC','RR','RO','AP','TO','MA','PI','CE','RN','PB','PE','AL', 'SE','BA') &
           !(SIT_PROPOSTA %in% c('CONVENIO_ANULADO', 'CONVENIO_RESCINDIDO', 'ASSINATURA_PENDENTE_REGISTRO_TV_SIAFI'))
  ) 

propostasBrasil <-  
  filter(propostas, ANO_PROP >= 2016 &  ANO_PROP <= 2022 & 
           MODALIDADE %in% c("CONVENIO", "CONTRATO DE REPASSE") &
           !(SIT_PROPOSTA %in% c('CONVENIO_ANULADO', 'CONVENIO_RESCINDIDO',
                                 'ASSINATURA_PENDENTE_REGISTRO_TV_SIAFI'))
  ) 

#convênios completos
convenios_completo <- read.csv("planilhas de dados/siconv_convenio.csv", sep=";" , dec=".")

##Carregar os dados das colunas que serão utilizadas - 1019812
convenios <- select(convenios_completo, 'NR_CONVENIO',	'ID_PROPOSTA',	'SIT_CONVENIO',	'DIAS_PREST_CONTAS',	'DIA_LIMITE_PREST_CONTAS', 'VL_REPASSE_CONV')

##junção de convênios com propostas pelo ID_PROPOSTA 101722
convenio_proposta <- inner_join(convenios,propostasBrasil,by="ID_PROPOSTA")

##Carregar os dados de todas os históricos do TransfereGov
historicos_completo <- read.csv("planilhas de dados/siconv_historico_situacao.csv", sep=";" , dec=".")

##Carregar os dados das colunas que serão utilizadas 
historico <- select(historicos_completo, "ID_PROPOSTA","DIA_HISTORICO_SIT","HISTORICO_SIT")

#pegar apenas os históricos de convenio_proposta
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


