##Carregar os dados de todas as propostas do TransfereGov
propostas_completo <- read.csv("planilhas de dados/siconv_proposta.csv", sep=";" , dec=".")
min(propostas_completo$ANO_PROP)
max(propostas_completo$ANO_PROP)
unique(propostas_completo$MODALIDADE)

##Carregar os dados com as colunas que serão utilizadas
propostas <- select(propostas_completo, "ID_PROPOSTA", "ANO_PROP", "NR_PROPOSTA", "SIT_PROPOSTA", "MODALIDADE",  "COD_ORGAO","DESC_ORGAO", "UF_PROPONENTE","MUNIC_PROPONENTE", "VL_REPASSE_PROP")
propostas <- rename(propostas, id_proposta = ID_PROPOSTA, ano=ANO_PROP, numero=NR_PROPOSTA, situacao_proposta=SIT_PROPOSTA, modalidade=MODALIDADE, siorg=COD_ORGAO, concedente=DESC_ORGAO, convenente=MUNIC_PROPONENTE, uf=UF_PROPONENTE, valor_repasse_proposta=VL_REPASSE_PROP)

##Carregar os dados de todas os  convênios do TransfereGov
convenios_completo <- read.csv("planilhas de dados/siconv_convenio.csv", sep=";" , dec=".")
##Carregar os dados das colunas que serão utilizadas 
convenios <- select(convenios_completo, 'ID_PROPOSTA', 'NR_CONVENIO',	'SIT_CONVENIO',	'DIAS_PREST_CONTAS',	'DIA_LIMITE_PREST_CONTAS', 'VL_REPASSE_CONV')
##Foram identificadas duplicidades na massa de dados
convenios <- distinct(convenios, ID_PROPOSTA, .keep_all = TRUE)
convenios <- rename(convenios, id_proposta = ID_PROPOSTA, convenio=NR_CONVENIO, situacao_convenio=SIT_CONVENIO, qtd_dias_pc=DIAS_PREST_CONTAS, data_limite_pc=DIA_LIMITE_PREST_CONTAS, valor_repasse_convenio=VL_REPASSE_CONV)

##Carregar os dados de todas os históricos das propostas/convênios  do TransfereGov
historicos_completo <- read.csv("planilhas de dados/siconv_historico_situacao.csv", sep=";" , dec=".")
##Carregar os dados das colunas que serão utilizadas 
historico <- select(historicos_completo, "ID_PROPOSTA","HISTORICO_SIT", "DIA_HISTORICO_SIT")
historico <- rename(historico, id_proposta = ID_PROPOSTA, situacao_historico=HISTORICO_SIT, data_hora=DIA_HISTORICO_SIT)

