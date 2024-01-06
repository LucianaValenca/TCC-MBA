##aqui só estão as situações das propostas
situacoes <- dplyr::distinct(dplyr::select(propostas_completo, "SIT_PROPOSTA"))

#filtra propostas dos anos entre 2016 e 2022 - Norte e Nordeste
propostasNorteNordeste <-  
  filter(propostas, ANO_PROP >= 2016 &  ANO_PROP <= 2022 & 
           MODALIDADE %in% c('CONVENIO', 'CONTRATO DE REPASSE') &
           UF_PROPONENTE %in% c('AM','PA','AC','RR','RO','AP','TO','MA','PI','CE','RN','PB','PE','AL', 'SE','BA') &
           !(SIT_PROPOSTA %in% c('CONVENIO_ANULADO', 'CONVENIO_RESCINDIDO', 'ASSINATURA_PENDENTE_REGISTRO_TV_SIAFI'))
  ) 

##verificar convenios 101722 - historico PROPOSTA_CADASTRADA - 88750
#propostas_sem_hist_prop_cad <- anti_join(convenio_proposta, hist_convenio_proposta_prop_cad ,by="ID_PROPOSTA")

