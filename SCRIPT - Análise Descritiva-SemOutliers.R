#https://rpubs.com/paternogbc/46768

siglas_concedentes <- c('CODEVASF' , 'FNS'    ,  'MAPA'    ,  'MCID'    ,    'MD'   ,    'MDR'  ,    'MESP'   ,   'MJSP', 'MS'   ,   'MTUR' )

#descritiva geral



##1 - retirar outlier tempo_positivo_elab_plano_top10_31###
dim(tempo_positivo_elab_plano_top10_31)

quartiles <- quantile(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(tempo_positivo_elab_plano_top10_31, tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias > Lower & tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias < Upper)

dim(data_no_outlier)

#"Outliers métrica Tempo de apresentação do plano de trabalho"
4441-4020

#descririva geral
(media= mean(data_no_outlier$tempo_elaboracao_plano_trabalho_dias))
(mediana= median(data_no_outlier$tempo_elaboracao_plano_trabalho_dias))
(minimo= min(data_no_outlier$tempo_elaboracao_plano_trabalho_dias))
(maximo= max(data_no_outlier$tempo_elaboracao_plano_trabalho_dias))
(maximo-minimo)
(desvio= sd(data_no_outlier$tempo_elaboracao_plano_trabalho_dias))
(cv= (desvio/media)*100)


#Descritiva por convenete
(médias1 = tapply(data_no_outlier$tempo_elaboracao_plano_trabalho_dias, data_no_outlier$convenente, mean))
(medianas1 = tapply(data_no_outlier$tempo_elaboracao_plano_trabalho_dias, data_no_outlier$convenente, median))
(desvios1 = tapply(data_no_outlier$tempo_elaboracao_plano_trabalho_dias, data_no_outlier$convenente, sd))
(minimos1 = tapply(data_no_outlier$tempo_elaboracao_plano_trabalho_dias, data_no_outlier$convenente, min))
(maximos1 = tapply(data_no_outlier$tempo_elaboracao_plano_trabalho_dias, data_no_outlier$convenente, max))

descritiva_por_convenente_elab_plan_sem_out <- tibble(convenentes, médias1, medianas1, minimos1, maximos1, maximos1-minimos1, desvios1, (desvios1/médias1)*100)
write.csv(descritiva_por_convenente_elab_plan_sem_out, "descritiva_por_convenente_elab_plan_sem_out.csv", row.names = FALSE)

##################
##2 - retirar outlier tempo_positivo_aprov_plano_top10###
dim(tempo_positivo_aprov_plano_top10)

quartiles <- quantile(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier3 <- subset(tempo_positivo_aprov_plano_top10, tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias > Lower & tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias < Upper)

dim(data_no_outlier3)

#"Outliers métrica Tempo de análise do plano de trabalhoo"
63364-58065

#descririva geral
(media= mean(data_no_outlier3$tempo_analise_plano_trabalho_dias))
(mediana= median(data_no_outlier3$tempo_analise_plano_trabalho_dias))
(minimo= min(data_no_outlier3$tempo_analise_plano_trabalho_dias))
(maximo= max(data_no_outlier3$tempo_analise_plano_trabalho_dias))
(maximo-minimo)
(desvio= sd(data_no_outlier3$tempo_analise_plano_trabalho_dias))
(cv= (desvio/media)*100)


#descritiva por concedente
(médias3 = tapply(data_no_outlier3$tempo_analise_plano_trabalho_dias, data_no_outlier3$concedente, mean))
(medianas3 = tapply(data_no_outlier3$tempo_analise_plano_trabalho_dias, data_no_outlier3$concedente, median))
(desvios3 = tapply(data_no_outlier3$tempo_analise_plano_trabalho_dias, data_no_outlier3$concedente, sd))
(minimos3 = tapply(data_no_outlier3$tempo_analise_plano_trabalho_dias, data_no_outlier3$concedente, min))
(maximos3 = tapply(data_no_outlier3$tempo_analise_plano_trabalho_dias, data_no_outlier3$concedente, max))

descritiva_por_convenente_analiseb_plan_sem_out <- tibble(siglas_concedentes, médias3, medianas3, minimos3, maximos3, maximos3-minimos3, desvios3, (desvios3/médias3)*100)
write.csv(descritiva_por_convenente_analiseb_plan_sem_out, "descritiva_por_convenente_analise_plan_sem_out.csv", row.names = FALSE)

##############
##3 - retirar outlier tempo_positivo_envio_pc_top10_31###
dim(tempo_positivo_envio_pc_top10_31)

quartiles <- quantile(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier2 <- subset(tempo_positivo_envio_pc_top10_31, tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias > Lower & tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias < Upper)

dim(data_no_outlier2)

totalatrasados <- data_no_outlier2 %>% count(convenente) 

totalatrasados <- rename(totalatrasados, 'total_atrasos' = n)
totalatrasados <- rename(totalatrasados, 'convenentes' = convenente)

#"Outliers Métrica Tempo de envio da prestação de contas para análise"
2620-2304

#descririva geral
(media= mean(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias))
(mediana= median(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias))
(minimo= min(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias))
(maximo= max(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias))
(maximo-minimo)
(desvio= sd(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias))
(cv= (desvio/media)*100)

#descritiva por convenete
(médias2 = tapply(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias, data_no_outlier2$convenente, mean))
(medianas2 = tapply(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias, data_no_outlier2$convenente, median))
(desvios2 = tapply(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias, data_no_outlier2$convenente, sd))
(minimos2 = tapply(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias, data_no_outlier2$convenente, min))
(maximos2 = tapply(data_no_outlier2$tempo_prestacao_contas_enviado_analise_dias, data_no_outlier2$convenente, max))

descritiva_por_convenente_envio_pc_sem_out <- tibble(convenentes, médias2, medianas2, minimos2, maximos2, maximos2-minimos2, desvios2, (desvios2/médias2)*100)

descritiva_por_convenente_envio_pc_sem_out   <- 
  left_join(descritiva_por_convenente_envio_pc_sem_out,
            totalatrasados,
            by="convenentes") 

write.csv(descritiva_por_convenente_envio_pc_sem_out, "descritiva_por_convenente_envio_pc_sem_out.csv", row.names = FALSE)

##4 - retirar outlier tempo_positivo_fim_pc_top10###
dim(tempo_positivo_fim_pc_top10)

quartiles <- quantile(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier4 <- subset(tempo_positivo_fim_pc_top10, tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias > Lower & tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias < Upper)

dim(data_no_outlier4)

#"Outliers métrica Tempo de prestação de contas em análise"
34676-31550

#descririva geral
(media= mean(data_no_outlier4$tempo_prestacao_contas_em_analise_dias))
(mediana= median(data_no_outlier4$tempo_prestacao_contas_em_analise_dias))
(minimo= min(data_no_outlier4$tempo_prestacao_contas_em_analise_dias))
(maximo= max(data_no_outlier4$tempo_prestacao_contas_em_analise_dias))
(maximo-minimo)
(desvio= sd(data_no_outlier4$tempo_prestacao_contas_em_analise_dias))
(cv= (desvio/media)*100)


(médias4 = tapply(data_no_outlier4$tempo_prestacao_contas_em_analise_dias, data_no_outlier4$concedente, mean))
(medianas4 = tapply(data_no_outlier4$tempo_prestacao_contas_em_analise_dias, data_no_outlier4$concedente, median))
(desvios4 = tapply(data_no_outlier4$tempo_prestacao_contas_em_analise_dias, data_no_outlier4$concedente, sd))
(minimos4 = tapply(data_no_outlier4$tempo_prestacao_contas_em_analise_dias, data_no_outlier4$concedente, min))
(maximos4 = tapply(data_no_outlier4$tempo_prestacao_contas_em_analise_dias, data_no_outlier4$concedente, max))

descritiva_por_concedente_fim_pc <- tibble(siglas_concedentes, médias4, medianas4, minimos4, maximos4, maximos4-minimos4, desvios4, (desvios4/médias4)*100)
write.csv(descritiva_por_concedente_fim_pc, "descritiva_por_concedente_fim_pc_sem_out.csv", row.names = FALSE)

rm(médias1)
rm(médias2)
rm(médias3)
rm(médias4)
rm(medianas1)
rm(medianas2)
rm(medianas3)
rm(medianas4)
rm(minimos1)
rm(minimos2)
rm(minimos3)
rm(minimos4)
rm(maximos1)
rm(maximos2)
rm(maximos3)
rm(maximos4)
rm(desvios1)
rm(desvios2)
rm(desvios3)
rm(desvios4)
