# tempo_prestacao_contas_enviado_analise_dias
# MESP - MINISTERIO DOS ESPORTES
# MDR-MINISTÉRIO DA INTEGRAÇÃO E DO DESENVOLVIMENTO REGIONAL

##ESPORTES
mesp <- tempo_positivo_envio_pc_top10_31 %>% 
  filter(sigla == "MESP") %>%  
  group_by(concedente) 


mesp <- 
  filter(mesp,  atraso_dias > 0 & excedeu_media == 'S') %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_prestacao_contas_enviado_analise_dias, excedeu_media, atraso_dias, sigla, data_limite_pc)




# ggplot(md, aes(x = tempo_prestacao_contas_enviado_analise_dias,fill = convenente)) +
#   geom_histogram(color = "black", binwidth = 50)+
#   #geom_vline(aes(xintercept =  mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)), md, color = "red", linewidth = 2)
#   facet_grid(convenente ~ .) +
#   labs(y = 'Frequência') +
#   scale_fill_manual(values=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3" ,"#FDB462" ,"#B3DE69", "#FCCDE5" ,"#D9D9D9" ,"#BC80BD"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), legend.position = 'top',
#         axis.line = element_line(colour = "black"))
# 

mediaMesp <- mean(mesp$atraso_dias, na.rm = TRUE)

#colunas
# graficoMesp <- mesp %>% 
#   group_by(convenente) %>% 
#   summarise(media = mean(atraso_dias, na.rm = TRUE)) %>% 
#   ggplot(aes(x = media, y = convenente)) +
#   geom_col()
# 
# graficoDef

graficoMesp2 <- mesp %>% 
   group_by(convenente) %>% 
   summarise(media = mean(atraso_dias, na.rm = TRUE)) %>% 
 ggplot(aes(x=media, y=convenente)) +
   geom_bar(stat="identity", position=position_dodge())+
   geom_vline(aes(xintercept =mediaMesp), mesp, color = "red", linewidth = 1)+
   annotate("text", x=214, y=2, label="219.1875", angle=90, size=4, color="blue")+
   ggtitle ('Concedente - Ministério da Defesa')+
   labs(x = "Média Tempo Envio Convênio Para Pretação de Contas - Período de 2016 a 2022",
       y = "Convenente")+
   # geom_text(aes(label='teste'), vjust=1.6, color="white",
   #           position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()

graficoMesp2
  
