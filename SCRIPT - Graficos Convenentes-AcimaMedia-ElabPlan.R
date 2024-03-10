# tempo_elaboracao_plano_trabalho_dias
# MD - MINISTERIO DA DEFESA - 2022
# MJSP-MINISTERIO DA JUSTICA E SEGURANCA PUBLICA - 2022
# MESP-MINISTERIO DO  - 2022


##DEFESA

md <- tempo_positivo_elab_plano_top10_31 %>% 
  filter(sigla == "MD") %>%  
  group_by(concedente) 


md <- 
  filter(md,  excedeu_media == 'S' & ano==2022) %>%
  select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
         dt_cadastro_proposta, dt_inicio_analise, 
         tempo_elaboracao_plano_trabalho_dias, excedeu_media, sigla)




# ggplot(md, aes(x = tempo_elaboracao_plano_trabalho_dias,fill = convenente)) +
#   geom_histogram(color = "black", binwidth = 50)+
#   #geom_vline(aes(xintercept =  mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)), md, color = "red", linewidth = 2)
#   facet_grid(convenente ~ .) +
#   labs(y = 'Frequência') +
#   scale_fill_manual(values=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3" ,"#FDB462" ,"#B3DE69", "#FCCDE5" ,"#D9D9D9" ,"#BC80BD"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), legend.position = 'top',
#         axis.line = element_line(colour = "black"))
# 

#colunas
graficoDef <- md %>% 
  group_by(convenente) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
  ggplot(aes(x = media, y = convenente)) +
  geom_col()

graficoDef


 
graficoDef2 <- md %>% 
   group_by(convenente) %>% 
   summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
 ggplot(aes(x=media, y=convenente)) +
   geom_bar(stat="identity", position=position_dodge())+
   geom_vline(aes(xintercept = 39.66647 ), md, color = "red", linewidth = 1)+
   annotate("text", x=37, y=1, label="39.66647", angle=90, size=4, color="blue")+
   ggtitle ('Concedente - Ministério da Defesa')+
   labs(x = "Média Tempo Elaboração Plano de trabalho - Ano 2022",
       y = "Convenente")+
   # geom_text(aes(label='teste'), vjust=1.6, color="white",
   #           position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()

graficoDef2
  

 ##MJSP
 
 mjsp <- tempo_positivo_elab_plano_top10_31 %>% 
   filter(sigla == "MJSP") %>%  
   group_by(concedente) 
 
 
 mjsp <- 
   filter(mjsp,   excedeu_media == 'S' & ano==2022) %>%
   select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
          dt_cadastro_proposta, dt_inicio_analise, 
          tempo_elaboracao_plano_trabalho_dias, excedeu_media, sigla)
 

 
 #barras
 graficoMJ <- mjsp %>% 
   group_by(convenente) %>% 
   summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
   ggplot(aes(x = media, y = convenente)) +
   geom_col()
 
 graficoMJ 
 
 graficoMJ2 <- mjsp %>% 
   group_by(convenente) %>% 
   summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
   ggplot(aes(x=media, y=convenente)) +
   geom_bar(stat="identity", position=position_dodge())+
   geom_vline(aes(xintercept = 39.66647 ), mjsp, color = "red", linewidth = 1)+
   annotate("text", x=36, y=4, label="39.66647", angle=90, size=4, color="blue")+
   ggtitle ('Concedente - Ministério da Justiça e Segurança Pública')+
   labs(x = "Média Tempo Elaboração Plano de trabalho - Ano 2022",
        y = "Convenente")+
   # geom_text(aes(label='teste'), vjust=1.6, color="white",
   #           position = position_dodge(0.9), size=3.5)+
   scale_fill_brewer(palette="Paired")+
   theme_minimal()
 
 graficoMJ2

 
 ##MESP
 
 mesp <- tempo_positivo_elab_plano_top10_31 %>% 
   filter(sigla == "MESP") %>%  
   group_by(concedente) 
 
 
 mesp <- 
   filter(mesp,   excedeu_media == 'S' & ano==2022) %>%
   select(ano, concedente, convenente, uf, valor_repasse_convenio, modalidade, id_proposta, numero, convenio, situacao_convenio, 
          dt_cadastro_proposta, dt_inicio_analise, 
          tempo_elaboracao_plano_trabalho_dias, excedeu_media, sigla)
 
 
 
 #barras
 graficoMESP <- mesp %>% 
   group_by(convenente) %>% 
   summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
   ggplot(aes(x = media, y = convenente)) +
   geom_col()
 
 graficoMESP 
 
 graficoMESP2 <- mesp %>% 
   group_by(convenente) %>% 
   summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)) %>% 
   ggplot(aes(x=media, y=convenente)) +
   geom_bar(stat="identity", position=position_dodge())+
   geom_vline(aes(xintercept = 39.66647 ), mesp, color = "red", linewidth = 1)+
   annotate("text", x=36, y=2, label="39.66647", angle=90, size=4, color="blue")+
   ggtitle ('Concedente - Ministério dos Esportes')+
   labs(x = "Média Tempo Elaboração Plano de trabalho - Ano 2022",
        y = "Convenente")+
   # geom_text(aes(label='teste'), vjust=1.6, color="white",
   #           position = position_dodge(0.9), size=3.5)+
   scale_fill_brewer(palette="Paired")+
   theme_minimal()
 
 graficoMESP2
