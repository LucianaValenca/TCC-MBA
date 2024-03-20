# ##ANOVA

# https://rpubs.com/paternogbc/46748
# 
# #ANÁLISE DE VARIÂNCIA DOS 4 TEMPOS

# 
# 

#elaboração plano
anova <- aov(tempo_elaboracao_plano_trabalho_dias ~ convenente, data= tempo_positivo_elab_plano_top10_31)
summary(anova)


g_media_erro <- tempo_positivo_elab_plano_top10_31 %>% 
  #filter(sigla == "MDR") %>%  
  group_by(convenente) %>% 
  summarise(media = mean(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE), 
            sd = sd(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE),
            sem = sd(tempo_elaboracao_plano_trabalho_dias, na.rm = TRUE)/sqrt(length(tempo_elaboracao_plano_trabalho_dias))) %>% 
  ggplot(aes(x = convenente, y = media)) +
  ggtitle ('XXXXX')+
  labs(x = "Convenentes",
       y = "Tempo Médio Aporvação Plano de Trabalho (em dias)")+
  geom_col() +  geom_errorbar(aes(ymin = media - sem, ymax = media + sem),
                              width=0.2) +   theme(axis.text.x = element_text(angle = 90, size = 10)) 
g_media_erro

options(max.print=999999)
TukeyHSD(anova)

#envio pc
anova <- aov(tempo_prestacao_contas_enviado_analise_dias ~ convenente, data= tempo_positivo_envio_pc_top10_31)
summary(anova)

g_media_erro2 <- tempo_positivo_envio_pc_top10_31 %>% 
  #filter(sigla == "MDR") %>%  
  group_by(convenente) %>% 
  summarise(media = mean(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE), 
            sd = sd(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE),
            sem = sd(tempo_prestacao_contas_enviado_analise_dias, na.rm = TRUE)/sqrt(length(tempo_prestacao_contas_enviado_analise_dias))) %>% 
  ggplot(aes(x = convenente, y = media)) +
  ggtitle ('XXXXX')+
  labs(x = "Convenentes",
       y = "Tempo Médio Envio Prestação Contas para análise (em dias)")+
  geom_col() +  geom_errorbar(aes(ymin = media - sem, ymax = media + sem),
                              width=0.2) +   theme(axis.text.x = element_text(angle = 90, size = 10)) 
g_media_erro2

TukeyHSD(anova)

#aprovacao plano
anova <- aov(tempo_analise_plano_trabalho_dias ~ sigla, data= tempo_positivo_aprov_plano_top10)
summary(anova)

g_media_erro3 <- tempo_positivo_aprov_plano_top10 %>% 
  #filter(sigla == "MDR") %>%  
  group_by(sigla) %>% 
  summarise(media = mean(tempo_analise_plano_trabalho_dias, na.rm = TRUE), 
            sd = sd(tempo_analise_plano_trabalho_dias, na.rm = TRUE),
            sem = sd(tempo_analise_plano_trabalho_dias, na.rm = TRUE)/sqrt(length(tempo_analise_plano_trabalho_dias))) %>% 
  ggplot(aes(x = sigla, y = media)) +
  ggtitle ('XXXXX')+
  labs(x = "Concedentes",
       y = "Tempo Médio Aprovação do Plano de Trabalho (em dias)")+
  geom_col() +  geom_errorbar(aes(ymin = media - sem, ymax = media + sem),
                              width=0.2) +   theme(axis.text.x = element_text(angle = 90, size = 10)) 
g_media_erro3

TukeyHSD(anova)

#fim pc
anova <- aov(tempo_prestacao_contas_em_analise_dias ~ sigla, data= tempo_positivo_fim_pc_top10)
summary(anova)

g_media_erro4 <- tempo_positivo_fim_pc_top10 %>% 
  #filter(sigla == "MDR") %>%  
  group_by(sigla) %>% 
  summarise(media = mean(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE), 
            sd = sd(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE),
            sem = sd(tempo_prestacao_contas_em_analise_dias, na.rm = TRUE)/sqrt(length(tempo_prestacao_contas_em_analise_dias))) %>% 
  ggplot(aes(x = sigla, y = media)) +
  ggtitle ('XXXXX')+
  labs(x = "Concedentes",
       y = "Tempo Médio Aprovação do Plano de Trabalho (em dias)")+
  geom_col() +  geom_errorbar(aes(ymin = media - sem, ymax = media + sem),
                              width=0.2) +   theme(axis.text.x = element_text(angle = 90, size = 10)) 
g_media_erro4

TukeyHSD(anova)