# ##ANOVA

# https://rpubs.com/paternogbc/46748
# 
# #ANÁLISE DE VARIÂNCIA DOS 4 TEMPOS
# 
# 
anova <- aov(tempo_elaboracao_plano_trabalho_dias ~ convenente, data= tempo_positivo_elab_plano_top10_31)
summary(anova)

medias <- with(tempo_positivo_elab_plano_top10_31,tapply(tempo_elaboracao_plano_trabalho_dias,convenente,mean)) 
erro <- with(tempo_positivo_elab_plano_top10_31,tapply(tempo_elaboracao_plano_trabalho_dias,convenente,function(x) sqrt(var(x)/length(x))))



medias
erro

x <- barplot(medias, beside=T, ylim=c(0,120),ylab="Média Tempo Elaboração Plano de Trabalho (dias)"
             , las=2 )
arrows(x0=x,y0=medias-erro,
       x1=x,y1=medias+erro,
       angle=90,length=0.20,code=3)

g1 <- tempo_positivo_elab_plano_top10_31 %>% 
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
g1

options(max.print=999999)
TukeyHSD(anova)

#rotate_x(medias, 'mpg', row.names(medias), 45)

# O resultado demonstra que existe diferença significativa (ver valor de p na tabela de ANOVA) entre os tratamentos (A,B,C). Para saber quais tratamentos são diferentes precisamos contruir um gráfico de barras com as médias de cada tratamento. Para testar estatísticamente quais tratamentos são diferentes podemos realizar o teste de Tukey (ver a baixo)

# 
# modelo2 <- aov(tempo_elaboracao_plano_trabalho_dias ~ convenente, data= tempo_positivo_elab_plano_top10_31)
# anova(modelo2)
# shapiro.test(modelo2$residuals) #Os erros não seguem distribuição normal
# bartlett.test(modelo2$residuals~convenente2)
# 
# resposta=c(02,02,01,01,00,01,00,00,01,01,12,10,14,17,11,07,09,15,08,10)
# cultivar=rep(LETTERS[1:4],e=5)
# cultivar=as.factor(cultivar)
# 
# modelo=aov(resposta~cultivar)
# anova(modelo) # Conferir GL
# 
# shapiro.test(modelo$residuals)
# bartlett.test(modelo$residuals~cultivar)
# 
# leveneTest(tempo_elaboracao_plano_trabalho_dias ~ ano, data= tempo_positivo_elab_plano_top10_31)
# shapiro.test(resid(anova)[0:5000])
# 
# # #normalização
#  min = min(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)
#  max = max(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)
#  tempo_positivo_elab_plano_top10_31$t_normal <- (tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias - min)/(max - min)
# 
#   leveneTest(t_normal ~ convenente, data= tempo_positivo_elab_plano_top10_31)
#   shapiro.test(resid(anova)[0:5000])
#  
# 
# 
# # anova <- aov(tempo_elaboracao_plano_trabalho_dias ~ convenente, data= tempo_positivo_elab_plano_top10_31)
# # summary(anova)
# #
# # 
# # #nao atendeu a premissa da anova
# # leveneTest(tempo_elaboracao_plano_trabalho_dias ~ convenente, data= tempo_positivo_elab_plano_top10_31)
# # shapiro.test(resid(anova)[0:5000])
# # 
# # #normalização
# # min = min(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)
# # max = max(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)
# # tempo_positivo_elab_plano_top10_31$t_normal <- (tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias - min)/(max - min)
# # 
# # anova <- aov(t_normal ~ convenente, data= tempo_positivo_elab_plano_top10_31)
# # summary(anova)
# # 
# # 
# # leveneTest(t_normal ~ convenente, data= tempo_positivo_elab_plano_top10_31)
# # shapiro.test(resid(anova))
# # 
# # ##envio PC
# # 
# # 
# # anova <- aov(tempo_prestacao_contas_enviado_analise_dias ~ convenente, data= tempo_positivo_envio_pc_top10_31)
# # summary(anova)
# # 
# # 
# # 
# # #nao atendeu a premissa da anova
# # leveneTest(tempo_prestacao_contas_enviado_analise_dias ~ convenente, data= tempo_positivo_envio_pc_top10_31)
# # shapiro.test(resid(anova)[0:5000])
# # 
# # #normalização
# # min = min(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)
# # max = max(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)
# # tempo_positivo_envio_pc_top10_31$t_normal <- (tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias - min)/(max - min)
# # 
# # anova <- aov(t_normal ~ convenente, data= tempo_positivo_envio_pc_top10_31)
# # summary(anova)
# # 
# # 
# # leveneTest(t_normal ~ convenente, data= tempo_positivo_envio_pc_top10_31)
# # shapiro.test(resid(anova))
# # 
# # 
# # # TukeyHSD(anova)
# # # 
# # # medias <- with(tempo_positivo_aprov_plano_top10,tapply(tempo_analise_plano_trabalho_dias,sigla,mean)) 
# # # medias
# # # 
# # # erro <- with(tempo_positivo_aprov_plano_top10,tapply(tempo_analise_plano_trabalho_dias,sigla,function(x) sqrt(var(x)/length(x))))
# # # erro
# # # 
# # # 
# # # require(MASS)
# # # boxcox(tempo_elaboracao_plano_trabalho_dias ~ sigla, data= tempo_positivo_elab_plano_top10, plotit=T)
# # # boxcox(tempo_elaboracao_plano_trabalho_dias ~ sigla, data= tempo_positivo_elab_plano_top10, lam=seq(-1, 1, 1/10))
# # 
# # # require(MASS)
# # # boxcox(resp ~ trat, data=tr, plotit=T)
# # # boxcox(resp ~ trat, data=tr, lam=seq(-1, 1, 1/10))
# # # O gráfico mostra que o valor que maximiza a função é aproximadamente $\hat{\lambda} = 0.1$. Desta forma o próximo passo é obter os dados transformados e depois fazer as análise utilizando estes novos dados.
# # # 
# # # tr$respt <- (tr$resp^(0.1) - 1)/0.1
# # # tr.avt <- aov(respt ~ trat, data=tr)
# # # plot(tr.avt)
# # 
# # # tempo_positivo_elab_plano_top10$respt <- (tempo_positivo_elab_plano_top10$tempo_elaboracao_plano_trabalho_dias^(-0.0625) - 1)/-0.0625
# # # tempo_positivo_elab_plano_top10.avt <- aov(respt ~ sigla, data=tempo_positivo_elab_plano_top10)
# # # plot(tempo_positivo_elab_plano_top10.avt)
# # 
# # # leveneTest(respt ~ sigla, data= tempo_positivo_elab_plano_top10)
# # 
# # 
# # # leveneTest(tempo_elaboracao_plano_trabalho_dias ~ sigla, data= tempo_positivo_elab_plano_top10)
# # # # # 
# # # shapiro.test(anova[0:5000])
# # # 
# # # min = min(tempo_positivo_elab_plano_top10$tempo_elaboracao_plano_trabalho_dias)
# # # max = max(tempo_positivo_elab_plano_top10$tempo_elaboracao_plano_trabalho_dias)
# # # 
# # # tempo_positivo_elab_plano_top10$t_normal <- (tempo_positivo_elab_plano_top10$tempo_elaboracao_plano_trabalho_dias - min)/(max - min)
# # 
