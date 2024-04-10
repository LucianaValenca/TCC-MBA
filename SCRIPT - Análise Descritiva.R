## https://agronomiar.github.io/livroagro/estat%C3%ADstica-descritiva.html

#geração de loxpot para identificar outliers

#boxplot tempo_positivo_elab_plano_top10_31
ggplot(tempo_positivo_elab_plano_top10_31, aes(x=convenente, y=tempo_elaboracao_plano_trabalho_dias))+
  xlab("Convenentes") + 
  ylab("Tempo de apresentação do plano de trabalho (dias)") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1, colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(breaks = c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000))+
  geom_boxplot()

#boxplot tempo_positivo_aprov_plano_top10
ggplot(tempo_positivo_aprov_plano_top10, aes(x=sigla, y=tempo_analise_plano_trabalho_dias))+
  xlab("Concedentes") + 
  ylab("Tempo de de análise do plano de trabalho (dias)") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1, colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(breaks = c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000))+
  geom_boxplot()

#boxplot tempo_positivo_envio_pc_top10_31
ggplot(tempo_positivo_envio_pc_top10_31, aes(x=convenente, y=tempo_prestacao_contas_enviado_analise_dias))+
  xlab("Convenentes") + 
  ylab("Tempo de envio da prest. de contas para análise (dias)") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1, colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(breaks = c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000))+
  geom_boxplot()


#boxplot tempo_positivo_fim_pc_top10
ggplot(tempo_positivo_fim_pc_top10, aes(x=sigla, y=tempo_prestacao_contas_em_analise_dias))+
  xlab("Concedentes") + 
  ylab("Tempo de prestação de contas em análise (dias)") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1, colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(breaks = c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000))+
  geom_boxplot()


