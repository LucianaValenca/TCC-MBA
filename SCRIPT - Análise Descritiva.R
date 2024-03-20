## https://agronomiar.github.io/livroagro/estat%C3%ADstica-descritiva.html


(média1 = mean(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias))
(média = mean(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias))

(média = mean(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias))
(média = mean(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias))

# cálculo das médias por convenente
(médias1 = tapply(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, tempo_positivo_elab_plano_top10_31$convenente, mean))
(médias = tapply(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, tempo_positivo_envio_pc_top10_31$convenente, mean))

# cálculo das médias por concedente
(médias = tapply(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, tempo_positivo_aprov_plano_top10$sigla, mean))
(médias = tapply(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, tempo_positivo_fim_pc_top10$sigla, mean))

#mediana convenente
(mediana1 = median(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dia))
(mediana = median(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias))

#mediana concedente
(mediana = median(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias))
(mediana = median(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias))

# cálculo da moda  por convenente
(medianas1 = tapply(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, tempo_positivo_elab_plano_top10_31$convenente, median))
(medianas = tapply(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, tempo_positivo_envio_pc_top10_31$convenente, median))

# cálculo da moda por concedente
(medianas = tapply(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, tempo_positivo_aprov_plano_top10$sigla, median))
(medianas = tapply(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, tempo_positivo_fim_pc_top10$sigla, median))

# moda
tab = table(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)
(moda1 = names(tab)[tab == max(tab)])

tab = table(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)
(moda = names(tab)[tab == max(tab)])

tab = table(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias)
(moda = names(tab)[tab == max(tab)])

tab = table(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias)
(moda = names(tab)[tab == max(tab)])

#amplitude
(amplitude1 = diff(range(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)))
(amplitude = diff(range(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)))

(amplitude = diff(range(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias)))
(amplitude = diff(range(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias)))

#vaiância
(variância1 = var(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias))
(variância = var(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias))

(variância = var(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias))
(variância = var(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias))


# cálculo da variância  por convenente
(variância = tapply(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, tempo_positivo_elab_plano_top10_31$convenente, var))
(variância = tapply(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, tempo_positivo_envio_pc_top10_31$convenente, var))

# cálculo da variância por concedente
(variância = tapply(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, tempo_positivo_aprov_plano_top10$sigla, var))
(variância = tapply(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, tempo_positivo_fim_pc_top10$sigla, var))

#$desvio padrão
(desvio1 = sd(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias))
(desvio = sd(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias))

(desvio = sd(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias))
(desvio = sd(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias))

# desvio  por convenente
(desvio = tapply(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias, tempo_positivo_elab_plano_top10_31$convenente, sd))
(desvio = tapply(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias, tempo_positivo_envio_pc_top10_31$convenente, sd))

# desvio por concedente
(desvio = tapply(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias, tempo_positivo_aprov_plano_top10$sigla, sd))
(desvio = tapply(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias, tempo_positivo_fim_pc_top10$sigla, sd))

#cv
(CV1 = sd(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias) / mean(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias)*100)
(CV = sd(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias) / mean(tempo_positivo_envio_pc_top10_31$tempo_prestacao_contas_enviado_analise_dias)*100)

(CV = sd(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias) / mean(tempo_positivo_aprov_plano_top10$tempo_analise_plano_trabalho_dias)*100)
(CV = sd(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias) / mean(tempo_positivo_fim_pc_top10$tempo_prestacao_contas_em_analise_dias)*100)

#desritiva tempo_elaboracao_plano_trabalho_dias
descritiva = rbind(Média = média1,
                   Mediana = mediana1,
                   Máximo=max(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias),
                   Mínimo=min(tempo_positivo_elab_plano_top10_31$tempo_elaboracao_plano_trabalho_dias),
                   Amplitude=amplitude1,
                   Variância=variância1,
                   "Desvio-padrão"=desvio1,
                   "CV(%)"=CV1)
colnames(descritiva) = 'Estatísticas Tempo Elaboração Plano de Trabalho'
descritiva
