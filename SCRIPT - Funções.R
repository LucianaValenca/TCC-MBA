
##Calcular o tempo em dias de cada etapa a partir de uma data-hora
converte_data = function (a) {
  return (strptime(a, "%d/%m/%Y %H:%M:%S", tz = "GMT"))
}

##Calcular o tempo em dias de cada etapa a partir de uma data
converte_data2 = function (a) {
  return (strptime(a, "%d/%m/%Y", tz = "GMT"))
}
