#-------------------
#-------------------
#--- Treino e Teste
#-------------------
#-------------------


set.seed(19)


index <- createDataPartition(y = banco_variaveis_ln$Zona, p = 0.7,
                             list = F)


treino <- banco_variaveis_ln[index, ]


teste <- banco_variaveis_ln[-index, ]


prop_zonas_treino <-
  treino %>%
  count(Zona) %>%
  mutate(perc = n/sum(n)*100) %>%
  magrittr::set_colnames(paste(colnames(.), "treino", sep = "_"))


prop_zonas_teste <-
  teste %>%
  count(Zona) %>%
  mutate(perc = n/sum(n)*100) %>%
  magrittr::set_colnames(paste(colnames(.), "teste", sep = "_"))


#cbind(prop_zonas_treino, prop_zonas_teste) %>%
#  write.xlsx(x = ., file = "Tabelas/Proporc_Zonas_TesteXTreino.xlsx")



prop_zonas_treino
prop_zonas_teste
