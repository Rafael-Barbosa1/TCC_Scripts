#------------------------
#------------------------
#--- K-nearest neighbors
#------------------------
#------------------------


set.seed(19)


#--- Criação de treinos e testes genéricos


index1 <- createDataPartition(y = banco_variaveis$Zona, p = 0.7,
                              list = F)

treino1 <- banco_variaveis[index1, -1]

treino_label1 <- banco_variaveis[index1, 1]

teste1 <- banco_variaveis[-index1, -1]

teste_label1 <- banco_variaveis[-index1, 1]


#--- Gráfico com o menor


type_predito <- NULL
taxa_erro <- NULL


for (i in 1:50) {
  type_predito <- knn(train = treino1, test = teste1,
                      cl = treino_label1, k = i)
  taxa_erro[i] <- mean(type_predito != teste_label1)

}




erro_knn <- data.frame(k = 1:50, error.type = taxa_erro*100)

menor_k <-
  erro_knn %>%
  arrange(error.type) %>%
  head(1) %>%
  select(k) %>%
  as.numeric()

menor_erro <-
  erro_knn %>%
  arrange(error.type) %>%
  head(1) %>%
  select(error.type) %>%
  as.numeric



ggplot(data = erro_knn, aes(k, error.type))+
  geom_point(size = 3)+
  geom_line() +
  theme_bw() +
  xlab("k") +
  ylab("Erro") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.y = element_text(colour = "black", face = "bold",
                                    size = 12),
        axis.title.x = element_text(colour = "black", face = "bold",
                                    size = 14),
        axis.text = element_text(colour = "black", size = 12),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black",
                                    face = "bold"),
        legend.text = element_text(size = 12, colour = "black")) +
  scale_y_continuous(labels = formato_real_graf) +
  scale_x_continuous(breaks = seq(0, 50, 2))


cat("O menor erro atingido foi de", menor_erro, "com k =", menor_k)


ggsave(filename = "Gráficos/Melhork_KNN1.pdf", device = "pdf",
       width = 11.57, height = 6, units = "in", dpi = "retina")


#--- KNN


modelo_knn <- knn(train = treino1, test = teste1, cl = treino_label1,
                  k = 33)



#--- Matriz de confusão


confusionMatrix(modelo_knn, teste_label1)
