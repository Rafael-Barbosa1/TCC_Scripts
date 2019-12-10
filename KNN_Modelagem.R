#------------------------
#------------------------
#--- K-nearest neighbors
#------------------------
#------------------------


set.seed(19)


#--- Criação de treinos e testes genéricos


index1 <- createDataPartition(y = banco_variaveis$Zona, p = 0.7,
                              list = F)

treino1 <- banco_variaveis[index1, ]

treino_label1 <- banco_variaveis[index1, 1]

teste1 <- banco_variaveis[-index1, ]

teste_label1 <- banco_variaveis[-index1, 1]


#--- Gráfico com o menor


type_predito <- NULL
taxa_erro <- NULL


for (i in 1:20) {
  type_predito <- knn(train = treino1, test = teste1,
                      cl = treino_label1, k = i)
  taxa_erro[i] <- mean(type_predito != teste1$Zona)
}


erro_knn <- data.frame(k = 1:20, error.type = taxa_erro*100)



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
  scale_x_continuous(breaks = seq(0, 20, 2))



ggsave(filename = "Gráficos/Melhork_KNN.pdf", device = "pdf",
       width = 11.57, height = 6, units = "in", dpi = "retina")


#--- KNN


modelo_knn <- knn(train = treino1, test = teste1, cl = treino_label1,
                  k = 7)



#--- Matriz de confusão


confusionMatrix(modelo_knn, teste1$Zona)





#---------------


index2 <- createDataPartition(y = banco_variaveis$Zona, p = 0.7,
                              list = F)

treino2 <- banco_variaveis[index2, ]
teste2 <- banco_variaveis[-index2, ]


trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)


tuneGrid <- expand.grid(kmax = 1:20,            # allows to test a range of k values
                        distance = 1:20,        # allows to test a range of distance values
                        kernel = c('gaussian',  # different weighting types in kknn
                                   'triangular',
                                   'rectangular',
                                   'epanechnikov',
                                   'optimal'))


x <- train(Zona ~ .,
           data = treino2,
           method = 'kknn',
           trControl = trctrl,
           preProcess = c('center', 'scale'),
           tuneGrid = tuneGrid,
           tuneLength = 10)
x


y <- predict(object = x, newdata = teste2)

confusionMatrix(y, teste2$Zona)


sum(z$table)
