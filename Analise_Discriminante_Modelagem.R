#----------------------------
#----------------------------
#--- Análise de discriminante
#----------------------------
#----------------------------

set.seed(19)

#----------- Modelo linear


modelo_lda <- lda(Zona ~ Profundidade + Temperatura + Fosforo_total +
                    Clorofila + Transparencia + Turbidez + STS +
                    Oxigênio_dissolvido + Ortofosfato + Amonia,
                  data = treino)


predito_lda <- predict(object = modelo_lda, newdata = teste)


conf_matrix_lda <- confusionMatrix(data = predito_lda$class,
                                   reference = teste$Zona)


proportion_trace <- paste(formato_real_graf(
  values = 100*modelo_lda$svd^2/sum(modelo_lda$svd^2)), "%", sep = "")



#--- Gráfico da LD1 x LD2



#--- https://stackoverflow.com/questions/35744365/lda-returns-only-ld1

ggplotLDAPrep <- function(x){
  if (!is.null(Terms <- x$terms)) {
    data <- model.frame(x)
    X <- model.matrix(delete.response(Terms), data)
    g <- model.response(data)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L)
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(x$means)
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
  rtrn <- data.frame(X,labels=as.character(g))
  return(rtrn)
}


# 1: Fluvial, 2: Transição, 3: Lacustre


ggplotLDAPrep(x = modelo_lda) %>%
  mutate(labels = ifelse(labels == 1, "Fluvial",
                         ifelse(labels == 2, "Transição",
                                "Lacustre"))) %>%
  magrittr::set_colnames(c("LD1", "LD2", "Zona")) %>%
  ggplot(data = ., aes(x = LD1, y = LD2, color = Zona, shape = Zona)) +
  geom_point(size = 2.5) +
  stat_ellipse(aes(x = LD1, y = LD2, fill = Zona), alpha = 0.2,
               geom = "polygon") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.y = element_text(colour = "black", face = "bold",
                                    size = 14),
        axis.title.x = element_text(colour = "black", face = "bold",
                                    size = 14),
        axis.text = element_text(colour = "black", size = 12),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black",
                                    face = "bold"),
        legend.text = element_text(size = 14, colour = "black")) +
  labs(x = paste("LD1", proportion_trace[1]),
       y = paste("LD2", proportion_trace[2])) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")



ggsave(filename = "Gráficos/LDA_Modelo.pdf", device = "pdf",
       width = 11.57, height = 6, units = "in", dpi = "retina")



#--- Métricas da matriz de confusão


kappa_lda <- wkappa(x = conf_matrix_lda$table)



#--- Coefficientes of linear discriminants:


coef_lin_disc <-
  modelo_lda$scaling %>%
  as.data.frame %>%
  rownames_to_column %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(Nova_LD1 = paste(LD1, "times", rowname)) %>%
  mutate(Nova_LD2 = paste(LD2, "times", rowname))


coef_lin_disc %>%
  select(Nova_LD2) %>%
  as.vector()

#----------- Modelo quadrático


modelo_qda <- qda(Zona ~ Profundidade + Temperatura + Fosforo_total +
                    Clorofila + Transparencia + Turbidez + STS +
                    Oxigênio_dissolvido + Ortofosfato + Amonia,
                  data = treino)


predito_qda <- predict(object = modelo_qda, newdata = teste,
                       type = "prob")


conf_matrix_qda <- confusionMatrix(data = predito_qda$class,
                                   reference = teste$Zona)


#--- Métricas da matriz de confusão


kappa_qda <- wkappa(x = conf_matrix_qda$table)
