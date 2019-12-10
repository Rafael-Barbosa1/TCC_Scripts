#----------
#----------
#--- ANOVA
#----------
#----------


#--- Teste de BoxM para homogeneidade das variâncias (Pressuposto 2)


# H0:The Covariance matrices are homogeneous
#                   x
# H1:The Covariance matrices are not homogeneous


boxm_covariance_matrizes <- BoxM(banco_variaveis_ln[, 2:12],
                                 banco_variaveis_ln$Zona)


# Como 0.0001 é menor que o nível de significância de 5%, então
# rejeitamos H0. Logo, as matrizes de covariância não são homogêneas



#--- Normalidade das variáveis ou multivariada (Pressuposto 3)


# H0: multivariate normality of your data
#                   x
# H1: no multivariate normality of your data


mvn(data = banco_variaveis_ln %>% select(-Zona),
         mvnTest = "dh")


# Como 0.0001 é menor que o nível de significância de 5%, então
# rejeitamos H0. Logo, os dados não seguem uma distruibuição normal
# multivariada




#--- Fórmula para ANOVA


# Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
# Analysis of variance; designed experiments.
# Chapter 5 of Statistical Models in S eds J. M. Chambers and
# T. J. Hastie, Wadsworth & Brooks/Cole.



anova1 <- aov(Cota + Profundidade + Temperatura + Fosforo_total +
                Clorofila + Transparencia + Turbidez + STS +
                Oxigênio_dissolvido + Ortofosfato + Amonia ~ Zona - 1,
              data = banco_variaveis_ln)

#--- Sumário da ANOVA com o p-valor


sumario_anova1 <- summary(anova1)

#xtable::xtable(anova1) %>%
#  write.xlsx(file = "Tabelas/Sumario_ANOVA.xlsx")


#--- Teste de tukey para ver se as médias se diferenciam

teste_tukey_anova1 <- TukeyHSD(x = anova1, conf.level = 0.95)


#--- Teste de normalidade para os resíduos


shapiro.test(anova1$residuals) # p = 0.158 > alpha de 5%


#--- Teste de Levene para homogeneidade das variâncias

leveneTest(y = anova1, center = mean)



# Zona --> 1: Fluvial, 2: Transição, 3: Lacustre


teste_tukey_anova1$Zona %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate(rowname = ifelse(test = rowname == "2-1",
                          yes = "Transição - Fluvial",
                          no = ifelse(test = rowname == "3-1",
                                      yes = "Lacustre - Fluvial",
                                      no = "Lacustre - Transição"))) %>%
  ggplot(data = ., aes(x = rowname, y = diff)) +
  geom_errorbar(aes(ymax = upr, ymin = lwr),
                color = "darkblue", size = 0.7) +
  geom_point(color = "darkblue", size = 2) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(-4, 4),
                     labels = formato_real_graf) +
  labs(x = "Comparação entre Zonas",
       y = "Diferenças nos níveis médios de Zona") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.y = element_text(colour = "black", face = "bold",
                                    size = 12),
        axis.title.x = element_text(colour = "black", face = "bold",
                                    size = 14),
        axis.text = element_text(colour = "black", size = 12),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black",
                                    face = "bold"),
        legend.text = element_text(size = 12, colour = "black"))
