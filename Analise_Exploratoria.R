#-----------------------------------
#-----------------------------------
#--- EDA (Exploratory data analysis)
#-----------------------------------
#-----------------------------------


banco %>%
  mutate(data_coleta = as.Date(paste(Ano, Mes, "01", sep = "-"))) %>%
  select(Zona, data_coleta, Cota, Profundidade, Temperatura:Amonia) %>%
  gather(key = "Variavel", value = "Valor", -data_coleta, -Zona) %>%
  ggplot(data = .) +
  geom_boxplot(aes(x = Variavel, y = Valor, color = Zona), size = 1.05) +
  facet_wrap(~ year(data_coleta), scales = "free") +
  theme_bw() +
  theme(legend.position = "right",
        axis.title.y = element_text(colour = "black", face = "bold", size = 16),
        axis.title.x = element_text(colour = "black", face = "bold", size = 16),
        axis.text = element_text(colour = "black", size = 13),
        strip.text.x = element_text(size = 14)) +
  labs(x = "Data de coleta", y = "Valor") +
  scale_color_brewer(palette = "Dark2")


#---- Correlação



cor_banco <-
  cor(banco %>% select(Cota, `Profundidade`, Temperatura:`Amonia`),
                 use = "na.or.complete")


p_valor_cor <-
  rcorr(as.matrix(banco %>% select(Cota, `Profundidade`,
                                   Temperatura:`Amonia`)),
        type = "pearson")$P


get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


p_valor_cor_banco <-
  get_lower_tri(cormat = p_valor_cor) %>%
  melt(data = ., na.rm = T)


coef_cor_banco <-
  get_lower_tri(cormat = cor_banco) %>%
  melt(data = ., na.rm = T) %>%
  mutate(value = round(x = value, digits = 2))


merge(coef_cor_banco, p_valor_cor_banco, by = c("Var1", "Var2"),
      all = T) %>%
  magrittr::set_colnames(c("Var1", "Var2", "Coeficiente", "p_valor")) %>%
  mutate(p_valor = ifelse(test = is.na(p_valor), yes = 1,
                          no = p_valor)) %>%
  ggplot(aes(Var1, Var2, fill = Coeficiente)) +
  geom_tile(color = "black")+
  geom_text(aes(label = formato_real_graf(values = Coeficiente,
                                          nsmall = 2)), size = 6) +
  geom_text(aes(label = ifelse(test = p_valor <= 0.5, yes = "*",
                               no = " ")),
            size = 6, vjust = 2.2, hjust = 0.3) +
  scale_fill_gradient2(low = "#6D9EC1", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Coeficiente de\ncorrelação",
                       labels = formato_real_graf,
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black")) +
  labs(x = "", y = "", caption = "* significante ao nível de 5%") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 16, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  coord_fixed()


ggsave(filename = "Gráficos/Correlation1.pdf", device = "pdf",
       width = 11.57, height = 11.57, units = "in", dpi = "retina")



#--- Correlação banco_variaveis_ln


cor_banco_variaveis_ln <-
  cor(banco_variaveis_ln %>% select(Cota, `Profundidade`,
                                    Temperatura:`Amonia`),
      use = "na.or.complete")


p_valor_cor <-
  rcorr(as.matrix(banco_variaveis_ln %>% select(Cota, `Profundidade`,
                                                Temperatura:`Amonia`)),
        type = "pearson")$P



p_valor_cor_banco_variaveis_ln <-
  get_lower_tri(cormat = p_valor_cor) %>%
  melt(data = ., na.rm = T)


coef_cor_banco_variaveis_ln <-
  get_lower_tri(cormat = cor_banco_variaveis_ln) %>%
  melt(data = ., na.rm = T) %>%
  mutate(value = round(x = value, digits = 2))


merge(coef_cor_banco_variaveis_ln, p_valor_cor_banco_variaveis_ln,
      by = c("Var1", "Var2"), all = T) %>%
  magrittr::set_colnames(c("Var1", "Var2", "Coeficiente", "p_valor")) %>%
  mutate(p_valor = ifelse(test = is.na(p_valor), yes = 1,
                          no = p_valor)) %>%
  ggplot(aes(Var1, Var2, fill = Coeficiente)) +
  geom_tile(color = "black")+
  geom_text(aes(label = formato_real_graf(values = Coeficiente,
                                          nsmall = 2)), size = 6) +
  geom_text(aes(label = ifelse(test = p_valor <= 0.5, yes = "*",
                               no = " ")), size = 6, vjust = 2.2,
            hjust = 0.3) +
  scale_fill_gradient2(low = "#6D9EC1", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Coeficiente de\ncorrelação",
                       labels = formato_real_graf,
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 16,
                                   hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  coord_fixed()


ggsave(filename = "Gráficos/Correlation_Variaveis_Ln.pdf", device = "pdf",
       width = 11.57, height = 11.57, units = "in", dpi = "retina")




#---- Boxplot



banco %>%
  select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
  gather(key = "Variavel", value = "Valor", -Zona) %>%
  ggplot(data = ., aes(x = Zona, y = Valor)) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552",
               outlier.colour = "#1F3552", outlier.shape = 20) +
  stat_summary(fun.y = mean, geom = "point", size = 1.5) +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  facet_wrap(~ Variavel, scales = "free", nrow = 3) +
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



ggsave(filename = "Gráficos/Boxplot.pdf", device = "pdf", width = 11.57,
       height = 6, units = "in", dpi = "retina")




# banco_variaveis %>%
#  count(Zona) %>%
#  mutate(perc = n/sum(n)*100) %>%
#  write.xlsx(x = ., file = "Tabelas/Proporc_Zonas.xlsx")


#--- Histograma


banco %>%
  select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
  gather(key = "Variavel", value = "Valor", -Zona) %>%
  ggplot(data = .) +
  geom_histogram(aes(x = Valor, fill = Zona), color = "black", alpha = 0.7, bins = 20) +
  facet_wrap(~ Variavel, scales = "free", nrow = 3) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.y = element_text(colour = "black", face = "bold", size = 12),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 12),
        strip.text.x = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14, colour = "black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black")) +
  labs(x = "Valor", y = "Frequência") +
  scale_fill_brewer(palette = "Dark2")





ggsave(filename = "Gráficos/Histograma.pdf", device = "pdf", width = 11.57,
       height = 6, units = "in", dpi = "retina")



#--- Tabela descritiva


my_summary <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

sd_all_variables <-
  banco %>%
  select(Cota, `Profundidade`, Temperatura:`Amonia`) %>%
  summarise_all(sd, na.rm = T) %>%
  as_vector()

tabela_descritiva <-
  do.call(cbind, lapply(banco %>%
                          select(Cota, `Profundidade`,
                                 Temperatura:`Amonia`),
                        my_summary)) %>%
  t %>%
  as.data.frame




tabela_descritiva$`Desvio Padrão` <- sd_all_variables

#tabela_descritiva %>%
#  select(Min.:Mean, `Desvio Padrão`, `3rd Qu.`:`NA's`) %>%
#  write.xlsx(x = ., file = "Tabelas/Descritiva.xlsx")



#cbind(
#  banco %>%
#    select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
#    group_by(Zona) %>%
#    mutate_all(mean, na.rm = T) %>%
#    slice(1) %>%
#    t %>%
#    as.data.frame
#
#  ,
#
#  banco %>%
#    select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
#    group_by(Zona) %>%
#    mutate_all(sd, na.rm = T) %>%
#    slice(1) %>%
#    t %>%
#    as.data.frame
#) %>%
#  magrittr::set_colnames(c(paste0("Media", 1:3),
#  paste0("Desvio", 1:3))) %>%
#  xlsx::write.xlsx(x = ., file = "Tabelas/Media_Desvio1.xlsx")



#--- Tabela descritiva (banco_ln)



sd_all_variables <-
  banco_variaveis_ln %>%
  select(Cota, `Profundidade`, Temperatura:`Amonia`) %>%
  summarise_all(sd, na.rm = T) %>%
  as_vector()

tabela_descritiva <-
  do.call(cbind, lapply(banco_variaveis_ln %>%
                          select(Cota, `Profundidade`,
                                 Temperatura:`Amonia`),
                        my_summary)) %>%
  t %>%
  as.data.frame




tabela_descritiva$`Desvio Padrão` <- sd_all_variables

#tabela_descritiva %>%
#  select(Min.:Mean, `Desvio Padrão`, `3rd Qu.`:`NA's`) %>%
#  write.xlsx(x = ., file = "Tabelas/Descritiva.xlsx")



#cbind(
#  banco_variaveis_ln %>%
#    select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
#    group_by(Zona) %>%
#    mutate_all(mean, na.rm = T) %>%
#    slice(1) %>%
#    t %>%
#    as.data.frame
#
#  ,
#
#  banco_variaveis_ln %>%
#    select(Zona, Cota, `Profundidade`, Temperatura:`Amonia`) %>%
#    group_by(Zona) %>%
#    mutate_all(sd, na.rm = T) %>%
#    slice(1) %>%
#    t %>%
#    as.data.frame
#) %>%
#  magrittr::set_colnames(c(paste0("Media", 1:3),
#  paste0("Desvio", 1:3))) %>%
#  xlsx::write.xlsx(x = ., file = "Tabelas/Media_Desvio_LN.xlsx")
