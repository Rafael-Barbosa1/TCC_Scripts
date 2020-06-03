#-------------------------

#--- Assunto: Script_TCC.R


#--- Objetivo: Classificar zonas de um reservatório através de um modelo
# estatístico e de aprendizado de máquina.


#--- Autor: Rafael Barbosa da Silva
#--- e-mail: lul.rafaelbarbosa@gmail.com


#--- Data de criação: 05/08/2019

#-------------------------


#--- Limpando o R


rm(list = ls())

options(digits = 4)


#--- Pacotes necessários


if(!require(MASS)) {
  install.packages("MASS", dependencies = T);
  require(MASS)
}


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(magrittr)) {
  install.packages("magrittr", dependencies = T);
  require(magrittr)
}


if(!require(caret)) {
  install.packages("caret", dependencies = T);
  require(caret)
}


if(!require(openxlsx)) {
  install.packages("openxlsx", dependencies = T);
  require(openxlsx)
}



if(!require(xlsx)) {
  install.packages("xlsx", dependencies = T);
  require(xlsx)
}


if(!require(class)) {
  install.packages("class", dependencies = T);
  require(class)
}


if(!require(psych)) {
  install.packages("psych", dependencies = T);
  require(psych)
}


if(!require(car)) {
  install.packages("car", dependencies = T);
  require(car)
}


if(!require(MVTests)) {
  install.packages("MVTests", dependencies = T);
  require(MVTests)
}


if(!require(MVN)) {
  install.packages("MVN", dependencies = T);
  require(MVN)
}


if(!require(Hmisc)) {
  install.packages("Hmisc", dependencies = T);
  require(Hmisc)
}


if(!require(lubridate)) {
  install.packages("lubridate", dependencies = T);
  require(lubridate)
}


if(!require(reshape2)) {
  install.packages("reshape2", dependencies = T);
  require(reshape2)
}



#--- Diretório


setwd("F:/UFPA/TCC/UHE_Tucurui_Classificacao")


#--- Funções adicionais

#- Formatando o valor como moeda

# https://pt.stackoverflow.com/questions/216852/adicionar-nota%C3%A7%C3%A3o-de-moeda-em-r

formato_real_graf <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim()
}


#--- Carregando o banco

banco <- openxlsx::read.xlsx(xlsxFile = "dataset.xlsx")

colnames(banco) <- c("ID", "Place", "Codigo_do_ciclo", "Zona", "Cota",
                     "Profundidade", "Mes", "Ano", "Temperatura",
                     "Fosforo_total", "Clorofila", "Transparencia",
                     "Turbidez", "STS", "Oxigênio_dissolvido",
                     "Ortofosfato", "Amonia")

banco %>%
  glimpse


#--- Manuseio do banco (Zona --> 0: Lacustre, 1: Transição, 2: Fluvial)


banco <-
  banco %>%
  mutate(Zona = case_when(Zona == 0 ~ "Lacustre",
                          Zona == 1 ~ "Transição",
                          Zona == 2 ~ "Fluvial")) %>%
  mutate(Zona = factor(x = Zona, levels = c("Fluvial",
                                            "Transição",
                                            "Lacustre")))


#--- 1: Fluvial, 2: Lacustre e 3: Transição


banco_variaveis <-
  banco %>%
  dplyr::select(Zona, Cota, `Profundidade`, Temperatura:Amonia) %>%
  drop_na %>%
  mutate(Zona = factor(x = Zona, labels = c(1, 2, 3))) %>%
  mutate_if(is.numeric, scale)


banco_variaveis_ln <-
  banco %>%
  dplyr::select(Zona, Cota, `Profundidade`, Temperatura:Amonia) %>%
  drop_na %>%
  mutate(Zona = factor(x = Zona, labels = c(1, 2, 3))) %>%
  mutate_if(is.numeric, log)



#-------------------
#-------------------
#--- Treino e Teste
#-------------------
#-------------------


source("Treino_Teste.R")



#-----------------------------------
#-----------------------------------
#--- EDA (Exploratory data analysis)
#-----------------------------------
#-----------------------------------


source("Analise_Exploratoria.R")


#----------
#----------
#--- ANOVA
#----------
#----------


source("Anova_Modelagem.R")



#----------------------------
#----------------------------
#--- Análise de discriminante
#----------------------------
#----------------------------


source("Analise_Discriminante_Modelagem.R")


#------------------------
#------------------------
#--- K-nearest neighbors
#------------------------
#------------------------


source("KNN_Modelagem.R")

