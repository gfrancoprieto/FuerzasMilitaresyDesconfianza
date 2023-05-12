#Crear gráficas para describir las variables

#Confianza en el Gobierno, por sexo
#Confianza en las Fuerzas Militares, por sexo
# Frecuencia de Miedo a la Violencia o a Delitos

library(ggplot2)

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(tidyverse) 
library(dplyr) 
library(stats) 
library(survey) 
library(vcd)
library(haven)
install.packages("haven")

#2. Directorio de trabajo
setwd("/Users/gabrielafranco/Desktop/FINAL_R/Datos")


#3. Cargar base de datos
latinobarometro <- readRDS("Latinobarometro_2020_Esp_Rds_v1_0.rds")

library(dplyr)
library(ggplot2)

latino2020 <- subset(latinobarometro, P13STGBS_A %in% c(1, 2, 3, 4))
latino2020<- subset(latinobarometro, p13st_e %in% c(1, 2, 3, 4)) 
latino2020<- subset(latinobarometro, p65st %in% c(1, 2, 3, 4))

latino2020$sexo<- as.factor(latino2020$sexo)

ggplot(latino2020, aes(x=sexo, y=P13STGBS_A,color=sexo))+
  geom_count()+
  labs(title="Confianza en las Fuerzas Militares por Sexo en América Latina",x="Sexo", y="Nivel de Confianza") + theme_light()

level_P13STGBS_A<- c("4","3","2","1")
tag_P13STGBS_A<- c("Ninguna", "Poca", "Algo", "Mucha")

level_sexo<- c("1","2")
tag_sexo<- c("Hombre", "Mujer")

latino2020$P13STGBS_A<-factor(latino2020$P13STGBS_A, levels=level_P13STGBS_A, labels= tag_P13STGBS_A)
latino2020$sexo<-factor(latino2020$sexo, levels=level_sexo, labels= tag_sexo)

ggplot(latino2020, aes(x=sexo, y=p13st_e,color=sexo))+
  geom_count()+
  labs(title="Confianza en el Gobierno por Sexo en América Latina",x="Sexo", y="Nivel de Confianza") + theme_light()

level_p13st_e<- c("4","3","2","1")
tag_p13st_e<- c("Ninguna", "Poca", "Algo", "Mucha")

latino2020$p13st_e<-factor(latino2020$p13st_e, levels=level_p13st_e, labels= tag_p13st_e)
latino2020$sexo<-factor(latino2020$sexo, levels=level_sexo, labels= tag_sexo)

level_p65st<- c("4","3","2","1")
tag_p65st<- c("Nunca", "Ocasionalmente", "Algunas veces", "Todo o casi todo el tiempo")

latino2020$p65st<-factor(latino2020$p65st, levels=level_p65st, labels= tag_p65st)
latino2020$sexo<-factor(latino2020$sexo, levels=level_sexo, labels= tag_sexo)

ggplot(latino2020, aes(x=sexo, y=p65st,color=sexo))+
  geom_count()+
  labs(title="Frecuencia de preocupación de ser víctimas de delitos o violencia en América Latina",x="Sexo", y="Frecuencia de Preocupación") + theme_light()
