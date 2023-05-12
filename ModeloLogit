
#--------------------------------------------------------------------------------
# Tema:       Examen Final Logit
# Autor:      Gabriela Franco Prieto <gabrielafranco2@gmail.com>
# Fecha:      11-05-2023
# Datos:      latinobarometro 2020
#             https://www.latino2020barometro.org/latContents.jsp
#             Descargar: latinobarómetro 2020 .Rrds
# Github:     
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Cargar base de datos
#       4. Pregunta guia: # Cuales son los factores que influyen para que las personas 
#       en América Latina, le tengan confianza o no a las Fuerzas Militares?
#       5. Regresion logistica
#           5.1 Variable dependiente/objetivo
#           5.2 Variables independientes/covariables
#           5.3 Ajustar el modelo
#           5.4 Resultados: momios

#--------------------------------------------------------------------------------


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
latino2020 <- readRDS("Latinobarometro_2020_Esp_Rds_v1_0.rds")


#4. Pregunta guia

# Cuales son los factores que influyen para que las personas 
# en América Latina, le tengan confianza o no a las Fuerzas Militares?

#5. Regresion logistica

    #5.1 Variable dependiente/objetivo
  

#P13STGBS_A. Para cada uno de los grupos, instituciones o personas de la lista 
#¿cuánta confianza tiene usted en ellas?: Las Fuerzas Armadas (79)
#1. Mucha
#2. Algo
#3. Poca
#4. Ninguna 8. NS
#0. NR
#9. No aplica

    
    table(latino2020$P13STGBS_A)
    
    summary(latino2020$P13STGBS_A)
    
    view(latino2020)
    
    
    # Cambiar a numerico para crear la variable binaria
    class(latino2020$P13STGBS_A)
    latino2020$P13STGBS_A<-as.numeric(as.character(latino2020$P13STGBS_A))
    class(latino2020$P13STGBS_A)
    
    
    # Filtrar solo los casos validos
    base_modelo<- filter (latino2020, P13STGBS_A>0) 
    
    # Creacion de la variable binaria (0/1) 
    base_modelo$confianzaMil=0
    base_modelo$confianzaMil[base_modelo$P13STGBS_A==1 |base_modelo$P13STGBS_A==2]<-1 
    base_modelo$confianzaMil[base_modelo$P13STGBS_A==3 |base_modelo$P13STGBS_A==4]<-0
    table(base_modelo$confianzaMil)
    
    view(base_modelo)
    
    
    #5.2 Variables independientes/covariables
    
    #MIEDO VIOLENCIA P65ST. ¿Con qué frecuencia se preocupa Ud. de que pueda llegar a ser víctima de un delito con violencia? (313) 
    #1. Todo o casi todo el tiempo
    #2. Algunas veces
    #3. Ocasionalmente
    #4. Nunca 0. NS/NR
    
    table(base_modelo$p65st)
    
    #CONFIANZA GOB
    table(base_modelo$p13st_e)
    
    # Clase social subjetiva  (S1)
    table(base_modelo$s1)
    
    # SEXO
    table(base_modelo$sexo)
    
    # EDAD
    table(base_modelo$edad)
    
    #IGLESIA P13ST.C
    table(base_modelo$p13st_c)
    
    #POLICIA 
    table(base_modelo$P13STGBS_B)
    
    # Renombrar variables
    base_modelo<-base_modelo%>%rename(clase=s1, sexo=sexo, miedoviolencia=p65st, confianzaIgl=p13st_c, confianzaGob=p13st_e, confianzaPol=P13STGBS_B)
    
    
    # Seleccionar las variables
    base_modelo<-base_modelo%>%select(confianzaMil, clase, sexo, miedoviolencia, confianzaGob, confianzaIgl, confianzaPol,edad, wt, idenpa)
    
    
    # Filtrar casos validos
    base_modelo<-filter(base_modelo, clase>0, miedoviolencia>0 & confianzaGob>0 & confianzaIgl>0 & confianzaPol>0 & edad>0)
    
    # Etiquetar variables categoricas
    base_modelo$confianzaGob<-factor(base_modelo$confianzaGob, levels =c(1,2,3,4), labels=c("Mucha", "Algo", "Poco", "Nada"))
    base_modelo$confianzaIgl<-factor(base_modelo$confianzaIgl, levels =c(1,2,3,4), labels=c("Mucha", "Algo", "Poco", "Nada"))
    base_modelo$confianzaPol<-factor(base_modelo$confianzaPol, levels =c(1,2,3,4), labels=c("Mucha", "Algo", "Poco", "Nada"))
    base_modelo$sexo<-factor(base_modelo$sexo, levels =c(1,2), labels=c("Hombre", "Mujer"))
    base_modelo$clase<-factor(base_modelo$clase, levels =c(1,2,3,4,5), labels=c("Alta", "Media Alta","Media", "Media Baja", "Baja"))
    base_modelo$miedoviolencia<-factor(base_modelo$miedoviolencia, levels =c(1,2,3,4), labels=c("Todo o casi todo el tiempo", "Algunas veces", "Ocasionalmente", "Nunca"))
    base_modelo$confianzaMil<-factor(base_modelo$confianzaMil, levels =c(0,1), labels=c("No Confianza", "Confianza"))
    base_modelo$edad<-as.numeric(base_modelo$edad)
    
    
    #5.3 Ajustar el modelo
    
    reg.log <- glm(confianzaMil ~ confianzaGob + confianzaIgl + confianzaPol + sexo+ clase + edad + miedoviolencia, 
                   data = base_modelo, family = "binomial")
    summary(reg.log)
    
    
    #5.4 Resultados: momios
    
    momios<-exp(coefficients(reg.log))%>%data.frame()
    View(momios)

write.
#-------------------------------------------
# RETO-2:
# Ajusta un modelo de regresion para
# algun pais (IDENPA):
# IDENPA  32  Argentina
# IDENPA  76  Brasil
# IDENPA  724 Espana
# IDENPA  858 Uruguay
#-------------------------------------------

