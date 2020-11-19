# Proyecto: Actualización modelo de economía dinámica departamento de Antioquia
# Cliente: Camara de Comercio de Medellín.
# Tarea específica: Caracterización de los municipios del departamento.
# Fuente de Data: ECV Departamental 2019
# Directorio de trabajo
setwd("/Users/lehyton/Google Drive/Ecsim/Proyecto Antioquia CCM/Entregables_Lehyton")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)

# Importamos la data
ECV2019_Ant <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Antioquia CCM/Entregables_Lehyton/3_Caracterización_v3DF.xlsx", sheet = "Indicadores_V2")
str(ECV2019_Ant)

# Trasformemos la data a tipo DF:
ECV2019_Ant <- data.frame(ECV2019_Ant)

# Gráficas de 1 variable:

# Creemos la función para crear gráficos.
# La función dependerá del Polo e indicador solicitado.
grafico <- function(PoloX, Indicador, Indicador_str){
  grafico1 <- ggplot(data=dplyr::filter(ECV2019_Ant,Eje_v2==PoloX | Eje_v2=="Antioquia"),
                     aes(y=Municipio, x={{Indicador}}))+
    geom_bar(stat="identity", position="dodge", fill="dodgerblue2")+
    ylab(PoloX)+
    xlab(Indicador_str)+
    geom_text(aes(label = {{Indicador}}),
              size=4, vjust=0.5, hjust=0 ,col="black")+
    theme (axis.text.x = element_text(size=rel(1.5)),
           axis.text.y = element_text(size=rel(1.5)))
  
  print(grafico1)
}

# Ahora crearemos un For para crear tantos gráficos como sean necesarios.
# Creamos unos vectores iniciales de prueba de Polos
Polos <- c("Cauca","Urabá y Occidente", "Centro", "Magdalena Medio", "Suroeste Uraba Pacifico")
# Se guardarán los archivos en el directorio declarado inicialmente, con formato tiff y dimensiones 800*600
# Haciento iteraciones por Polo
# Dejando fijo el indicador. 
# Aplicación para: Ingresos
for (i in Polos) { 
    tiff(filename = paste("Grafico_",c(i),"Ingresos",".tiff",sep = ""), # Cambiar indicador a gusto
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=Ingresos, # Cambiar indicador a gusto
                        Indicador_str="Ingresos") # Cambiar indicador a gusto
    dev.off()  
}


# Gráficas de 2 variables:
# Creamos tres funciones en tres pasos:
#1. Aplicamos filtros por Polo e indicadores
Data_DosVbles <- function(PoloX,Indicador1,Indicador2){
  DosVbles <- ECV2019_Ant %>% 
    dplyr::filter(.,Eje_v2==PoloX| Eje_v2=="Antioquia") %>% 
    dplyr::select(.,"Municipio",{{Indicador1}},{{Indicador2}})
}

                                     
#2. Transformamos la data a tipo Long
DataTransformada <- function(Data){
  Data_long <- Data %>% 
    gather("Indicador", "Valor", -Municipio) 
}

# 3. Hacemos los gráficos
#3.A. Aplicando el dodge en la posición de columnas
Grafico_2vbles <- function(Data_Tr,NombEjeStr){
  grafico2vbles <- ggplot(Data_Tr, aes(x = Valor, y = Municipio, fill = Indicador))+
    geom_bar(stat="identity",position="dodge")+
    geom_text(aes(label = Valor),
              size=4, vjust=0, hjust=0 ,col="black")+
    ylab(NombEjeStr)+
    theme (axis.text.x = element_text(size=rel(1.5)),
           axis.text.y = element_text(size=rel(1.5)))
  print(grafico2vbles)
}

#3.B. Sin Aplicar el dodge en la posición de columnas
Grafico_2vbles_v2 <- function(Data_Tr,NombEjeStr){
  grafico2vbles <- ggplot(Data_Tr, aes(x = Valor, y = Municipio, fill = Indicador))+
    geom_bar(stat="identity")+
    ylab(NombEjeStr)+
    theme (axis.text.x = element_text(size=rel(1.5)),
           axis.text.y = element_text(size=rel(1.5)))
  print(grafico2vbles)
}


# Apliquemos los  pasos anteriores con una iteración:
# Haciento iteraciones por Polo
# Dejando fijo los indicadores. 
# Aplicación para: Formalidad e informalidad
for (i in Polos) { 
  tiff(filename = paste("Grafico_",c(i),"_Informalidad-EmpleoFormal_",".tiff",sep = ""), # Cambiar indicadores a gusto
       width = 800, height = 600) 
  Data_DosVbles_Eje <- Data_DosVbles(PoloX=i,
                                       Indicador1=Informalidad, # Cambiar indicadores a gusto
                                       Indicador2=EmpleoFormal) # Cambiar indicadores a gusto
  
  DataTransformada_Eje <- DataTransformada(Data=Data_DosVbles_Eje)
  
  Grafico2Vbles_Eje <- Grafico_2vbles(Data_Tr=DataTransformada_Eje,
                                        NombEjeStr=i)
   dev.off()  
}

# Aplicamos una iteración para el grafico tipo 2
# Solo para las vbles de Formalidad e informalidad:
for (i in Polos) { 
  tiff(filename = paste("Grafico_",c(i),"_Informalidad-EmpleoFormal_v2_",".tiff",sep = ""), # Cambiar indicadores a gusto
       width = 800, height = 600) 
  Data_DosVbles_Eje <- Data_DosVbles(PoloX=i,
                                     Indicador1=Informalidad, # Cambiar indicadores a gusto
                                     Indicador2=EmpleoFormal) # Cambiar indicadores a gusto
  
  DataTransformada_Eje <- DataTransformada(Data=Data_DosVbles_Eje)
  
  Grafico2Vbles_Eje <- Grafico_2vbles_v2(Data_Tr=DataTransformada_Eje,
                                      NombEjeStr=i)
  dev.off()  
}



