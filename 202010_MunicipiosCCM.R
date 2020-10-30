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
ECV2019_Ant <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Antioquia CCM/Entregables_Lehyton/3_Caracterización_v3DF.xlsx")
str(ECV2019_Ant)

# Trasformemos la data a tipo DF:
ECV2019_Ant <- data.frame(ECV2019_Ant)

# Gráficas
# Creemos la función para crear gráficos.
# La función dependerá del Polo e indicador solicitado.
grafico <- function(PoloX, Indicador, Indicador_str){
  grafico1 <- ggplot(data=dplyr::filter(ECV2019_Ant,Polo==PoloX | Polo=="Antioquia"),
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

Polos <- c("Polo 1 Urabá y Occidente", "Polo 2 Bajo Cauca y Norte del Altiplano")

# Se guardarán los archivos en el directorio declarado inicialmente, con formato tiff y dimensiones 800*600
# Haciento iteraciones por Polo
# Dejando fijo el indicador. Ejm: ICVM

for (i in Polos) { 
    tiff(filename = paste("Grafico_",c(i),"ICVM",".tiff",sep = ""), # Cambiar indicador a gusto
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=ICVM, # Cambiar indicador a gusto
                        Indicador_str="ICVM") # Cambiar indicador a gusto
    dev.off()  
}

# Hasta aquí todo Ok.
