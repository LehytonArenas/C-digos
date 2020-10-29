# Proyecto: Ctualización modelo economía dinámica departamento de Antioquia
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

# Gráficas
# Creemos la función
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

# Aplicamos la función.
tiff(filename = "prueba.tiff", width = 800, height = 600) # con esto exportamos el grafico a crear, se guardará en nuestro directorio
graficoprueba <- grafico(PoloX="Polo 1 Urabá y Occidente",
                         Indicador=ICVM,
                         Indicador_str="ICVM")
dev.off() # con esto limpiamos el visor de graficos para hacer más

graficoprueba2 <- grafico(PoloX="Polo 5 Suroeste",
                         Indicador=TNM,
                         Indicador_str="TNM")

graficoprueba3 <- grafico(PoloX="Polo 3 Magdalena Medio y Nordeste",
                          Indicador=Bilinguismo,
                          Indicador_str="Bilinguismo")
# Hasta ahora vamos bien

# Ahora intentemos crear un For
Polos <- c("Polo 1 Urabá y Occidente", "Polo 2 Bajo Cauca y Norte del Altiplano")
Indicadores <- c("ICVM","TNM")

for (i in Polos) { 
  for (j in Indicadores) { 
    tiff(filename = "prueba.tiff", width = 800, height = 600) 
    graficoprueba <- grafico(PoloX="i",
                             Indicador=j,
                             Indicador_str="j")
    dev.off()  
  }
}


