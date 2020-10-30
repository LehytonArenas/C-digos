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

# Hasta aquí vamos bien. Sigamos con las pruebas del For.

# Ahora intentemos crear un For para crear tantos gráficos como sean necesarios.
# Creamos unos vectores iniciales de prueba de Polos e Indicadores
Polos <- c("Polo 1 Urabá y Occidente", "Polo 2 Bajo Cauca y Norte del Altiplano")
Indicadores <- c("ICVM","TNM")

# Prueba 1 del FOR:
# Muy bien, esta es la estructura del FOR que necesito
for (i in Polos) { 
  for (j in Indicadores) { 
    print(c(i,j))
  }
}

for (i in Polos) { 
    print(paste("Grafico_", c(i))) # error corregido
  }

for (i in Polos) { 
  for (j in Indicadores) { 
    print(paste("Grafico_", c(i),c(j))) # error corregido
  }
}


# Prueba 2 del FOR:
# Bien, este sería el FOR aplicado a la función del gráfico
for (i in Polos) { 
  for (j in Indicadores) { 
    tiff(filename = "prueba.tiff", width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=j,
                        Indicador_str=j)
    dev.off()  
  }
}

# Prueba 3 del FOR:
# Ruta de descarga: /Users/lehyton/Downloads
for (i in Polos) { 
  for (j in Indicadores) { 
    mypath <- file.path("Users","lehyton","Downloads",paste("Grafico_", c(i),c(j), ".jpeg", sep = ""))
    jpeg(file=mypath, width = 800, height = 600)
    grafico2 <- grafico(PoloX=i,
                        Indicador=j,
                        Indicador_str=j)
    dev.off()  
  }
}

# Prueba 4 del FOR:
# COn el directorio declarado inicialmente
# Con este me genera los gráficos, pero los gráficos están mal.
for (i in Polos) { 
  for (j in Indicadores) { 
    tiff(filename = paste("Grafico_", c(i),c(j), ".tiff", sep = ""), 
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=j, # Aquí está el problema
                        Indicador_str=j)
    dev.off()  
  }
}

# Prueba 5 del FOR:
# COn el directorio declarado inicialmente
# Con este me genera los gráficos, pero los gráficos están mal.
ECV2019_Ant <- data.frame(ECV2019_Ant)

for (i in Polos) { 
  for (j in Indicadores) { 
    tiff(filename = paste("Grafico_", c(i),c(j), ".tiff", sep = ""), 
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=data.frame(ECV2019_Ant[j]), # Aquí está el problema
                        Indicador_str=j)
    dev.off()  
  }
}


# Prueba 6 del FOR:
# COn el directorio declarado inicialmente
# Haciento iteraciones por Polo
# Dejando fijo el indicador. Ejm: ICVM
# Con este me genera los gráficos, pero los gráficos están mal.
ECV2019_Ant <- data.frame(ECV2019_Ant)

for (i in Polos) { 
    tiff(filename = paste("Grafico_",c(i),"ICVM",".tiff",sep = ""), # Cambiar indicador a gusto
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=ICVM, # Cambiar indicador a gusto
                        Indicador_str="ICVM") # Cambiar indicador a gusto
    dev.off()  
}


