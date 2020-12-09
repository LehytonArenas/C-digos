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

# colores de gráficos: dodgerblue2, cyan4
# Importamos la data
ECV2019_Ant <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Antioquia CCM/Entregables_Lehyton/3_Caracterización_v4.xlsx")
# Eliminamos decimales de todas las columnas:
ECV2019_Ant$TBN <- round(ECV2019_Ant$TBN,2)
ECV2019_Ant$TNM <- round(ECV2019_Ant$TNM,2)
ECV2019_Ant$ICVM <- round(ECV2019_Ant$ICVM,2)
ECV2019_Ant$SGSS <- round(ECV2019_Ant$SGSS,2)
ECV2019_Ant$Ingresos <- round(ECV2019_Ant$Ingresos,2)
ECV2019_Ant$TNE17_21 <- round(ECV2019_Ant$TNE17_21,2)
ECV2019_Ant$Informalidad <- round(ECV2019_Ant$Informalidad,2)
ECV2019_Ant$EmpleoFormal <- round(ECV2019_Ant$EmpleoFormal,2)
ECV2019_Ant$LP <- round(ECV2019_Ant$LP,2)
ECV2019_Ant$LI <- round(ECV2019_Ant$LI,2)
ECV2019_Ant$R.Contributivo <- round(ECV2019_Ant$R.Contributivo,2)
ECV2019_Ant$R.Subsidiado <- round(ECV2019_Ant$R.Subsidiado,2)
ECV2019_Ant$Desempleo <- round(ECV2019_Ant$Desempleo,2)
ECV2019_Ant$Ocupación <- round(ECV2019_Ant$Ocupación,2)
ECV2019_Ant$TNE5 <- round(ECV2019_Ant$TNE5,2)
ECV2019_Ant$TNE6_10 <- round(ECV2019_Ant$TNE6_10,2)
ECV2019_Ant$TNE11_14 <- round(ECV2019_Ant$TNE11_14,2)
ECV2019_Ant$TNE15_16 <- round(ECV2019_Ant$TNE15_16,2)
ECV2019_Ant$TA15 <- round(ECV2019_Ant$TA15,2)
ECV2019_Ant$Bilinguismo <- round(ECV2019_Ant$Bilinguismo,2)
ECV2019_Ant$C.R.Contributivo <- round(ECV2019_Ant$C.R.Contributivo,2)
str(ECV2019_Ant)



#### Gráficas de 1 variable por cada eje: ####

# Creemos la función para crear gráficos.
# La función dependerá del Polo e indicador solicitado.
grafico <- function(PoloX, Indicador, Indicador_str){
  grafico1 <- ggplot(data=dplyr::filter(ECV2019_Ant,Eje_v2==PoloX | Eje_v2=="Antioquia"),
                     aes(y=Municipio, x={{Indicador}}))+
    geom_bar(stat="identity", position="dodge", fill="cyan4")+
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
Polos <- c("Eje Cauca","Eje Urabá y Occidente", "Eje Centro", "Eje Magdalena Medio", "Eje Suroeste Uraba Pacifico")
# Se guardarán los archivos en el directorio declarado inicialmente, con formato tiff y dimensiones 800*600
# Haciento iteraciones por Polo
# Dejando fijo el indicador. 
# Aplicación para: C.R.Contributivo
for (i in Polos) { 
    tiff(filename = paste("Grafico_",c(i),"_C.R.Contributivo_",".tiff",sep = ""), # Cambiar indicador a gusto
         width = 800, height = 600) 
    grafico2 <- grafico(PoloX=i,
                        Indicador=C.R.Contributivo, # Cambiar indicador a gusto
                        Indicador_str="C.R.Contributivo") # Cambiar indicador a gusto
    dev.off()  
}


#### Gráficas de 2 variables por cada eje: ####
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
    geom_text(aes(label = Valor),position = position_dodge(0.9),
              size=4, vjust=0.5, hjust=0 ,col="black")+
    ylab(NombEjeStr)+
    theme (axis.text.x = element_text(size=rel(1.5)),
           axis.text.y = element_text(size=rel(1.5)))
  print(grafico2vbles)
}

#3.B. Sin Aplicar el dodge en la posición de columnas
Grafico_2vbles_v2 <- function(Data_Tr,NombEjeStr){
  grafico2vbles <- ggplot(Data_Tr, aes(x = Valor, y = Municipio, fill = Indicador))+
    geom_bar(stat="identity")+
    geom_text(aes(label = Valor),position = position_stack(0.2),
              size=4, vjust=0.5, hjust=0 ,col="black")+
    ylab(NombEjeStr)+
    theme (axis.text.x = element_text(size=rel(1.5)),
           axis.text.y = element_text(size=rel(1.5)))
  print(grafico2vbles)
}


# Apliquemos los  pasos anteriores con una iteración: Graficos tipo 3.A
# Haciento iteraciones por Polo
# Dejando fijo los indicadores. 
# Aplicación para: Bilinguismo y TA
for (i in Polos) { 
  tiff(filename = paste("Grafico_",c(i),"_Bilinguismo-TA_",".tiff",sep = ""), # Cambiar indicadores a gusto
       width = 800, height = 600) 
  Data_DosVbles_Eje <- Data_DosVbles(PoloX=i,
                                       Indicador1=TA15, # Cambiar indicadores a gusto
                                       Indicador2=Bilinguismo) # Cambiar indicadores a gusto
  
  DataTransformada_Eje <- DataTransformada(Data=Data_DosVbles_Eje)
  
  Grafico2Vbles_Eje <- Grafico_2vbles(Data_Tr=DataTransformada_Eje,
                                        NombEjeStr=i)
   dev.off()  
}

# Aplicamos una iteración para el grafico tipo 3.B
# Solo para las vbles de Contributivo y Subsidiado:
for (i in Polos) { 
  tiff(filename = paste("Grafico_",c(i),"_RContri-RSubsid_v2_",".tiff",sep = ""), # Cambiar indicadores a gusto
       width = 800, height = 600) 
  Data_DosVbles_Eje <- Data_DosVbles(PoloX=i,
                                     Indicador1=R.Contributivo, # Cambiar indicadores a gusto
                                     Indicador2=R.Subsidiado) # Cambiar indicadores a gusto
  
  DataTransformada_Eje <- DataTransformada(Data=Data_DosVbles_Eje)
  
  Grafico2Vbles_Eje <- Grafico_2vbles_v2(Data_Tr=DataTransformada_Eje,
                                      NombEjeStr=i)
  dev.off()  
}


#### Graficos de 1 vble: entre ejes ####
# Aquí no aplica la función, entonces se exporta un gráfico por indicador

tiff(filename = "GraficoEjes_TNM.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(data=dplyr::filter(ECV2019_Ant,
                          Municipio=="Antioquia" | Municipio=="Eje Cauca" |  Municipio=="Eje Urabá y Occidente" | Municipio=="Eje Centro" | Municipio=="Eje Magdalena Medio" |  Municipio=="Eje Suroeste Uraba Pacifico" ),
       aes(y=Municipio, x=TNM))+ # Cambiar indicador a gusto
  geom_bar(stat="identity", position="dodge", fill="cyan4")+
  ylab("Ejes")+
  xlab("TNM")+ # Cambiar indicador a gusto
  geom_text(aes(label = TNM), # Cambiar indicador a gusto
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off() 

#### Graficos de 2 vbles entre ejes: ####
# Recordemos que estos graficos los hacemos en 3 pasos:
# Cambiaremos los indicadores donde sea necesario
#1. Aplicamos filtros por Polo e indicadores
DosVbles <- ECV2019_Ant %>% 
    dplyr::filter(.,Municipio=="Antioquia" | Municipio=="Eje Cauca" |  Municipio=="Eje Urabá y Occidente" | Municipio=="Eje Centro" | Municipio=="Eje Magdalena Medio" |  Municipio=="Eje Suroeste Uraba Pacifico") %>% 
    dplyr::select(.,"Municipio","TNE11_14","TNE15_16") # cambiar indicadores

#2. Transformamos la data a tipo Long
Data_long <- DosVbles %>% 
    gather("Indicador", "Valor", -Municipio) 

# 3A. Grafico dos variables, tipo 3A:Con doge
tiff(filename = "GraficoEjes_TNE1114-TNE1516.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label = Valor),position = position_dodge(0.9),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  ylab("Ejes")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off() 

# 3B. Grafico dos variables, tipo 3B: Sin dodge
tiff(filename = "GraficoEjes_RContrib-RSubsid.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Valor),position = position_stack(0.2),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  ylab("Ejes")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off() 





