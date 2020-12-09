# Proyecto: Proyecto de Centralidad en la Provincia del Cartama
# Tarea específica: Caracterización de los municipios de la Provincia del Cartama.
# Fuente de Data: ECV Departamental 2019

# Directorio de trabajo
setwd("/Users/lehyton/Google Drive/Ecsim/Proyecto Centralidad Cartama/Entregables_Lehyton")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)
library(scales)

#### Importamos los datos ####
# ECV Departamental:
Datos <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Centralidad Cartama/Entregables_Lehyton/Datos.xlsx")
# Eliminamos decimales:
Datos[,3:33] <- round(Datos[,3:33],2)

# PIB Per cápita:
Economia <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Centralidad Cartama/Entregables_Lehyton/PIB_Municipios.xlsx", sheet = "PIB_Percapita")
# Organizamos
Economia <- Economia[1:14,]
Economia[,5] <- round(Economia[,5],2)

# Producción:
Produccion <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Centralidad Cartama/Entregables_Lehyton/PIB_Municipios.xlsx", sheet = "ValorAgregadoActividad (3)")
# Eliminamos decimales:
Produccion[,3:15] <- round(Produccion[,3:15],2)

#### Gráficos de 1 vble ####
# colores de gráficos: dodgerblue2, cyan4
tiff(filename = "Grafico_Dependencia.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(data=Datos,
       aes(y=Municipio, x=Dependencia))+ # Cambiar indicador a gusto
  geom_bar(stat="identity", position="dodge", fill="dodgerblue2")+
  ylab("Municipios")+
  xlab("Indice de Dependencia")+ # Cambiar indicador a gusto
  geom_text(aes(label = Dependencia), # Cambiar indicador a gusto
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off()

tiff(filename = "Grafico_PIBPC.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(data=Economia,
       aes(y=Municipio, x=PIB_PC))+ # Cambiar indicador a gusto
  geom_bar(stat="identity", position="dodge", fill="dodgerblue2")+
  ylab("Municipios")+
  xlab("PIB per cápita (miles de pesos)")+ # Cambiar indicador a gusto
  geom_text(aes(label = PIB_PC), # Cambiar indicador a gusto
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off()

#### Gráficos de +2  vbles ####
# Se hacen en 3 pasos:
#1. Aplicamos filtros por Polo e indicadores
DosVbles <- Produccion %>% 
  dplyr::select(.,-c(Eje,TotalValorAgregado)) # cambiar indicadores

#2. Transformamos la data a tipo Long
Data_long <- DosVbles %>% 
  gather("Sector", "Participación", -Municipio) 

# 3A. Grafico +2 variables, tipo 3A: Con dodge: columna a lado y lado
tiff(filename = "Grafico_InseguridadAlimentaria.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label = Valor),position = position_dodge(0.9),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  ylab("Municipios")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off() 

# 3B. Grafico +2 variables, tipo 3B: Columnas apiladas: Sin dodge
tiff(filename = "Grafico_Formal-Informal.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Valor),position = position_stack(0.2),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  ylab("Municipios")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off()

# 3C. Grafico +2 variables, tipo 3C: Con identity, columnas solapadas
tiff(filename = "Grafico_LP-LI.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Participación, y = Municipio, fill = Sector))+
  geom_bar(stat="identity",position=position_identity())+
  geom_text(aes(label = Participación),position = position_dodge(width=0),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  ylab("Municipios")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off()

# 3D. Grafico +2 variables, tipo 3D: Columnas apiladas Economía
tiff(filename = "Grafico_Economía.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)
ggplot(Data_long, aes(x = Participación, y = Municipio, fill = Sector))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Participación),position = position_stack(vjust=0.5),
            size=3, vjust=0.5, hjust=0 ,col="black")+
  ylab("Municipios")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))
dev.off()


#### Gráfico de torta (pie chart): Sectores económicos ####
# Este gráfico se hace en 3 pasos:
# 1. Filtro por municipio
Municipio <- Produccion %>% 
  dplyr::select(.,-c(Eje,TotalValorAgregado)) %>% 
  dplyr::filter(.,Municipio=="Provincia del Cartama") # cambiar municipio

# 2. Giramos la data
Municipio_2 <- Municipio %>% 
  gather("Sector", "Participación", -Municipio) 

# 3. Gráfico
tiff(filename = "Economia_ProvinciaCartama.tiff", # Cambiar nombre del gráfico
     width = 800, height = 600)
ggplot(data=Municipio_2,
       aes(x="Provincia del Cartama", y=Participación, fill=Sector))+ # cambiar municipio
  geom_bar(width = 1, stat = "identity", color="white")+
  geom_text(aes(label=percent(Participación/100,accuracy = 0.01)),
            position = position_stack(vjust=0.5),
            size=4, vjust=0.5, hjust=0.25 ,col="black")+
  theme (axis.title = element_text(size=rel(1)),
         axis.text.x = element_text(size=rel(1.5)))
dev.off()

# 4. Pie Chart: se descarta
# bp2 <- ggplot(data=Municipio_2,
#             aes(x="Fredonia", y=Participación, fill=Sector))+ 
#  geom_col(position="stack",width = 1, color="white")+
 # geom_text(aes(label=percent(Participación/100)),
  #          position = position_stack(vjust=0.5))+
#  theme_classic() +
 # theme(plot.title = element_text(vjust=0.2,hjust=0.5),
  #      axis.line = element_blank(),
   #     axis.text = element_blank(),
    #    axis.ticks = element_blank()) +
#  coord_polar("y")
# bp2




