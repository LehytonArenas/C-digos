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
Datos <- read_excel("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Ecsim/2020_Proyecto Centralidad Cartama/Entregables_Lehyton/Datos.xlsx")
# Eliminamos decimales:
Datos[,3:33] <- round(Datos[,3:33],2)

# PIB Per cápita:
Economia <- read_excel("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Ecsim/2020_Proyecto Centralidad Cartama/Entregables_Lehyton/PIB_Municipios.xlsx", sheet = "PIB_Percapita")
# Organizamos
Economia <- Economia[1:14,]
Economia[,5] <- round(Economia[,5],2)

# Producción:
Produccion <- read_excel("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Ecsim/2020_Proyecto Centralidad Cartama/Entregables_Lehyton/PIB_Municipios.xlsx", sheet = "ValorAgregadoActividad (3)")
# Eliminamos decimales:
Produccion[,3:15] <- round(Produccion[,3:15],2)

#### Gráficos de 1 vble ####
# colores de gráficos: dodgerblue2, cyan4
tiff(filename = "Grafico_CRContributivo.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)

ggplot(data=Datos,
       aes(y=Municipio, x=Ingresos))+ # Cambiar indicador a gusto
  geom_bar(stat="identity", position="dodge", fill="cyan4")+
  ylab("Municipios")+
  xlab("Ingresos población ocupada (miles)")+ # Cambiar indicador a gusto
  geom_text(aes(label = Ingresos), # Cambiar indicador a gusto
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme(axis.title=element_text(size=rel(1.3)),
         axis.text.x = element_text(size=rel(1.3)),
         axis.text.y = element_text(size=rel(1.3)))+
  scale_x_continuous(limits=c(0,1060))

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
DosVbles <- Datos %>% 
  dplyr::select(.,c(Municipio,TNE5,TNE6_10,TNE11_14,TNE15_16,TNE17_21)) %>%  # cambiar indicadores
  dplyr::rename("TNE 5 años"=TNE5,"TNE 6-10 años"=TNE6_10,
                "TNE 11-14 años"=TNE11_14,"TNE 15-16 años"=TNE15_16,
                "TNE 17-21 años"=TNE17_21) # cambiar indicadores

#2. Transformamos la data a tipo Long
Data_long <- DosVbles %>% 
  gather("Indicador:", "Valor", -Municipio)

# Paso opcional: poner data en factores para un orden establecido:
Data_long$Indicador <- factor(Data_long$`Indicador:`, levels=c("TNE 5 años","TNE 6-10 años","TNE 11-14 años","TNE 15-16 años","TNE 17-21 años"))

# 3A. Grafico +2 variables, tipo 3A: Con dodge: columna a lado y lado
tiff(filename = "Grafico_TNE.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)

ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="Indicadores: Tasa Neta Escolaridad (TNE)",y="Municipios",fill="Indicador:")+
  geom_text(aes(label = Valor),position = position_dodge(1),
            size=4.4, vjust=0.4, hjust=-0.05 ,col="black")+
  theme(axis.title=element_text(size=rel(1.5)),
        axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        legend.title=element_text(size=rel(1.3)),
        legend.text=element_text(size=rel(1)),
        legend.position="top"
        )+
  scale_x_continuous(limits=c(0,100))+
  scale_fill_brewer(palette=1)

dev.off() 

# 3B. Grafico +2 variables, tipo 3B: Columnas apiladas: Sin dodge
tiff(filename = "Grafico_Contributivo-Subsidiado.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)

ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity")+
  labs(x="Indices: Empleo formal e Informal",y="Municipios",fill="Indicador")+
  geom_text(aes(label = Valor),position = position_stack(0.2),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme(axis.title=element_text(size=rel(1.3)),
        axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        legend.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(0.9)),
        legend.position="top")+
  scale_x_continuous(limits=c(0,100))

dev.off()

# 3C. Grafico +2 variables, tipo 3C: Con identity, columnas solapadas
tiff(filename = "Grafico_LP-LI.tiff", # Cambiar indicador a gusto
     width = 800, height = 600)

ggplot(Data_long, aes(x = Valor, y = Municipio, fill = Indicador))+
  geom_bar(stat="identity",position=position_identity())+
  labs(x="Indicadores: Pobreza e Indigencia",y="Municipios",fill="Indicador")+
  geom_text(aes(label = Valor),position = position_dodge(width=0),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme(axis.title=element_text(size=rel(1.3)),
        axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        legend.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(0.9)),
        legend.position="top")+
  scale_x_continuous(limits=c(0,52))

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





#### Gráfico Producción ####

# data tipo long:
Data_long <- Produccion %>% 
  gather("Indicador:", "Valor", -Municipio)

# Filtro por municipio de interés
Data_long <- Data_long %>% 
  dplyr::filter(Municipio=="Fredonia")

# Codifico según corresponda
Data_long$Columna <- c("Columna")

Data_long <- Data_long %>% 
  dplyr::mutate(Columna=ifelse(`Indicador:`=="Agricultura", "Agricultura",Columna),
                Columna=ifelse(`Indicador:`=="Explotación_Minas", "Explotación Minas",Columna),
                Columna=ifelse(`Indicador:`=="Industria_Manufacturera", "Industria Manufacturera",Columna),
                Columna=ifelse(`Indicador:`=="Suministro_Electricidad", "Suministro Electricidad",Columna),
                Columna=ifelse(`Indicador:`=="Construcción", "Construcción",Columna),
                Columna=ifelse(`Indicador:`=="ComercioYTransporte", "Comercio Y Transporte",Columna),
                Columna=ifelse(`Indicador:`=="InformaticaYComunicaciones", "Informatica Y Comunicaciones",Columna),
                Columna=ifelse(`Indicador:`=="ActividadesFinancieras", "Actividades Financieras",Columna),
                Columna=ifelse(`Indicador:`=="ActividadesInmobiliarias", "Actividades Inmobiliarias",Columna),
                Columna=ifelse(`Indicador:`=="ActividadesProfesionales", "Actividades Profesionales",Columna),
                Columna=ifelse(`Indicador:`=="AdministraciónPública", "Administración Pública",Columna),
                Columna=ifelse(`Indicador:`=="ActividadesArtísticas", "Actividades Artísticas",Columna),
                ) %>% 
  dplyr::filter(Columna!="Columna")

# Tranformamos los datos a tipo numérico
Data_long$Valor <- as.numeric(Data_long$Valor)

# Graficamos:
ggplot(Data_long,aes(x=Valor,y=reorder(Columna,-Valor)))+
  geom_col(fill="cyan4")+
  labs(x="Participación (%) del valor agregado",y="Fredonia: Sectores productivos")+
  geom_text(aes(label = Valor),position = position_stack(1),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme(axis.title=element_text(size=rel(1)),
        axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)))+
  scale_x_continuous(limits=c(0,40))

#### Gráfico PIB ####

# Tranformamos los datos a tipo numérico
Economia$PIB_PC <- as.numeric(Economia$PIB_PC)

# Graficamos:
ggplot(Economia,aes(x=PIB_PC,y=Municipio))+
  geom_col(fill="coral")+
  labs(x="PIB per cápita (miles COP)",y="Municipios")+
  geom_text(aes(label = PIB_PC),position = position_stack(1),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme(axis.title=element_text(size=rel(1.3)),
        axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)))+
  scale_x_continuous(limits=c(0,30000))
