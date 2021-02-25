# Directorio de trabajo
setwd("/Volumes/Respaldo/Google Drive-Actualizado/Ecsim/Proyecto Agosto/3_Entregables Lehyton")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)

#### Data: ICC 2018-2019-2020. ####
# Nota: En aquellos indcadores que se presente doble información debido a cambios en el año base, se dejará el indicador con el año base más reciente
# 1. Creamos data por indicador.
# 2. Creamos DF completo.

# Autonomía Fiscal
AutFiscal_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-2-1") %>% 
  dplyr::rename(AutonomiaFiscal_2018_base2017="2018",AutonomiaFiscal_2019_base2017="2019") 
AutFiscal_2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-2-1") %>% 
  dplyr::select(., c("Ciudad","2020")) %>% 
  dplyr::rename(AutonomiaFiscal_2020_base2018="2020")

# Capacidad de recaudo
CapacidadRecaudo_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-2-2") %>% 
  dplyr::select(., c("Ciudad","2018")) %>% 
  dplyr::rename(CapacidadRecaudo_2018_base2017="2018") 
CapacidadRecaudo_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-2-2") %>% 
  dplyr::rename(CapacidadRecaudo_2019_base2018="2019",CapacidadRecaudo_2020_base2018="2020") 

# Homicidios:
Homicidios_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-1") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(Homicidios_2018_base2018="2018")
Homicidios_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-1") %>% 
  dplyr::rename(Homicidios_2019_base2019="2019",Homicidios_2020_base2019="2020")

# Hurtos:
Hurtos_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-2") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(Hurtos_2018_base2018="2018")
Hurtos_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-2") %>% 
  dplyr::rename(Hurtos_2019_base2019="2019", Hurtos_2020_base2019="2020")

# Extorsión:
Extorsion_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-3") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(Extorsion_2018_base2018="2018")
Extorsion_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-3") %>% 
  dplyr::rename(Extorsion_2019_base2019="2019", Extorsion_2020_base2019="2020")

# Eficiencia de la Justicia:
EficienciaJusticia_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-4") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(EficJusticia_2018_base2017="2018")
EficienciaJusticia_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-4") %>% 
  dplyr::rename(EficJusticia_2019_base2019="2019", EficJusticia_2020_base2019="2020")

# Productividad de Jueces:
ProductividadJueces_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-5") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(ProducJueces_2018_base2017="2018")
ProductividadJueces_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-5") %>% 
  dplyr::rename(ProducJueces_2019_base2019="2019", ProducJueces_2020_base2019="2020")

# Facilidad para abrir empresa:
FacilidadEmpresa_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "NEG-1-1") %>% 
  dplyr::rename(FacilidadEmpresa_2018="2018", FacilidadEmpresa_2019="2019")
FacilidadEmpresa_2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "NEG-1-1") %>% 
  dplyr::select(., c("Ciudad","2020")) %>% 
  dplyr::rename(FacilidadEmpresa_2020="2020")

# Impuestos Empresa:
ImpuestosEmpresa_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "NEG-1-4") %>% 
  dplyr::rename(ImpuestosEmpresa_2018="2018", ImpuestosEmpresa_2019="2019")
ImpuestosEmpresa_2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "NEG-1-4") %>% 
  dplyr::select(., c("Ciudad","2020")) %>% 
  dplyr::rename(ImpuestosEmpresa_2020="2020")

# Desempleo:
Desempleo_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "LAB-1-2") %>% 
  dplyr::select(., c("Ciudad","2018")) %>% 
  dplyr::rename(Desempleo_2018="2018")
Desempleo_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "LAB-1-2") %>% 
  dplyr::rename(Desempleo_2019="2019", Desempleo_2020="2020")

# Complejidad Productiva:
ComplejidadProductiva_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "SOF-1-1") %>% 
  dplyr::rename(ComplejidadProductiva_2018="2018", ComplejidadProductiva_2019="2019")
ComplejidadProductiva_2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "SOF-1-1") %>% 
  dplyr::select(., c("Ciudad","2020")) %>% 
  dplyr::rename(ComplejidadProductiva_2020="2020")

# Diversificación de Canasta Exportadora:
DiversificacionExportadora_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "SOF-2-2") %>% 
  dplyr::select(., c("Ciudad","2018")) %>%
  dplyr::rename(DiversifCanasta_2018_base2018="2018")
DiversificacionExportadora_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "SOF-2-2") %>% 
  dplyr::rename(DiversifCanasta_2019_base2019="2019", DiversifCanasta_2020_base2019="2020")

# Creación del DF: Mediante concatenación de los indicadores previos:
Anexos_ICC <- join_all(list(AutFiscal_2018y2019,AutFiscal_2020, 
                            CapacidadRecaudo_2018, CapacidadRecaudo_2019y2020,
                            Homicidios_2018, Homicidios_2019y2020,
                            Hurtos_2018, Hurtos_2019y2020,
                            Extorsion_2018, Extorsion_2019y2020,
                            EficienciaJusticia_2018, EficienciaJusticia_2019y2020,
                            ProductividadJueces_2018, ProductividadJueces_2019y2020,
                            FacilidadEmpresa_2018y2019, FacilidadEmpresa_2020,
                            ImpuestosEmpresa_2018y2019, ImpuestosEmpresa_2020,
                            Desempleo_2018, Desempleo_2019y2020,
                            ComplejidadProductiva_2018y2019, ComplejidadProductiva_2020,
                            DiversificacionExportadora_2018, DiversificacionExportadora_2019y2020), 
                       by=c("Ciudad"),type = "full")
write_xlsx(Anexos_ICC, "Anexos_ICC_2018-2020.xlsx")

#### Data: GEIH ####
# Es importante tener presente la construcción del ID:
# ID Hogares: Directorio+Hogar
# ID Personas:Directorio+Orden+Hogar, se cambia a: Directorio+Hogar+Orden

# Importamos la data mensual para cada modulo a usar:
# Enero:
# Se cambia el nombre de las vbles pues venian con cambios de tipeo
GEIH_VyH_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201901 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Febrero:
GEIH_VyH_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201902 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Marzo:
GEIH_VyH_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201903 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Abril:
GEIH_VyH_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201904 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Mayo:
GEIH_VyH_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201905 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Junio:
GEIH_VyH_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio)
GEIH_CGP_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,ESC=Esc,fex_c_2011=Fex_c_2011)
GEIH_Ocu_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,RAMA2D=Rama2d,OCI=Oci,INGLABO=Inglabo,fex_c_2011=Fex_c_2011)
GEIH_OIng_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,P7500S3A1=P7500s3a1,P7500S2A1=P7500s2a1,P7500S1A1=P7500s1a1,P7510S2A1=P7510s2a1,P7510S6A1=P7510s6a1,P7510S5A1=P7510s5a1,P7510S3A1=P7510s3a1,P7510S1A1=P7510s1a1,P7510S7A1=P7510s7a1,fex_c_2011=Fex_c_2011)
GEIH_FT_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,FT=Ft)
GEIH_Des_201906 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden,DSI=Dsi,fex_c_2011=Fex_c_2011)

# Julio:
GEIH_VyH_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_Ocu_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_OIng_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_FT_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201907 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",") 

# Agosto:
GEIH_VyH_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201908 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")

# Septiembre:
GEIH_VyH_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201909 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")

# Octubre:
GEIH_VyH_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201910 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")

# Noviembre:
GEIH_VyH_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201911 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")

# Diciembre:
GEIH_VyH_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201912 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")


# Creamos una función para concatenar todas las variables a nivel mensual.
# Los detalles y paso a paso se encuentran en el constructo a continuación:

Data_Mes <- function(GEIH_VyH,GEIH_CGP,GEIH_Ocu,SMMV,GEIH_OIng,GEIH_FT,GEIH_Des,AnioMes,NombreArchivo){
# Data: GEIH. Modulo: Vivienda y Hogares [VyH]
  # Seleccionamos y transformamos las variables :
  # Personas en el hogar: P6008: númerica, se deja tal cual.
  # Posesión de vivienda: P5090: categorica, se transforma.
  # Posesión de computador: P5210S16: categorica, se transforma.
  # Posesión de internet: P5210S3: categorica, se transforma.
  GEIH_VyH_v2 <- GEIH_VyH %>% 
    dplyr::select(., c("AREA", "DIRECTORIO", "HOGAR", 
                       "P6008", "P5090", "P5210S16", "P5210S3",
                       "fex_c_2011")) %>% 
    dplyr::rename(.,PersonasHogar=P6008,PosesionVivienda=P5090, PosesionComputador=P5210S16,PosesionInternet=P5210S3) %>% 
    dplyr::mutate(PosesionVivienda=ifelse(PosesionVivienda==1,"Propia Pagada",PosesionVivienda),
                  PosesionVivienda=ifelse(PosesionVivienda==2,"Propia Pagando",PosesionVivienda),
                  PosesionVivienda=ifelse(PosesionVivienda==3,"Arriendo",PosesionVivienda),
                  PosesionVivienda=ifelse(PosesionVivienda==4,"Usufructo",PosesionVivienda),
                  PosesionVivienda=ifelse(PosesionVivienda==5,"Posesion sin titulo",PosesionVivienda),
                  PosesionVivienda=ifelse(PosesionVivienda==6,"Otra",PosesionVivienda),
                  PosesionComputador=ifelse(PosesionComputador==1,"Si",PosesionComputador),
                  PosesionComputador=ifelse(PosesionComputador==2,"No",PosesionComputador),
                  PosesionInternet=ifelse(PosesionInternet==1,"Si",PosesionInternet),
                  PosesionInternet=ifelse(PosesionInternet==2,"No",PosesionInternet))
  
# Data: GEIH. Modulo: Caracteristicas Generales Personas [CGP]
  # Seleccionamos y transformamos las variables:
  # Sexo: P6020: categorica, se transforma.
  # Edad: P6040: numérica, se deja así.
  # Nivel educativo: P6220: categorica, se transforma.
  # Años de escolaridad: ESC: numérica, se deja así.
  # Parentesco con Jefe de hogar: P6050: categorica, se transforma.
  # Estado Civil: P6070: categorica, se transforma.
  
  GEIH_CGP_v2 <- GEIH_CGP %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "P6020","P6040","P6220","ESC","P6050","P6070", 
                       "fex_c_2011")) %>% 
    dplyr::rename(., Sexo=P6020, Edad=P6040, NivelEducativo=P6220, AniosEscolaridad=ESC, ParentescoJefe=P6050, EstadoCivil=P6070) %>% 
    dplyr::mutate(Sexo=ifelse(Sexo==1,"Hombre",Sexo),
                  Sexo=ifelse(Sexo==2,"Mujer",Sexo),
                  NivelEducativo=ifelse(NivelEducativo==1,"Ninguno",NivelEducativo),
                  NivelEducativo=ifelse(NivelEducativo==2,"Bachiller",NivelEducativo),
                  NivelEducativo=ifelse(NivelEducativo==3,"Tecnologo",NivelEducativo),
                  NivelEducativo=ifelse(NivelEducativo==4,"Universitario",NivelEducativo),
                  NivelEducativo=ifelse(NivelEducativo==5,"Postgrado",NivelEducativo),
                  NivelEducativo=ifelse(NivelEducativo==9,"No sabe",NivelEducativo),
                  ParentescoJefe=ifelse(ParentescoJefe==1,"Jefe de hogar",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==2,"Conyuge",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==3,"Hijo(a)",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==4,"Nieto(a)",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==5,"Otro pariente",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==6,"Empleado domestico",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==7,"Pensionista",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==8,"Trabajador",ParentescoJefe),
                  ParentescoJefe=ifelse(ParentescoJefe==9,"Otro no pariente",ParentescoJefe),
                  EstadoCivil=ifelse(EstadoCivil==1,"Vive en pareja <2 años",EstadoCivil),
                  EstadoCivil=ifelse(EstadoCivil==2,"Vive en pareja >=2 años",EstadoCivil),
                  EstadoCivil=ifelse(EstadoCivil==3,"Casado(a)",EstadoCivil),
                  EstadoCivil=ifelse(EstadoCivil==4,"Separado(a)/Divorciado(a)",EstadoCivil),
                  EstadoCivil=ifelse(EstadoCivil==5,"Viudo(a)",EstadoCivil),
                  EstadoCivil=ifelse(EstadoCivil==6,"Soltero(a)",EstadoCivil))
  
# Data: GEIH. Modulo: Ocupados [Ocu]
  # Seleccionamos y transformamos las variables:
  # Posición Ocupacional: P6430: categorica, se transforma
  # Sector económico: RAMA2D: categorica, se transforma agrupando en secciones según CIUU
  # Ingreso laboral: INGLABO: Es numerica, se eliminan valores menores a 1000 y 9999.
  
  GEIH_Ocu_v2 <- GEIH_Ocu %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "P6430","RAMA2D","OCI","INGLABO",
                       "fex_c_2011")) %>% 
    dplyr::rename(., PosicionOcupacional=P6430, SectorEconomico=RAMA2D, IngresoLaboral_Mes=INGLABO) %>% 
    dplyr::mutate(PosicionOcupacional=ifelse(PosicionOcupacional==1,"Empleado_Empresa_Particular",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==2,"Empleado_Gobierno",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==3,"Empleado_Domestico",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==4,"CuentaPropia",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==5,"Empleador",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==6,"Trabajador_Familiar",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==7,"Trabajador_SinRemuneracion",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==8,"Jornalero",PosicionOcupacional),
                  PosicionOcupacional=ifelse(PosicionOcupacional==9,"Otro",PosicionOcupacional),
                  SectorEconomico=ifelse(SectorEconomico==0,"No Responde",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==1 | SectorEconomico==2,"Agrícultura Ganadería Caza Y Silvicultura",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==5,"Pesca",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==10 | SectorEconomico==11 | SectorEconomico==12 |SectorEconomico==13 |SectorEconomico==14,"Explotación de Minas y Canteras",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==15| SectorEconomico==16|SectorEconomico==17|SectorEconomico==18|SectorEconomico==19|SectorEconomico==20|SectorEconomico==21|SectorEconomico==22|SectorEconomico==23|SectorEconomico==24|SectorEconomico==25|SectorEconomico==26|SectorEconomico==27|SectorEconomico==28|SectorEconomico==29|SectorEconomico==30|SectorEconomico==31|SectorEconomico==32|SectorEconomico==33|SectorEconomico==34|SectorEconomico==35|SectorEconomico==36|SectorEconomico==37,"Industrias Manufactureras",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==40|SectorEconomico==41,"Suministro Electricidad Gas y Agua",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==45,"Construcción",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==50|SectorEconomico==51|SectorEconomico==52,"Comercio al por Mayor y por Menor",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==55,"Hoteles y Restaurantes",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==60|SectorEconomico==61|SectorEconomico==62|SectorEconomico==63|SectorEconomico==64,"Transporte Almacenamiento y Comunicaciones",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==65|SectorEconomico==66|SectorEconomico==67,"Intermediación Financiera",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==70|SectorEconomico==71|SectorEconomico==72|SectorEconomico==73|SectorEconomico==74,"Actividades Inmobiliarias Empresariales y Alquiler",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==75,"Administración Pública y Defensa",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==80,"Educación",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==85,"Servicios Sociales y de Salud",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==90|SectorEconomico==91|SectorEconomico==92|SectorEconomico==93,"Servicios Comunitarios Sociales y Personales",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==95,"Hogares Privados con servicio doméstico",SectorEconomico),
                  SectorEconomico=ifelse(SectorEconomico==99,"Organizaciones Extraterritoriales",SectorEconomico),
                  IngresoLaboral_Mes=ifelse(IngresoLaboral_Mes<1000 | IngresoLaboral_Mes==9999,NA,IngresoLaboral_Mes))
  
  # Creamos las categorías de Generadores de empleo y autoempleados:
  # Definimos el salario, incluyendo el salario de transporte
  # Crearemos una variable ficticia que luego modificaremos
  SMMV_2 <- SMMV*2.5
  GEIH_Ocu_v2$Generadores_Empleo <- c("G")
  GEIH_Ocu_v2 <- GEIH_Ocu_v2 %>% 
    dplyr::mutate(Generadores_Empleo=if_else(PosicionOcupacional=="Empleador","Generador de empleo",Generadores_Empleo),
                  Generadores_Empleo=if_else(PosicionOcupacional=="CuentaPropia" & IngresoLaboral_Mes>SMMV_2,"Generador de empleo",Generadores_Empleo),
                  Generadores_Empleo=if_else(PosicionOcupacional=="CuentaPropia" & IngresoLaboral_Mes<=SMMV_2,"Autoempleado",Generadores_Empleo)) 
  
# Data: GEIH. Modulo: Otros Ingresos [OIng]
  # Seleccionamos y renombramos las variables.
  # Eliminamos los valores < 1000 y los 9999 en todas las columnas.
  # Creamos y filtramos las variables que nos interesan.
  # Cambiamos los 0 por NA, los cuales fueron resultados de la suma creada.
  
  GEIH_OIng_v2 <- GEIH_OIng %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "P7500S3A1","P7500S2A1","P7500S1A1","P7510S2A1","P7510S6A1","P7510S5A1","P7510S3A1","P7510S1A1","P7510S7A1",
                       "fex_c_2011")) %>% 
    dplyr::rename(.,IngDivorcioMes=P7500S3A1,IngPensionMes=P7500S2A1,IngArriendoMes=P7500S1A1,IngRemExtrAnio=P7510S2A1,IngCesaAnio=P7510S6A1,IngFcieroAnio=P7510S5A1,TransfSubsidios_Anio=P7510S3A1,IngRemIntAnio=P7510S1A1,IngOtrosAnio=P7510S7A1) %>% 
    dplyr::mutate(IngDivorcioMes=ifelse(IngDivorcioMes<1000 | IngDivorcioMes==9999,NA,IngDivorcioMes),
                  IngPensionMes=ifelse(IngPensionMes<1000 |IngPensionMes==9999,NA,IngPensionMes),
                  IngArriendoMes=ifelse(IngArriendoMes<1000 |IngArriendoMes==9999,NA,IngArriendoMes),
                  IngRemExtrAnio=ifelse(IngRemExtrAnio<1000 |IngRemExtrAnio==9999,NA,IngRemExtrAnio),
                  IngCesaAnio=ifelse(IngCesaAnio<1000 |IngCesaAnio==9999,NA,IngCesaAnio),
                  IngFcieroAnio=ifelse(IngFcieroAnio<1000 |IngFcieroAnio==9999,NA,IngFcieroAnio),
                  TransfSubsidios_Anio=ifelse(TransfSubsidios_Anio<1000 |TransfSubsidios_Anio==9999,NA,TransfSubsidios_Anio),
                  IngRemIntAnio=ifelse(IngRemIntAnio<1000 |IngRemIntAnio==9999,NA,IngRemIntAnio),
                  IngOtrosAnio=ifelse(IngOtrosAnio<1000 |IngOtrosAnio==9999,NA,IngOtrosAnio))
  
  GEIH_OIng_v2$CapFinanciero_Mes <- rowSums(GEIH_OIng_v2[,c("IngDivorcioMes", "IngPensionMes", "IngArriendoMes")], na.rm=TRUE)
  GEIH_OIng_v2$CapFinanciero_Anio <- rowSums(GEIH_OIng_v2[,c("IngRemExtrAnio", "IngCesaAnio", "IngFcieroAnio", "IngRemIntAnio", "IngOtrosAnio")], na.rm=TRUE)
  
  GEIH_OIng_v3 <- GEIH_OIng_v2 %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "CapFinanciero_Mes","CapFinanciero_Anio","TransfSubsidios_Anio",
                       "fex_c_2011")) %>% 
    dplyr::mutate(CapFinanciero_Mes=ifelse(CapFinanciero_Mes==0,NA,CapFinanciero_Mes),
                  CapFinanciero_Anio=ifelse(CapFinanciero_Anio==0,NA,CapFinanciero_Anio))
  
# Data: GEIH. Modulo: Fuerza de Trabajo [FT]:
  # Seleccionamos nuestra única variable de interés: FT
  GEIH_FT_v2 <- GEIH_FT %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "FT",
                       "fex_c_2011"))
  
# Data: GEIH. Modulo: Descupados [Des]:
  # Seleccionamos nuestra única variable de interés: DSI
  GEIH_Des_v2 <- GEIH_Des %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR","ORDEN",
                       "DSI",
                       "fex_c_2011"))

# Calculos adicionales:
  # Ocupados por Hogar: 
  OcupadosEnHogar <- GEIH_Ocu_v2 %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      OcupadosPorHogar = sum(OCI,na.rm = TRUE))
  
  # Niños iguales o menores a 14 años en el hogar: 
  NiniosEnHogar <- GEIH_CGP_v2 %>% 
    dplyr::filter(Edad<=14) %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      NiniosMenores14 = n())

  # Ingresos en la unidad de gasto:
  # Son los ingresos laborales por hogar:
  # Eliminamos los 0 por NA
  IngresosEnHogar <- GEIH_Ocu_v2 %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      IngresosPorHogar = sum(IngresoLaboral_Mes,na.rm = TRUE)) %>% 
    dplyr::mutate(IngresosPorHogar=ifelse(IngresosPorHogar==0,NA,IngresosPorHogar))
  
  # Desempleados en el hogar:
  DesempleadosEnHogar <- GEIH_Des_v2 %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      DesempleadosPorHogar = sum(DSI,na.rm = TRUE))
  
  # Generadores de empleo en el hogar:
  GeneradoresEnHogar <- GEIH_Ocu_v2 %>% 
    dplyr::filter(Generadores_Empleo=="Generador de empleo") %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      GeneradoresPorHogar = n())
  
  # Autoempleados en el hogar:
  AutoempleadosEnHogar <- GEIH_Ocu_v2 %>% 
    dplyr::filter(Generadores_Empleo=="Autoempleado") %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      AutoempleadosPorHogar = n())
  
  # Educación de Jefes en el hogar: Crearemos dos variables.
  # 1. La sumatoria de los años de educación de los Jefes por hogar
  EducJefes_Suma <- GEIH_CGP_v2 %>% 
    dplyr::filter(ParentescoJefe=="Jefe de hogar"|ParentescoJefe=="Conyuge") %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      EducacionJefes_Suma = sum(AniosEscolaridad,na.rm = TRUE))
  
  # 2. El mayor número de años de educación entre los jefes del hogar:
  EducJefes_Max <- GEIH_CGP_v2 %>% 
    dplyr::filter(ParentescoJefe=="Jefe de hogar"|ParentescoJefe=="Conyuge") %>% 
    dplyr::group_by(DIRECTORIO, HOGAR) %>%
    dplyr::summarise(
      EducacionJefes_Max = max(AniosEscolaridad))
  
# Concatenando información:
  # Concatenamos por Hogares:
  Hogares <- join_all(list(GEIH_VyH_v2,EducJefes_Suma,EducJefes_Max,NiniosEnHogar,IngresosEnHogar,GeneradoresEnHogar,AutoempleadosEnHogar,OcupadosEnHogar,DesempleadosEnHogar), by=c("DIRECTORIO","HOGAR"),type = "full")
  
  # Concatenamos por individuo:
  Individuos <- join_all(list(GEIH_CGP_v2,GEIH_FT_v2,GEIH_Des_v2,GEIH_Ocu_v2,GEIH_OIng_v3), by=c("DIRECTORIO","HOGAR","ORDEN"),type = "full")
  
  # Concatenamos las dos datas previas, para tener un DF Mensual:
  # Corregimos algunas variables.
  Data <- join_all(list(Individuos,Hogares), by=c("DIRECTORIO","HOGAR"),type = "full") %>% 
    dplyr::mutate(Generadores_Empleo=ifelse(Generadores_Empleo=="G",NA,Generadores_Empleo),
                  NiniosMenores14=ifelse(is.na(NiniosMenores14),0,NiniosMenores14),
                  AutoempleadosPorHogar=ifelse(is.na(AutoempleadosPorHogar),0,AutoempleadosPorHogar),
                  GeneradoresPorHogar=ifelse(is.na(GeneradoresPorHogar),0,GeneradoresPorHogar),
                  DesempleadosPorHogar=ifelse(is.na(DesempleadosPorHogar),0,DesempleadosPorHogar),
                  OcupadosPorHogar=ifelse(is.na(OcupadosPorHogar),0,OcupadosPorHogar))
  Data$Anio_Mes <- c(AnioMes)
  
  # Exportamos la data mensual:
  write_xlsx(Data, NombreArchivo)
}

# Aplicación de la función
Data <- Data_Mes(GEIH_VyH=GEIH_VyH_201912,
                       GEIH_CGP=GEIH_CGP_201912,
                       GEIH_Ocu=GEIH_Ocu_201912,
                       SMMV=925148, # Este salario ya inluye el subsidio de transporte
                       GEIH_OIng=GEIH_OIng_201912,
                       GEIH_FT=GEIH_FT_201912,
                       GEIH_Des=GEIH_Des_201912,
                       AnioMes="2019_12",
                       NombreArchivo="DataMes_201912.xlsx")

# Aplicamos un join para tener la data mensual, y crear así un DF anual:
# Importamos la data  creada y creamos el ID
Enero <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201901.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Febrero <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201902.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Marzo <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201903.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Abril <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201904.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Mayo <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201905.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Junio <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201906.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Julio <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201907.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Agosto <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201908.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Septiembre <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201909.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Octubre <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201910.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Noviembre <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201911.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

Diciembre <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataMes_201912.xlsx") %>% 
  unite(.,ID,c(2:4),sep="",remove=TRUE,na.rm = FALSE)

# Aplicamos el Join y exportamos la data anual:
DataAnio_2019 <- join_all(list(Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre), by=c("ID"),type = "full")
# Verificamos que la longitud del jooion sea igual a la suma de los meses:
nrow(Enero)+nrow(Febrero)+nrow(Marzo)+nrow(Abril)+nrow(Mayo)+nrow(Junio)+nrow(Julio)+nrow(Agosto)+nrow(Septiembre)+nrow(Octubre)+nrow(Noviembre)+nrow(Diciembre)

# Analizamos y exportamos la data anual creada:
str(DataAnio_2019)
summary(DataAnio_2019)
write_xlsx(DataAnio_2019, "DataAnio_2019.xlsx")

# Aplicamos los pendientes a la data anual
# Cambiamos los códigos por Areas.
# Creamos las dummies a usar.
# Se calculan los pendientes: carga económica y experiencia potencial
DataAnio_2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/DataAnio_2019.xlsx")

DataAnio_2019_v2 <- DataAnio_2019 %>% 
  dplyr::rename(.,Desocupado=DSI, Ocupado=OCI) %>% 
  dplyr::mutate(AREA=ifelse(AREA==5,"Medellin",AREA),
                AREA=ifelse(AREA==8,"Barranquilla",AREA),
                AREA=ifelse(AREA==11,"Bogota DC",AREA),
                AREA=ifelse(AREA==13,"Cartagena",AREA),
                AREA=ifelse(AREA==17,"Manizales",AREA),
                AREA=ifelse(AREA==23,"Monteria",AREA),
                AREA=ifelse(AREA==50,"Villavicencio",AREA),
                AREA=ifelse(AREA==52,"Pasto",AREA),
                AREA=ifelse(AREA==54,"Cucuta",AREA),
                AREA=ifelse(AREA==66,"Pereira",AREA),
                AREA=ifelse(AREA==68,"Bucaramanga",AREA),
                AREA=ifelse(AREA==73,"Ibague",AREA),
                AREA=ifelse(AREA==76,"Cali",AREA),
                Desocupado=ifelse(is.na(Desocupado)==TRUE,0,Desocupado),
                Ocupado=ifelse(is.na(Ocupado)==TRUE,0,Ocupado),
                Sexo=ifelse(Sexo=="Hombre",1,0),
                PosesionComputador=ifelse(PosesionComputador=="Si",1,0),
                PosesionInternet=ifelse(PosesionInternet=="Si",1,0),
                NivelEducativo=ifelse(NivelEducativo=="Tecnologo" | NivelEducativo=="Universitario" | NivelEducativo=="Postgrado",1,0),
                EstadoCivil=ifelse(EstadoCivil=="Vive en pareja <2 años" | EstadoCivil=="Vive en pareja >=2 años" | EstadoCivil=="Casado(a)",1,0),
                PosesionVivienda=ifelse(PosesionVivienda=="Propia Pagada" | PosesionVivienda=="Propia Pagando",1,0),
                Autoempleado=ifelse(Generadores_Empleo=="Autoempleado",1,0),
                Autoempleado=ifelse(is.na(Autoempleado)==TRUE,0,Autoempleado),
                GeneradorEmpleo=ifelse(Generadores_Empleo=="Generador de empleo",1,0),
                GeneradorEmpleo=ifelse(is.na(GeneradorEmpleo)==TRUE,0,GeneradorEmpleo),
                PerentescoJefe_Jefe=ifelse(ParentescoJefe=="Jefe de hogar",1,0),
                PerentescoJefe_Conyuge=ifelse(ParentescoJefe=="Conyuge",1,0),
                PerentescoJefe_Hijo=ifelse(ParentescoJefe=="Hijo(a)",1,0),
                PerentescoJefe_Nieto=ifelse(ParentescoJefe=="Nieto(a)",1,0),
                PerentescoJefe_OtroPariente=ifelse(ParentescoJefe=="Otro pariente",1,0),
                PerentescoJefe_EmpleadoDomestico=ifelse(ParentescoJefe=="Empleado domestico",1,0),
                PerentescoJefe_Pensionista=ifelse(ParentescoJefe=="Pensionista",1,0),
                PerentescoJefe_Trabajador=ifelse(ParentescoJefe=="Trabajador",1,0),
                PerentescoJefe_OtroNoPariente=ifelse(ParentescoJefe=="Otro no pariente",1,0),
                Sector_NR=ifelse(SectorEconomico=="No Responde",1,0),
                Sector_Agricultura=ifelse(SectorEconomico=="Agrícultura Ganadería Caza Y Silvicultura",1,0),
                Sector_Pesca=ifelse(SectorEconomico=="Pesca",1,0),
                Sector_Minas=ifelse(SectorEconomico=="Explotación de Minas y Canteras",1,0),
                Sector_Industria=ifelse(SectorEconomico=="Industrias Manufactureras",1,0),
                Sector_Electricidad=ifelse(SectorEconomico=="Suministro Electricidad Gas y Agua",1,0),
                Sector_Comercio=ifelse(SectorEconomico=="Comercio al por Mayor y por Menor",1,0),
                Sector_Educacion=ifelse(SectorEconomico=="Educación",1,0),
                Sector_Transporte=ifelse(SectorEconomico=="Transporte Almacenamiento y Comunicaciones",1,0),
                Sector_Financiero=ifelse(SectorEconomico=="Intermediación Financiera",1,0),
                Sector_Construccion=ifelse(SectorEconomico=="Construcción",1,0),
                Sector_Administracion=ifelse(SectorEconomico=="Administración Pública y Defensa",1,0),
                Sector_Extraterritorial=ifelse(SectorEconomico=="Organizaciones Extraterritoriales",1,0),
                Sector_Inmobiliario=ifelse(SectorEconomico=="Actividades Inmobiliarias Empresariales y Alquiler",1,0),
                Sector_Salud=ifelse(SectorEconomico=="Servicios Sociales y de Salud",1,0),
                Sector_Hotel=ifelse(SectorEconomico=="Hoteles y Restaurantes",1,0),
                Sector_Comunitario=ifelse(SectorEconomico=="Servicios Comunitarios Sociales y Personales",1,0),
                Sector_Domestico=ifelse(SectorEconomico=="Hogares Privados con servicio doméstico",1,0),
                CargaEconomica=round(PersonasHogar/OcupadosPorHogar,2),
                CargaEconomica=ifelse(is.infinite(CargaEconomica)==T,NA,CargaEconomica),
                ExperienciaPotencial=round(Edad-AniosEscolaridad-6,2)) %>% 
  dplyr::select(.,-c("ParentescoJefe","FT","PosicionOcupacional","SectorEconomico","Generadores_Empleo"))
colnames(DataAnio_2019_v2)
str(DataAnio_2019_v2)

#### Concatenando Data de diferentes fuentes ####
# Importamos Data CCI
DataCCI <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/Anexos_ICC_2018-2020.xlsx")

# Modificamos data CCI: 
# Seleccionamos las columnas de interés
# Cambiamos los códigos de areas para que coincidan con la GEIH
DataCCI_2019 <- DataCCI %>% 
  dplyr::select(.,c("Ciudad","AutonomiaFiscal_2020_base2018","CapacidadRecaudo_2020_base2018","Homicidios_2020_base2019",
                    "Hurtos_2020_base2019","Extorsion_2020_base2019","EficJusticia_2020_base2019","ProducJueces_2020_base2019",
                    "FacilidadEmpresa_2020","ImpuestosEmpresa_2020","Desempleo_2020","ComplejidadProductiva_2020","DiversifCanasta_2020_base2019")) %>% 
  dplyr::rename(.,AREA=Ciudad) %>% 
  dplyr::mutate(AREA=ifelse(AREA=="Medellín AM","Medellin",AREA),
                AREA=ifelse(AREA=="Barranquilla AM","Barranquilla",AREA),
                AREA=ifelse(AREA=="Bogotá D.C.","Bogota DC",AREA),
                AREA=ifelse(AREA=="Cartagena","Cartagena",AREA),
                AREA=ifelse(AREA=="Manizales AM","Manizales",AREA),
                AREA=ifelse(AREA=="Montería","Monteria",AREA),
                AREA=ifelse(AREA=="Villavicencio","Villavicencio",AREA),
                AREA=ifelse(AREA=="Pasto","Pasto",AREA),
                AREA=ifelse(AREA=="Cúcuta AM","Cucuta",AREA),
                AREA=ifelse(AREA=="Pereira AM","Pereira",AREA),
                AREA=ifelse(AREA=="Bucaramanga AM","Bucaramanga",AREA),
                AREA=ifelse(AREA=="Ibagué","Ibague",AREA),
                AREA=ifelse(AREA=="Cali AM","Cali",AREA)) %>% 
  dplyr::filter(.,AREA=="Medellin"|AREA=="Barranquilla"|AREA=="Bogota DC"|AREA=="Cartagena"|
                  AREA=="Manizales"|AREA=="Monteria"|AREA=="Villavicencio"|AREA=="Pasto"|
                  AREA=="Cucuta"|AREA=="Pereira"|AREA=="Bucaramanga"|AREA=="Ibague"|
                  AREA=="Cali") 

# Importamos data del DANE
Dane_Desempleo <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/DANE_DesemPobPIB.xls", sheet = "DesempleoyPoblacion")
Dane_PIB_PC <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/DANE_DesemPobPIB.xls", sheet = "PIB PC")

# Modificamos data del Dane
# Seleccionamos las columnas de interés
# Cambiamos los códigos de areas para que coincidan con la GEIH
Dane_Desempleo_2019 <- Dane_Desempleo %>% 
  dplyr::select(.,c("Departamento","Desempleo_2019_Dane","Poblacion_2019_Dane")) %>% 
  dplyr::rename(.,AREA=Departamento) %>% 
  dplyr::mutate(AREA=ifelse(AREA=="Antioquia","Medellin",AREA),
                AREA=ifelse(AREA=="Atlántico","Barranquilla",AREA),
                AREA=ifelse(AREA=="Bogotá D.C.","Bogota DC",AREA),
                AREA=ifelse(AREA=="Bolívar","Cartagena",AREA),
                AREA=ifelse(AREA=="Caldas","Manizales",AREA),
                AREA=ifelse(AREA=="Córdoba","Monteria",AREA),
                AREA=ifelse(AREA=="Meta","Villavicencio",AREA),
                AREA=ifelse(AREA=="Nariño","Pasto",AREA),
                AREA=ifelse(AREA=="Norte de Santander","Cucuta",AREA),
                AREA=ifelse(AREA=="Risaralda","Pereira",AREA),
                AREA=ifelse(AREA=="Santander","Bucaramanga",AREA),
                AREA=ifelse(AREA=="Tolima","Ibague",AREA),
                AREA=ifelse(AREA=="Valle del Cauca","Cali",AREA)) %>% 
  dplyr::filter(.,AREA=="Medellin"|AREA=="Barranquilla"|AREA=="Bogota DC"|AREA=="Cartagena"|
                  AREA=="Manizales"|AREA=="Monteria"|AREA=="Villavicencio"|AREA=="Pasto"|
                  AREA=="Cucuta"|AREA=="Pereira"|AREA=="Bucaramanga"|AREA=="Ibague"|
                  AREA=="Cali")

Dane_PIB_PC_2019 <- Dane_PIB_PC %>% 
  dplyr::select(.,c("Departamento","PIB_PC2019_Dane")) %>% 
  dplyr::rename(.,AREA=Departamento) %>% 
  dplyr::mutate(AREA=ifelse(AREA=="Antioquia","Medellin",AREA),
                AREA=ifelse(AREA=="Atlántico","Barranquilla",AREA),
                AREA=ifelse(AREA=="Bogotá D.C.","Bogota DC",AREA),
                AREA=ifelse(AREA=="Bolívar","Cartagena",AREA),
                AREA=ifelse(AREA=="Caldas","Manizales",AREA),
                AREA=ifelse(AREA=="Córdoba","Monteria",AREA),
                AREA=ifelse(AREA=="Meta","Villavicencio",AREA),
                AREA=ifelse(AREA=="Nariño","Pasto",AREA),
                AREA=ifelse(AREA=="Norte de Santander","Cucuta",AREA),
                AREA=ifelse(AREA=="Risaralda","Pereira",AREA),
                AREA=ifelse(AREA=="Santander","Bucaramanga",AREA),
                AREA=ifelse(AREA=="Tolima","Ibague",AREA),
                AREA=ifelse(AREA=="Valle del Cauca","Cali",AREA)) %>% 
  dplyr::filter(.,AREA=="Medellin"|AREA=="Barranquilla"|AREA=="Bogota DC"|AREA=="Cartagena"|
                  AREA=="Manizales"|AREA=="Monteria"|AREA=="Villavicencio"|AREA=="Pasto"|
                  AREA=="Cucuta"|AREA=="Pereira"|AREA=="Bucaramanga"|AREA=="Ibague"|
                  AREA=="Cali")
  
# Ahora podemos aplicar el Join de toda la data para 2018: DataAnio+DataCCI+DataDane:
DataAnio_2019_v3 <- join_all(list(DataAnio_2019_v2,DataCCI_2019,Dane_Desempleo_2019,Dane_PIB_PC_2019), by=c("AREA"),type = "full")

# Finalmente revisamos y exportamos la data
colnames(DataAnio_2019_v3)
str(DataAnio_2019_v3)
summary(DataAnio_2019_v3)
write_xlsx(DataAnio_2019_v3, "DataAnio_2019_v3.xlsx")

##### Concatenando data anual en un solo DF #####
DataAnio_2017 <- read_excel("/Volumes/Respaldo/Google Drive-Actualizado/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/DataAnio_2017_v3.xlsx")
DataAnio_2018 <- read_excel("/Volumes/Respaldo/Google Drive-Actualizado/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/DataAnio_2018_v3.xlsx")
DataAnio_2019 <- read_excel("/Volumes/Respaldo/Google Drive-Actualizado/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/DataAnio_2019_v3.xlsx")

# Debemos renombrar algunas variables en cada DF, así:
DataAnio_2017_v4 <- DataAnio_2017 %>% 
  dplyr::rename(AutonomiaFiscal="AutonomiaFiscal_2018_base2017",
                CapacidadRecaudo="CapacidadRecaudo_2018_base2017", 
                Homicidios="Homicidios_2018_base2018",
                Hurtos="Hurtos_2018_base2018",
                Extorsion="Extorsion_2018_base2018",
                EficJusticia="EficJusticia_2018_base2017",      
                ProducJueces="ProducJueces_2018_base2017",
                FacilidadEmpresa= "FacilidadEmpresa_2018",          
                ImpuestosEmpresa="ImpuestosEmpresa_2018",
                Desempleo_ICC="Desempleo_2018",                  
                ComplejidadProductiva="ComplejidadProductiva_2018",
                DiversifCanasta= "DiversifCanasta_2018_base2018",   
                Desempleo_Dane="Desempleo_2017_Dane",
                Poblacion_Dane="Poblacion_2017_Dane",             
                PIB_PC="PIB_PC2017_Dane")

DataAnio_2018_v4 <- DataAnio_2018 %>% 
  dplyr::rename(AutonomiaFiscal="AutonomiaFiscal_2019_base2017",
                CapacidadRecaudo="CapacidadRecaudo_2019_base2018", 
                Homicidios="Homicidios_2019_base2019",
                Hurtos="Hurtos_2019_base2019",
                Extorsion="Extorsion_2019_base2019",
                EficJusticia="EficJusticia_2019_base2019",      
                ProducJueces="ProducJueces_2019_base2019",
                FacilidadEmpresa= "FacilidadEmpresa_2019",          
                ImpuestosEmpresa="ImpuestosEmpresa_2019",
                Desempleo_ICC="Desempleo_2019",                  
                ComplejidadProductiva="ComplejidadProductiva_2019",
                DiversifCanasta= "DiversifCanasta_2019_base2019",   
                Desempleo_Dane="Desempleo_2018_Dane",
                Poblacion_Dane="Poblacion_2018_Dane",             
                PIB_PC="PIB_PC2018_Dane")

DataAnio_2019_v4 <- DataAnio_2019 %>% 
  dplyr::rename(AutonomiaFiscal="AutonomiaFiscal_2020_base2018",
                CapacidadRecaudo="CapacidadRecaudo_2020_base2018", 
                Homicidios="Homicidios_2020_base2019",
                Hurtos="Hurtos_2020_base2019",
                Extorsion="Extorsion_2020_base2019",
                EficJusticia="EficJusticia_2020_base2019",      
                ProducJueces="ProducJueces_2020_base2019",
                FacilidadEmpresa= "FacilidadEmpresa_2020",          
                ImpuestosEmpresa="ImpuestosEmpresa_2020",
                Desempleo_ICC="Desempleo_2020",                  
                ComplejidadProductiva="ComplejidadProductiva_2020",
                DiversifCanasta= "DiversifCanasta_2020_base2019",   
                Desempleo_Dane="Desempleo_2019_Dane",
                Poblacion_Dane="Poblacion_2019_Dane",             
                PIB_PC="PIB_PC2019_Dane")

DataAnio <- rbind(DataAnio_2017_v4,DataAnio_2018_v4,DataAnio_2019_v4)

#### Data GEIH. Controles Etapa3 ####
# Es importante tener presente la construcción del ID:
# ID Hogares: Directorio+Hogar
# ID Personas: Directorio+Hogar+Orden
# También es de recordar la data que necesitamos: Modulo CGP
# Gasto en salud (P6120): Cuánto paga o cuánto le descuentan por la afiliación?
# Analfabetismo (P6160): Sabe leer y escribir.

# Funcion para seleccionar vbles de interés:
Funcion1 <- function(Data){
  Data <- Data %>% 
    dplyr::select(., c("AREA", "DIRECTORIO","HOGAR", "ORDEN",
                       "P6120", "P6160"))
  return(Data)
}

# Importamos la data mensual y seleccionamos las vbles de interés:
# Anio 2017
# Enero:
GEIH_CGP_201701 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201701 <- Funcion1(Data=GEIH_CGP_201701)
# Febrero:
GEIH_CGP_201702 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/2_Febrero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201702 <- Funcion1(Data=GEIH_CGP_201702)
# Marzo:
GEIH_CGP_201703 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/3_Marzo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201703 <- Funcion1(Data=GEIH_CGP_201703)
# Abril
GEIH_CGP_201704 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/4_Abril.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201704 <- Funcion1(Data=GEIH_CGP_201704)
# Mayo:
GEIH_CGP_201705 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/5_Mayo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201705 <- Funcion1(Data=GEIH_CGP_201705)
# Junio:
GEIH_CGP_201706 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/6_Junio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201706 <- Funcion1(Data=GEIH_CGP_201706)
# Julio:
GEIH_CGP_201707 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/7_Julio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201707 <- Funcion1(Data=GEIH_CGP_201707)
# Agosto:
GEIH_CGP_201708 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/8_Agosto.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201708 <- Funcion1(Data=GEIH_CGP_201708)
# Septiembre
GEIH_CGP_201709 <- read.delim("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/9_Septiembre.txt/Area - Caracteristicas generales (Personas).txt",header = TRUE, sep = "\t", dec = ".")
GEIH_CGP_201709 <- Funcion1(Data=GEIH_CGP_201709)
# Octubre:
GEIH_CGP_201710 <- read.delim("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/10_Octubre.txt/Area - Caracteristicas generales (Personas).txt",header = TRUE, sep = "\t", dec = ".")
GEIH_CGP_201710 <- Funcion1(Data=GEIH_CGP_201710)
# Noviembre:
GEIH_CGP_201711 <- read.delim("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/11_Noviembre.txt/Area - Caracteristicas generales (Personas).txt",header = TRUE, sep = "\t", dec = ".")
GEIH_CGP_201711 <- Funcion1(Data=GEIH_CGP_201711)
# Diciembre:
GEIH_CGP_201712 <- read.delim("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/12_Diciembre.txt/Area - Caracteristicas generales (Personas).txt",header = TRUE, sep = "\t", dec = ".")
GEIH_CGP_201712 <- Funcion1(Data=GEIH_CGP_201712)

# Anio 2018:
# Enero:
GEIH_CGP_201801 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201801 <- Funcion1(Data=GEIH_CGP_201801)
# Febrero:
GEIH_CGP_201802 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/2_Febrero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201802 <- Funcion1(Data=GEIH_CGP_201802)
# Marzo:
GEIH_CGP_201803 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/3_Marzo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201803 <- Funcion1(Data=GEIH_CGP_201803)
# Abril
GEIH_CGP_201804 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/4_Abril.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201804 <- Funcion1(Data=GEIH_CGP_201804)
# Mayo:
GEIH_CGP_201805 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/5_Mayo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201805 <- Funcion1(Data=GEIH_CGP_201805)
# Junio:
GEIH_CGP_201806 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/6_Junio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201806 <- Funcion1(Data=GEIH_CGP_201806)
# Julio:
GEIH_CGP_201807 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/7_Julio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201807 <- Funcion1(Data=GEIH_CGP_201807)
# Agosto:
GEIH_CGP_201808 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/8_Agosto.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201808 <- Funcion1(Data=GEIH_CGP_201808)
# Septiembre
GEIH_CGP_201809 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/9_Septiembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201809 <- Funcion1(Data=GEIH_CGP_201809)
# Octubre:
GEIH_CGP_201810 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/10_Octubre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201810 <- Funcion1(Data=GEIH_CGP_201810)
# Noviembre:
GEIH_CGP_201811 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/11_Noviembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201811 <- Funcion1(Data=GEIH_CGP_201811)
# Diciembre:
GEIH_CGP_201812 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2018/12_Diciembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201812 <- Funcion1(Data=GEIH_CGP_201812)

# Anio 2019:
# Fuuncion para cambiar minúsculas por mayúsculas
Funcion2 <- function(Data){
  Data <- Data %>% 
    dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden)
  return(Data)
}
# Enero:
GEIH_CGP_201901 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201901 <- GEIH_CGP_201901 %>% 
  dplyr::rename(.,AREA=Area,DIRECTORIO=Directorio,HOGAR=Hogar,ORDEN=Orden)
GEIH_CGP_201901 <- Funcion1(Data=GEIH_CGP_201901)
# Febrero:
GEIH_CGP_201902 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201902 <- Funcion2(Data=GEIH_CGP_201902)
GEIH_CGP_201902 <- Funcion1(Data=GEIH_CGP_201902)
# Marzo:
GEIH_CGP_201903 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201903 <- Funcion2(Data=GEIH_CGP_201903)
GEIH_CGP_201903 <- Funcion1(Data=GEIH_CGP_201903)
# Abril
GEIH_CGP_201904 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201904 <- Funcion2(Data=GEIH_CGP_201904)
GEIH_CGP_201904 <- Funcion1(Data=GEIH_CGP_201904)
# Mayo:
GEIH_CGP_201905 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201905 <- Funcion2(Data=GEIH_CGP_201905)
GEIH_CGP_201905 <- Funcion1(Data=GEIH_CGP_201905)
# Junio:
GEIH_CGP_201906 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201906 <- Funcion2(Data=GEIH_CGP_201906)
GEIH_CGP_201906 <- Funcion1(Data=GEIH_CGP_201906)
# Julio:
GEIH_CGP_201907 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201907 <- Funcion1(Data=GEIH_CGP_201907)
# Agosto:
GEIH_CGP_201908 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201908 <- Funcion1(Data=GEIH_CGP_201908)
# Septiembre
GEIH_CGP_201909 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201909 <- Funcion1(Data=GEIH_CGP_201909)
# Octubre:
GEIH_CGP_201910 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201910 <- Funcion1(Data=GEIH_CGP_201910)
# Noviembre:
GEIH_CGP_201911 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201911 <- Funcion1(Data=GEIH_CGP_201911)
# Diciembre:
GEIH_CGP_201912 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",")
GEIH_CGP_201912 <- Funcion1(Data=GEIH_CGP_201912)

# Concatenamos todo
DataControles <- join_all(list(GEIH_CGP_201701,GEIH_CGP_201702,GEIH_CGP_201703,GEIH_CGP_201704,GEIH_CGP_201705,GEIH_CGP_201706,
                               GEIH_CGP_201707,GEIH_CGP_201708,GEIH_CGP_201709,GEIH_CGP_201710,GEIH_CGP_201711,GEIH_CGP_201712,
                               GEIH_CGP_201801,GEIH_CGP_201802,GEIH_CGP_201803,GEIH_CGP_201804,GEIH_CGP_201805,GEIH_CGP_201806,
                               GEIH_CGP_201807,GEIH_CGP_201808,GEIH_CGP_201809,GEIH_CGP_201810,GEIH_CGP_201811,GEIH_CGP_201812,
                               GEIH_CGP_201901,GEIH_CGP_201902,GEIH_CGP_201903,GEIH_CGP_201904,GEIH_CGP_201905,GEIH_CGP_201906,
                               GEIH_CGP_201907,GEIH_CGP_201908,GEIH_CGP_201909,GEIH_CGP_201910,GEIH_CGP_201911,GEIH_CGP_201912), by=c("DIRECTORIO","HOGAR","ORDEN"),type = "full")

# Eliminamos AREA pues no necesitamos
# Creamos ID
# Eliminamos malas respuestas en P6120
# Organizamos rtas en P6160: Sabe leer y escribir 1 Si; 0 No.
# Renombramos las vaariables
DataControles_v2 <- DataControles %>% 
  dplyr::select(.,-c("AREA")) %>%
  unite(.,ID,c(1:3),sep="",remove=TRUE,na.rm = FALSE) %>% 
  dplyr::mutate(P6120=ifelse(P6120==98 | P6120==99,NA,P6120),
                P6160=ifelse(P6160==2,0,P6160)) %>% 
  dplyr::rename(GastoAfiliacion="P6120",Lectoescritura="P6160")

# Exportamos la data: en formato dta porque .xls no deja
write_dta(DataControles_v2, "DataControles.dta")
##### Pruebas#####






