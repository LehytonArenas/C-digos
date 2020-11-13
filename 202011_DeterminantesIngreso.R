# Directorio de trabajo
setwd("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)

#### Data: GEIH ####
# Es importante tener presente la construcción del ID:
# ID Hogares: Directorio+Hogar
# ID Personas:Directorio+Orden+Hogar 

# Data: GEIH. Modulo: Vivienda y Hogares [VyH]
# Seleccionamos y transformamos las variables :
# Personas en el hogar: P6008: númerica, se deja tal cual.
# Posesión de vivienda: P5090: categorica, se transforma.
# Posesión de computador: P5210S16: categorica, se transforma.
# Posesión de internet: P5210S3: categorica, se transforma.

GEIH_VyH_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",")
GEIH_VyH_201701_v2 <- GEIH_VyH_201701 %>% 
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

summary (GEIH_VyH_201701_v2)

# Data: GEIH. Modulo: Caracteristicas Generales Personas [CGP]
# Seleccionamos y transformamos las variables:
# Sexo: P6020: categorica, se transforma.
# Edad: P6040: numérica, se deja así.
# Nivel educativo: P6220: categorica, se transforma.
# Años de escolaridad: ESC: numérica, se deja así.
# Parentesco con Jefe de hogar: P6050: categorica, se transforma.
# Estado Civil: P6070: categorica, se transforma.

GEIH_CGP_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") 
GEIH_CGP_201701_v2 <- GEIH_CGP_201701 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO", "ORDEN","HOGAR",
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
                NivelEducativo=ifelse(NivelEducativo==6,"No sabe",NivelEducativo),
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

summary(GEIH_CGP_201701_v2)

# Data: GEIH. Modulo: Ocupados [Ocu]
# Seleccionamos y transformamos las variables:
# Posición Ocupacional: P6430: categorica, se transforma
# Sector económico: RAMA2D: categorica, se transforma agrupando en secciones según CIUU
# Ingreso laboral: INGLABO: Es numerica, se eliminan valores menores a 1000 y 9999.

GEIH_Ocu_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Ocu_201701_v2 <- GEIH_Ocu_201701 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO", "ORDEN","HOGAR",
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
                IngresoLaboral_Mes_Mes=ifelse(IngresoLaboral_Mes<1000 |IngresoLaboral_Mes==9999,NA,IngresoLaboral_Mes))

summary(GEIH_Ocu_201701_v2)

# Data: GEIH. Modulo: Otros Ingresos [OIng]
# Seleccionamos y renombramos las variables.
# Eliminamos los valores < 1000 y los 9999 en todas las columnas.
# Creamos y filtramos las variables que nos interesan.
# Cambiamos los 0 por NA, los cuales fueron resultados de la suma creada.
 
GEIH_OIng_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Otros ingresos.csv",sep = ";", header = TRUE, dec = ",")
GEIH_OIng_201701_v2 <- GEIH_OIng_201701 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO", "ORDEN","HOGAR",
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
                
GEIH_OIng_201701_v2$CapFinanciero_Mes <- rowSums(GEIH_OIng_201701_v2[,c("IngDivorcioMes", "IngPensionMes", "IngArriendoMes")], na.rm=TRUE)
GEIH_OIng_201701_v2$CapFinanciero_Anio <- rowSums(GEIH_OIng_201701_v2[,c("IngRemExtrAnio", "IngCesaAnio", "IngFcieroAnio", "IngRemIntAnio", "IngOtrosAnio")], na.rm=TRUE)

GEIH_OIng_201701_v3 <- GEIH_OIng_201701_v2 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO","ORDEN","HOGAR",
                     "CapFinanciero_Mes","CapFinanciero_Anio","TransfSubsidios_Anio",
                     "fex_c_2011")) %>% 
  dplyr::mutate(CapFinanciero_Mes=ifelse(CapFinanciero_Mes==0,NA,CapFinanciero_Mes),
                CapFinanciero_Anio=ifelse(CapFinanciero_Anio==0,NA,CapFinanciero_Anio))

summary(GEIH_OIng_201701_v3)                    

# Data: GEIH. Modulo: Fuerza de Trabajo [FT]:
# Seleccionamos nuestra única variable de interés: FT
GEIH_FT_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Fuerza de trabajo.csv",sep = ";", header = TRUE, dec = ",")
GEIH_FT_201701_v2 <- GEIH_FT_201701 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO", "ORDEN","HOGAR",
                     "FT",
                     "fex_c_2011"))

# Data: GEIH. Modulo: Descupados [Des]:
# Seleccionamos nuestra única variable de interés: DSI
GEIH_Des_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Desocupados.csv",sep = ";", header = TRUE, dec = ",")
GEIH_Des_201701_v2 <- GEIH_Des_201701 %>% 
  dplyr::select(., c("AREA", "DIRECTORIO", "ORDEN","HOGAR",
                     "DSI",
                     "fex_c_2011"))

# Calculos adicionales:
# Ocupados por Hogar: Un summarize a partir de la data de Ocupados
OcupadosEnHogar <- GEIH_Ocu_201701_v2 %>% 
  dplyr::group_by(DIRECTORIO, HOGAR) %>%
  dplyr::summarise(
    OcupadosPorHogar = sum(OCI,na.rm = TRUE))
summary(OcupadosEnHogar)

# Niños iguales o menores a 14 años en el hogar: Un summarize a partir de la data de CGP:
NiniosEnHogar <- GEIH_CGP_201701_v2 %>% 
  filter(Edad<=14) %>% 
  dplyr::group_by(DIRECTORIO, HOGAR) %>%
  dplyr::summarise(
    NiniosMenores14 = n())
summary(NiniosEnHogar)

# Ingresos en la unidad de gasto:
# Son los ingresos laborales por hogar:
# Eliminamos los 0 por NA
IngresosEnHogar <- GEIH_Ocu_201701_v2 %>% 
  dplyr::group_by(DIRECTORIO, HOGAR) %>%
  dplyr::summarise(
    IngresosPorHogar = sum(IngresoLaboral_Mes,na.rm = TRUE)) %>% 
  dplyr::mutate(IngresosPorHogar=ifelse(IngresosPorHogar==0,NA,IngresosPorHogar))
summary(IngresosEnHogar)

# Desempleados en el hogar:
DesempleadosEnHogar <- GEIH_Des_201701_v2 %>% 
  dplyr::group_by(DIRECTORIO, HOGAR) %>%
  dplyr::summarise(
    DesempleadosPorHogar = sum(DSI,na.rm = TRUE))
summary(DesempleadosEnHogar)


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











##### Pruebas#####
summary(GEIH_Ocu_201701_v2$SectorEconomico)
colnames(GEIH_OIng_201701)

# Prueba de Importando data de Autonomía Fiscal en 2016
class(AutFiscal_2016)

# Creando DF solo para menores de 14 años:
PruebaMenores <- GEIH_CGP_201701_v2 %>% 
  filter(Edad<=14)










