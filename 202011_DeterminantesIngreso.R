# Directorio de trabajo
setwd("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)

# Data: GEIH. Modulo: Vivienda y Hogares [VyH]
# Seleccionamos y transformamos las variables :
# Personas en el hogar: P6008: númerica, se deja tal cual.
# Posesión de vivienda: P5090: categorica, se transforma.
# Posesión de computador: P5210S16: categorica, se transforma.
# Posesión de internet: P5210S3: categorica, se transforma.

GEIH_VyH_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Vivienda y Hogares.csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::select(., c("AREA", "MES", "DIRECTORIO", "SECUENCIA_P","HOGAR", "P6008", "P5090", "P5210S16", "P5210S3","fex_c_2011")) %>% 
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

GEIH_CGP_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Caracteristicas generales (Personas).csv",sep = ";", header = TRUE, dec = ",") %>% 
  dplyr::select(., c("AREA", "MES", "DIRECTORIO", "SECUENCIA_P","ORDEN","HOGAR","P6020","P6040","P6220","ESC","P6050","P6070", "fex_c_2011")) %>% 
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

# Data: GEIH. Modulo: Ocupados [Ocu]
# Seleccionamos y transformamos las variables:
# Posición Ocupacional: P6430: categorica, se transforma
# Sector económico: RAMA2D: categorica, se transforma agrupando en secciones según CIUU
# Ingreso laboral: INGLABO: Es numerica, se deja así.

GEIH_Ocu_201701 <- read.csv("/Users/lehyton/Google Drive (lehyton.arenas@ucn.cl)/Lehyton Windows/Empleo/Data_GEIH_Original/GEIH 2017/1_Enero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",")%>% 
  dplyr::select(., c("AREA", "MES", "DIRECTORIO", "SECUENCIA_P","ORDEN","HOGAR","P6430","RAMA2D","OCI","INGLABO","fex_c_2011")) %>% 
  dplyr::rename(., PosicionOcupacional=P6430, SectorEconomico=RAMA2D, IngresoLaboral=INGLABO) %>% 
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
                SectorEconomico=ifelse(SectorEconomico==99,"Organizaciones Extraterritoriales",SectorEconomico))
  
# Data: GEIH. Modulo: Otros Ingresos [OIng]
# VOY AQUI A LA ESPERA DE RESPUESTAS EN EL MAIL


# Data: ICC 2018.
# Corregir porcentajes en: Autonomía fiscal, capacidad de recaudo, eficiencia de la justicia.
AutFiscal_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-2-1") 
CapacidadRecaudo_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-2-2")

Homicidios_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-3-1")
Hurtos_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-3-2")
Extorsión_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-3-3")
EficienciaJusticia_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-3-5")
ProductividadJueces_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INS-3-6")

FacilidadEmpresa_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "EFI-1-3")
ImpuestosEmpresa_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "EFI-1-4")
DensidadEmpresa_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "INN-3-2")

Desempleo_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "EFI-2-3")

ComplejidadProductiva_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "SOF-1-1")
DiversificacionExportadora_2018 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2018_ICC.xlsx", sheet = "SOF-2-2")

# Data: ICC 2019.
# Corregir porcentajes en: Autonomía fiscal, capacidad de recaudo, eficiencia de la justicia.
AutFiscal_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-2-1") 
CapacidadRecaudo_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-2-2")

Homicidios_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-1")
Hurtos_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-2")
Extorsión_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-3")
EficienciaJusticia_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-4")
ProductividadJueces_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "INS-4-5")

FacilidadEmpresa_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "NEG-1-1")
ImpuestosEmpresa_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "NEG-1-4")
DensidadEmpresa_2016 <- read_excel("", sheet = "")

Desempleo_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "LAB-1-2")

ComplejidadProductiva_2018y2019 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "SOF-1-1")
DiversificacionExportadora_2016 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2019_ICC.xlsx", sheet = "SOF-2-2")

# Data: ICC 2020.
# Corregir porcentajes en: eficiencia de la justicia.
AutFiscal_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-2-1") 
CapacidadRecaudo_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-2-2")

Homicidios_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-1")
Hurtos_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-2")
Extorsión_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-3")
EficienciaJusticia_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-4")
ProductividadJueces_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "INS-4-5")

FacilidadEmpresa_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "NEG-1-1")
ImpuestosEmpresa_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "NEG-1-4")
DensidadEmpresa_2016 <- read_excel("", sheet = "INN-3-2")

Desempleo_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "LAB-1-2")

ComplejidadProductiva_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "SOF-1-1")
DiversificacionExportadora_2019y2020 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto Agosto/3_Entregables Lehyton/Data/2020_ICC.xlsx", sheet = "SOF-2-2")

# Pruebas:  
summary(GEIH_Ocu_201701_v2$SectorEconomico)
colnames(GEIH_Ocu_201701)

# Prueba de Importando data de Autonomía Fiscal en 2016

class(AutFiscal_2016)











