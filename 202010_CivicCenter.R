# Directorio de trabajo
setwd("/Users/lehyton/Google Drive/Ecsim/Proyecto CivicCenter/Directorio_RStudio")

# Paquetes a usar
library(tidyverse)
library(plyr); library(dplyr)
library(writexl) 
library(readxl)

# Importamos la data
ECV2017 <- read_excel("/Users/lehyton/Google Drive/Ecsim/Proyecto CivicCenter/2017_ECVMedellín/ECV_2017.xlsx")

# Seleccionamos las variables de interés
# Renombramos las variables
# Filtramos por las comunas de interés
# Transformamos las variables de interés según su manual
ECV2017_v2 <- ECV2017 %>% 
  dplyr::select(.,c("formulario", "Hogar", "Orden", "P_6", "P_45", "P_48", "P_66", "P_85", "P_87", "P_90", "P_255", "P_256", "P_257", "FEP", "FEVH")) %>% 
  dplyr::rename(.,Comunas=P_6, EstudiosNivel=P_45, EstudiosArea=P_48,AfiliaciónSegSoc=P_66, EmpleoActividad=P_85, EmpleoSalario=P_87, ActividadGanancia=P_90, HogarNegocios=P_255, NegocioActividad=P_256,NegocioActividadCIUU=P_257) %>% 
  dplyr::filter(.,Comunas>=8 & Comunas<=13) %>% 
  dplyr::mutate(Comunas=ifelse(Comunas==8,"Comuna8 Villa Hermosa",Comunas),
                Comunas=ifelse(Comunas==9,"Comuna9 Buenos Aires",Comunas),
                Comunas=ifelse(Comunas==10,"Comuna10 La Candelaria",Comunas),
                Comunas=ifelse(Comunas==11,"Comuna11 Laureles Estadio",Comunas),
                Comunas=ifelse(Comunas==12,"Comuna12 La América",Comunas),
                Comunas=ifelse(Comunas==13,"Comuna13 San Javier",Comunas),
                EstudiosNivel=ifelse(EstudiosNivel==-98,"No Sabe",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==-99,"No Responde",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==0,"Ninguno",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==1,"Preescolar",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==2,"Primaria",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==3,"Secundaria",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==4,"Media Acádemica",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==5,"Media Técnica",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==6,"Tecnológico",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==7,"Universidad",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==8,"Especialización",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==9,"Maestría",EstudiosNivel),
         EstudiosNivel=ifelse(EstudiosNivel==10,"Doctorado",EstudiosNivel),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==-99,"No Responde",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==-98,"No Sabe",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==1,"Es contributivo cotizante. Tiene EPS.",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==2,"Beneficiario del régimen contributivo.",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==3,"Subsidiado, tiene EPS - Subsidiada",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==4,"Régimen especial: (FFAA, ECOPETROL y magisterio)",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==5,"Beneficiario del Régimen especial",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==6,"No está afiliado y está identificado en el SISBEN",AfiliaciónSegSoc),
         AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==7,"No está afiliado y no está identificado en el SISBENe",AfiliaciónSegSoc),
         EmpleoActividad=ifelse(EmpleoActividad==-88,NA,EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==1,"Agropecuaria, silvicultura y pesca",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==2,"Minería",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==3,"Electricidad, gas, agua y alcantarillado",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==4,"Industria",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==5,"Construcción",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==6,"Comercio, hotelería y restaurantes",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==7,"Transporte, almacenamiento y comunicaciones",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==8,"Establecimientos Financieros",EmpleoActividad),
         EmpleoActividad=ifelse(EmpleoActividad==9,"Servicios sociales, comunales y personales",EmpleoActividad),
         EmpleoSalario=ifelse(EmpleoSalario==-99,NA,EmpleoSalario),
         EmpleoSalario=ifelse(EmpleoSalario==-98,NA,EmpleoSalario),
         EmpleoSalario=ifelse(EmpleoSalario==-97,NA,EmpleoSalario),
         EmpleoSalario=ifelse(EmpleoSalario==-88,NA,EmpleoSalario),
         HogarNegocios=ifelse(HogarNegocios==1,"Si",HogarNegocios),
         HogarNegocios=ifelse(HogarNegocios==2,"No",HogarNegocios),
         NegocioActividad=ifelse(NegocioActividad==-88,NA,NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==1,"Agropecuaria, silvicultura y pesca",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==2,"Minería",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==3,"Electricidad, gas, agua y alcantarillado",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==4,"Industria",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==5,"Construcción",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==6,"Comercio, hotelería y restaurantes",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==7,"Transporte, almacenamiento y comunicaciones",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==8,"Establecimientos Financieros, inmuebles y otros",NegocioActividad),
         NegocioActividad=ifelse(NegocioActividad==9,"Servicios sociales, comunales y personales",NegocioActividad))

# Calculando descriptivos por comuna:

# Función para descriptivos, en variables categoricas
Funcion <- function(Data, variable, variable_str){
  paso1 <- Data %>% 
    dplyr::group_by(Comunas) %>% 
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  paso2 <- Data %>% 
    dplyr::group_by(Comunas,{{variable}}) %>% 
    dplyr::summarise(SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  paso3  <- join_all(list(paso1,paso2), by=c("Comunas"),type = "full") %>% 
    plyr::mutate(PorcentajeEnComuna=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Comunas",all_of(variable_str),"PorcentajeEnComuna"))
  
  return(paso3)
} # Sin filtro de NA
Funcion2 <- function(Data, variable, variable_str){
  paso1 <- Data %>% 
    dplyr::group_by(Comunas) %>%
    dplyr::filter(.,is.na({{variable}})==FALSE) %>%
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  paso2 <- Data %>%
    dplyr::filter(.,is.na({{variable}})==FALSE) %>%
    dplyr::group_by(Comunas,{{variable}}) %>% 
    dplyr::summarise(SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  paso3  <- join_all(list(paso1,paso2), by=c("Comunas"),type = "full") %>% 
    plyr::mutate(PorcentajeEnComuna=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Comunas",all_of(variable_str),"PorcentajeEnComuna"))
  
  return(paso3)
} # Con filtro de NA

# Educación en cada una de las comunas
NivelEducativo <- Funcion(Data=ECV2017_v2, variable=EstudiosNivel, variable_str="EstudiosNivel")
write_xlsx(NivelEducativo, "Comunas_NivelEducativo.xlsx")

# Rama (tanto en empleo como en negocios) en cada una de las comuna
SectorEmpleo2017 <- Funcion2(Data=ECV2017_v2, variable=EmpleoActividad, variable_str="EmpleoActividad")
write_xlsx(SectorEmpleo2017, "Comunas_SectorEmpleo.xlsx")

SectorNegocio2017 <- Funcion2(Data=ECV2017_v2, variable=NegocioActividad, variable_str="NegocioActividad")
write_xlsx(SectorNegocio2017, "Comunas_SectorNegocio.xlsx")

# Afiliación a seguridad social
Afiliación_Com <- Funcion2(Data=ECV2017_v2, variable=AfiliaciónSegSoc, variable_str="AfiliaciónSegSoc")
write_xlsx(Afiliación_Com, "Comunas_Afiliación.xlsx")

# Ingreso: Ingreso medio de empleados en cada comuna
Salario <- function(Data){
  Paso1 <- Data %>% 
    dplyr::filter(.,is.na(EmpleoSalario)==FALSE) %>% 
    dplyr::group_by(Comunas) %>% 
    plyr::mutate(WiXi=round((EmpleoSalario*FEP),0)) %>% 
    dplyr::summarise(
      SumWiXi=round(sum(WiXi,na.rm = TRUE),0))
  
  Paso2 <- Data %>% 
    dplyr::filter(.,is.na(EmpleoSalario)==FALSE) %>% 
    dplyr::group_by(Comunas) %>% 
    dplyr::summarise(
      SumWi=round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(Paso1,Paso2), by=c("Comunas"),type = "full") %>% 
    plyr::mutate(SalarioMedio=round(SumWiXi/SumWi,0)) %>% 
    dplyr::select(.,c("Comunas","SalarioMedio")) 
}
Salario2017 <- Salario(Data=ECV2017_v2)
write_xlsx(Salario2017, "Comunas_Salario.xlsx")

# Ahora hallamos las mismas variables, pero a nivel ciudad
# Seleccionamos las variables de interés
# Renombramos las variables
# Transformamos las variables de interés según su manual
ECV2017_v3 <- ECV2017 %>% 
  dplyr::select(.,c("Ciudad","formulario", "Hogar", "Orden", "P_6", "P_45", "P_48", "P_66", "P_85", "P_87", "P_90", "P_255", "P_256", "P_257", "FEP", "FEVH")) %>% 
  dplyr::rename(.,Comunas=P_6, EstudiosNivel=P_45, EstudiosArea=P_48, AfiliaciónSegSoc=P_66, EmpleoActividad=P_85, EmpleoSalario=P_87, ActividadGanancia=P_90, HogarNegocios=P_255, NegocioActividad=P_256,NegocioActividadCIUU=P_257) %>% 
  dplyr::mutate(Ciudad=ifelse(Ciudad==5001,"Medellín",Ciudad),
                EstudiosNivel=ifelse(EstudiosNivel==-98,"No Sabe",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==-99,"No Responde",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==0,"Ninguno",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==1,"Preescolar",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==2,"Primaria",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==3,"Secundaria",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==4,"Media Acádemica",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==5,"Media Técnica",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==6,"Tecnológico",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==7,"Universidad",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==8,"Especialización",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==9,"Maestría",EstudiosNivel),
                EstudiosNivel=ifelse(EstudiosNivel==10,"Doctorado",EstudiosNivel),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==-99,"No Responde",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==-98,"No Sabe",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==1,"Es contributivo cotizante. Tiene EPS.",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==2,"Beneficiario del régimen contributivo.",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==3,"Subsidiado, tiene EPS - Subsidiada",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==4,"Régimen especial: (FFAA, ECOPETROL y magisterio)",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==5,"Beneficiario del Régimen especial",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==6,"No está afiliado y está identificado en el SISBEN",AfiliaciónSegSoc),
                AfiliaciónSegSoc=ifelse(AfiliaciónSegSoc==7,"No está afiliado y no está identificado en el SISBENe",AfiliaciónSegSoc),
                EmpleoActividad=ifelse(EmpleoActividad==-88,NA,EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==1,"Agropecuaria, silvicultura y pesca",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==2,"Minería",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==3,"Electricidad, gas, agua y alcantarillado",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==4,"Industria",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==5,"Construcción",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==6,"Comercio, hotelería y restaurantes",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==7,"Transporte, almacenamiento y comunicaciones",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==8,"Establecimientos Financieros",EmpleoActividad),
                EmpleoActividad=ifelse(EmpleoActividad==9,"Servicios sociales, comunales y personales",EmpleoActividad),
                EmpleoSalario=ifelse(EmpleoSalario==-99,NA,EmpleoSalario),
                EmpleoSalario=ifelse(EmpleoSalario==-98,NA,EmpleoSalario),
                EmpleoSalario=ifelse(EmpleoSalario==-97,NA,EmpleoSalario),
                EmpleoSalario=ifelse(EmpleoSalario==-88,NA,EmpleoSalario),
                HogarNegocios=ifelse(HogarNegocios==1,"Si",HogarNegocios),
                HogarNegocios=ifelse(HogarNegocios==2,"No",HogarNegocios),
                NegocioActividad=ifelse(NegocioActividad==-88,NA,NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==1,"Agropecuaria, silvicultura y pesca",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==2,"Minería",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==3,"Electricidad, gas, agua y alcantarillado",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==4,"Industria",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==5,"Construcción",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==6,"Comercio, hotelería y restaurantes",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==7,"Transporte, almacenamiento y comunicaciones",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==8,"Establecimientos Financieros, inmuebles y otros",NegocioActividad),
                NegocioActividad=ifelse(NegocioActividad==9,"Servicios sociales, comunales y personales",NegocioActividad))

# Calculando descriptivos: Aquí crearemos una función en cada apartado.
# Educación a nivel ciudad.
EducacionMed <- function(Data){
  prueba <- Data %>% 
    dplyr::group_by(Ciudad) %>% 
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba2 <- Data %>% 
    dplyr::group_by(Ciudad,EstudiosNivel) %>% 
    dplyr::summarise(
      SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(prueba,prueba2), by=c("Ciudad"),type = "full") %>% 
    plyr::mutate(Porcentaje=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Ciudad","EstudiosNivel","Porcentaje"))
  
  return(prueba3)
}
NivelEducativoMed <- EducacionMed(Data=ECV2017_v3)
write_xlsx(NivelEducativoMed, "Medellín_NivelEducativo.xlsx")

# Rama (tanto en empleo como en negocios) en la ciudad
Sector_EmpleoMed <- function(Data){
  prueba <- Data %>% 
    dplyr::filter(.,is.na(EmpleoActividad)==FALSE) %>% 
    dplyr::group_by(Ciudad) %>% 
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba2 <- Data %>% 
    dplyr::filter(.,is.na(EmpleoActividad)==FALSE) %>% 
    dplyr::group_by(Ciudad,EmpleoActividad) %>% 
    dplyr::summarise(
      SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(prueba,prueba2), by=c("Ciudad"),type = "full") %>% 
    plyr::mutate(Porcentaje=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Ciudad","EmpleoActividad","Porcentaje")) 
  
  return (prueba3)
}
SectorEmpleo2017Med <- Sector_EmpleoMed(Data=ECV2017_v3)
write_xlsx(SectorEmpleo2017Med, "Medellín_SectorEmpleo.xlsx")

Sector_NegocioMed <- function(Data){
  prueba <- Data %>% 
    dplyr::filter(.,is.na(NegocioActividad)==FALSE) %>% 
    dplyr::group_by(Ciudad) %>% 
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba2 <- Data %>% 
    dplyr::filter(.,is.na(NegocioActividad)==FALSE) %>% 
    dplyr::group_by(Ciudad,NegocioActividad) %>% 
    dplyr::summarise(
      SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(prueba,prueba2), by=c("Ciudad"),type = "full") %>% 
    plyr::mutate(Porcentaje=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Ciudad","NegocioActividad","Porcentaje")) 
  
  return (prueba3)
}
SectorNegocio2017Med <- Sector_NegocioMed(Data=ECV2017_v3)
write_xlsx(SectorNegocio2017Med, "Medellín_SectorNegocio.xlsx")

# Afiliación a seguridad social
AfiliaciónMed <- function(Data){
  prueba <- Data %>% 
    dplyr::filter(.,is.na(AfiliaciónSegSoc)==FALSE) %>% 
    dplyr::group_by(Ciudad) %>% 
    dplyr::summarise(
      SumWi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba2 <- Data %>% 
    dplyr::filter(.,is.na(AfiliaciónSegSoc)==FALSE) %>% 
    dplyr::group_by(Ciudad,AfiliaciónSegSoc) %>% 
    dplyr::summarise(
      SumWiXi =round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(prueba,prueba2), by=c("Ciudad"),type = "full") %>% 
    plyr::mutate(Porcentaje=(round(SumWiXi/SumWi,3))*100) %>% 
    dplyr::select(.,c("Ciudad","AfiliaciónSegSoc","Porcentaje")) 
  
  return (prueba3)
}
Afiliación_Med <- AfiliaciónMed(Data=ECV2017_v3)
write_xlsx(Afiliación_Med, "Medellín_Afiliación.xlsx")

# Ingreso: Ingreso medio de empleados en la ciudad
SalarioMed <- function(Data){
  Paso1 <- Data %>% 
    dplyr::filter(.,is.na(EmpleoSalario)==FALSE) %>% 
    dplyr::group_by(Ciudad) %>% 
    plyr::mutate(WiXi=round((EmpleoSalario*FEP),0)) %>% 
    dplyr::summarise(
      SumWiXi=round(sum(WiXi,na.rm = TRUE),0))
  
  Paso2 <- Data %>% 
    dplyr::filter(.,is.na(EmpleoSalario)==FALSE) %>% 
    dplyr::group_by(Ciudad) %>% 
    dplyr::summarise(
      SumWi=round(sum(FEP,na.rm = TRUE),0))
  
  prueba3  <- join_all(list(Paso1,Paso2), by=c("Ciudad"),type = "full") %>% 
    plyr::mutate(SalarioMedio=round(SumWiXi/SumWi,0)) %>% 
    dplyr::select(.,c("Ciudad","SalarioMedio")) 
}
Salario2017Med <- SalarioMed(Data=ECV2017_v3)
write_xlsx(Salario2017Med, "Medellín_Salario.xlsx")

# PRUEBA para hacer gráficos
# Las pruebas están bien. falta agregarlas al documento según corresponda
data2=NivelEducativo %>% 
  dplyr::filter(.,Comunas=="Comuna10 La Candelaria"|Comunas=="Comuna11 Laureles Estadio")

ggplot(data=data2,aes(y=EstudiosNivel, x=PorcentajeEnComuna, fill=Comunas))+
  geom_bar(stat="identity", position="dodge")+
  ylab("Nivel de Estudios")+
  xlab("% En la Comuna")

data3=SectorEmpleo2017 %>% 
  dplyr::filter(.,Comunas=="Comuna10 La Candelaria"|Comunas=="Comuna11 Laureles Estadio")

ggplot(data=data3,aes(y=EmpleoActividad, x=PorcentajeEnComuna, fill=Comunas))+
  geom_bar(stat="identity", position="dodge")+
  ylab("Sector Empleo")+
  xlab("% En la Comuna")







  