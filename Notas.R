##### Condicional IF #####
statura = 75
if (statura<70){
  print ("es bajo")
} else {
  print ("es alto")
}

# Podemos agregar varios condicionantes:
if (statura<70){
  print ("es bajo")
} else if (statura >= 70 & statura < 76) {
  print ("es mediano")
} else {
  print ("es alto")
}

##### Función simple #####
# Función simple para una suma
# Creamos la función.
suma <- function(x,y) {
  x+y
}

# Aplicamos la función:
suma (x=2,y=3)

##### Función con condiciones "if else"#####
# La TMB se calcula siguiendo las siguientes ecuaciones:
# TMB Mujer = 655 + (9,6 * P) + (1,8 * A) – (4,7 * E)
# TMB Hombre = 66 + (13,7 * P) + (5 * A) – (6,8 * E)
# donde necesitamos información del Sexo, A=Altura, P=Peso y E=Edad de cada persona, nuestros argumentos
# Creamos la función
TMB <- function(Sexo,Altura,Peso,Edad){
  if(Sexo=="mujer"){ 655 + (9.6 * Peso) + (1.8 * Altura) - (4.7 * Edad)
  }
  else{ 66 + (13.7 * Peso) + (5 * Altura) - (6.8 * Edad) 
  }
  }

# Aplicamos la función:
TMB("hombre",170,57,32)

##### Función en donde los resultados sean un DF #####
# Ejemplo con el calculo de la hipotenusa de un triangulo
hipotenusa <- function(cateto1, cateto2){
  h<-sqrt(cateto1^2+cateto2^2)
  data.frame(variable=c("cateto","cateto","hipotenusa"),valor=c(cateto1,cateto2,h))
  }

# Aplicamos la función:
hipotenusa(2,4)
# Otra forma de aplicar la función
hipotenusa(2:4,4:6)


##### Función con el uso de "Return"#####
# Sirve para obtener el resultado de un paso específico de la función

f<-function(x,y){
  if(is.character(y)) 
    return("y debe ser numérico")
  x+y
  }

f(2,"hola")

##### Bucles For #####
# Las iteraciones permiten aplicar una funcionalidad en los elementos de uno o más  vectores
# Ejemplo 1:
for (i in c(1,3,5,9)) { # Defino el vector i (el "i" puede llamarse a voluntad)
  for (j in c("pepe","tino")) { # Defino el vector j
    print(c(i,j)) # Aplico la funcionalidad: imprimir (i,j), en cada par (i,j)
    
  }
  
}

# Ejemplo 2: Aplicar el For en vectores i e j:
for (i in c(1,3,5,9)) {
  for (j in c("pepe","tino")) {
  }
  print(c(i,j))
}

# Ejemplo 3: 
poblacion <- c(1000, 1500, 1600, 2000)
for (pob in poblacion){
  print(pob*2)
}
# Ejempplo 4: Basado en el ejemplo 3
for(i in 1: length(poblacion)){
  print(paste ("la población en la posición", i, "es", poblacion[i] ))
}

# Ejemplo 5:
municipios <- c("san Juan", "Ponce", "Caguas", "Cidra")
for(muni in municipios){
  print(toupper(muni))
}

# Ejemplo 6:
for(muni in municipios){
  if(nchar(muni)==5){
    break
  }
  print(toupper(muni))
}

for(muni in municipios){
  if(nchar(muni)==5){
    next
  }
  print(toupper(muni))
}

##### Iteraciones while #####
# Las iteraciones while permiten aplicar una iteración hasta que se cumple alguna condición
# De no especificarse bien se aplicará un bucle infinito
carros <- 1 # Elemento sobre el cual se aplica la iteración.
while (carros<10){ # Aplico el while poniendo la condición
 print(paste("el número de carros al momento es", carros)) # Describo la funcionalidad
  carros <- carros+1 # Esta condición es fundamental sirve para detener el loop
}

# Ahora creamos una iteración while con dos elementos y con break
carros <- 1 # elemento 1.
aviones <- 1 # elemento 2.
while (carros<10){ # Aplico el while poniendo la condición
  if(carros+aviones==8){ # Aquí puedo poner una condición para detener la iteración
    break
  }
  print(paste("el número de carros al momento es", carros)) # Describo la funcionalidad
  print(paste("el número de aviones al momento es", aviones)) # Describo la funcionalidad
  carros <- carros+1 # Esta condición es fundamental sirve para detener el loop
  aviones <- aviones+1
}





#### Ordenando datos ####
prueba <- c(2,4,6,3,1)
prueba_ordenada <- sort(prueba)

# Pruebas de gráficos
ggplot(data=dplyr::filter(ECV2019_Ant,Eje_v2=="Cauca" | Eje_v2=="Antioquia"),
       aes(y=Municipio, x=LP))+
  geom_bar(stat="identity", position="dodge", fill="dodgerblue2")+
  ylab("Cauca")+
  xlab("LP")+
  geom_text(aes(label = LP),
            size=4, vjust=0.5, hjust=0 ,col="black")+
  theme (axis.text.x = element_text(size=rel(1.5)),
         axis.text.y = element_text(size=rel(1.5)))

#### Ejemplo de Muestreo ####
Muestreo <- function(z,p,q,N,e){
  M=round(((z^2)*p*q*N)/(((e^2)*(N-1))+((z^2)*p*q)),0)
  print(M)
}
Prueba1=Muestreo(z=1.96,
                 p=0.5,
                 q=0.5,
                 N=5000,
                 e=0.03)

#### Regresiones ####
# Se tomará la data de Adultos_2017 del proyecto de Determinantes del ingreso:

# regresión simple entre ingresos mensuales vs educación
regresion <- lm(IngresoLaboral_Mes~AniosEscolaridad,data = Adultos)
summary(regresion)

# Regresión 2spls para:
Data_Prueba <- Adultos %>% 
  dplyr::select(c("IngresoLaboral_Mes","AniosEscolaridad",
                  "GeneradorEmpleo","AutonomiaFiscal")) %>% 
  drop_na(.)

tspls(IngresoLaboral_Mes~AniosEscolaridad,
      GeneradorEmpleo~AutonomiaFiscal,
      data = Data_Prueba)


#### ECSIM: Ocupados en Salud 2019 ####
# Data original: Importamos data GEIH 2019:
GEIH_Ocu_201901 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/1_Enero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201902 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/2_Febrero.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201903 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/3_Marzo.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201904 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/4_Abril.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201905 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/5_Mayo.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201906 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/6_Junio.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201907 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/7_Julio.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201908 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/8_Agosto.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201909 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/9_Septiembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201910 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/10_Octubre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201911 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/11_Noviembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 
GEIH_Ocu_201912 <- read.csv("/Volumes/Respaldo/Google Drive (lehyton.arenas@ucn.cl)-Actualizado/Lehyton Windows/Empleo/Data_Original/Data_GEIH_Original/GEIH 2019/12_Diciembre.csv/Area - Ocupados.csv",sep = ";", header = TRUE, dec = ",") 

# Creamos funciones de interés:
# Funciones para seleccionar vbles de interés
FuncionVblesMin <- function(Data){
  Data_v2 <- Data %>% 
    dplyr::select(.,c(Rama2d,P6430,Fex_c_2011))
  return(Data_v2)
}
FuncionVblesMay <- function(Data){
  Data_v2 <- Data %>% 
    dplyr::select(.,c(RAMA2D,P6430,fex_c_2011)) %>% 
    dplyr::rename(.,Rama2d=RAMA2D,Fex_c_2011=fex_c_2011)
  return(Data_v2)
}

# Seleccionamos variables de interés
GEIH_Ocu_201901_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201901)
GEIH_Ocu_201902_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201902)
GEIH_Ocu_201903_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201903)
GEIH_Ocu_201904_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201904)
GEIH_Ocu_201905_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201905)
GEIH_Ocu_201906_v2 <- FuncionVblesMin(Data=GEIH_Ocu_201906)
GEIH_Ocu_201907_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201907)
GEIH_Ocu_201908_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201908)
GEIH_Ocu_201909_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201909)
GEIH_Ocu_201910_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201910)
GEIH_Ocu_201911_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201911)
GEIH_Ocu_201912_v2 <- FuncionVblesMay(Data=GEIH_Ocu_201912)

# Concatenamos y así tenemos un DF anual 
GEIH2019Anio <- rbind(GEIH_Ocu_201901_v2,GEIH_Ocu_201902_v2,GEIH_Ocu_201903_v2,GEIH_Ocu_201904_v2,
                      GEIH_Ocu_201905_v2,GEIH_Ocu_201906_v2,GEIH_Ocu_201907_v2,GEIH_Ocu_201908_v2,
                      GEIH_Ocu_201909_v2,GEIH_Ocu_201910_v2,GEIH_Ocu_201911_v2,GEIH_Ocu_201912_v2)

# Eliminamos puntos medios:
rm(GEIH_Ocu_201901,GEIH_Ocu_201902,GEIH_Ocu_201903,GEIH_Ocu_201904,GEIH_Ocu_201905,GEIH_Ocu_201906,
   GEIH_Ocu_201907,GEIH_Ocu_201908,GEIH_Ocu_201909,GEIH_Ocu_201910,GEIH_Ocu_201911,GEIH_Ocu_201912,
   GEIH_Ocu_201901_v2,GEIH_Ocu_201902_v2,GEIH_Ocu_201903_v2,GEIH_Ocu_201904_v2,GEIH_Ocu_201905_v2,
   GEIH_Ocu_201906_v2,GEIH_Ocu_201907_v2,GEIH_Ocu_201908_v2,GEIH_Ocu_201909_v2,GEIH_Ocu_201910_v2,
   GEIH_Ocu_201911_v2,GEIH_Ocu_201912_v2,FuncionVblesMin,FuncionVblesMay)

# Renombramos y recodificamos P6430:
# dividimos Fexp en 12
GEIH2019Anio <- GEIH2019Anio %>% 
  dplyr::rename(., PosicionOcupacional=P6430) %>% 
  dplyr::mutate(PosicionOcupacional=ifelse(PosicionOcupacional==1,"Empleado_Empresa_Particular",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==2,"Empleado_Gobierno",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==3,"Empleado_Domestico",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==4,"CuentaPropia",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==5,"Empleador",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==6,"Trabajador_Familiar",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==7,"Trabajador_SinRemuneracion",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==8,"Jornalero",PosicionOcupacional),
                PosicionOcupacional=ifelse(PosicionOcupacional==9,"Otro",PosicionOcupacional)) %>% 
  mutate(FExp=Fex_c_2011/12) %>% 
  select(.,-c(Fex_c_2011))

# Encontramos estadísticas de interés
OcuSalud <- GEIH2019Anio %>% 
  dplyr::filter(.,Rama2d==85) %>% 
  dplyr::group_by(PosicionOcupacional) %>% 
  dplyr::summarise(OcupadosSalud = round(sum(FExp,na.rm = TRUE),0))

#### Pruebas ####