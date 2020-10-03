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





##### Otros #####
# Ordenando datos
prueba <- c(2,4,6,3,1)
prueba_ordenada <- sort(prueba)
