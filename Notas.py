#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Sep 26 20:02:44 2020

@author: lehyton
"""
# Demo file for Spyder Tutorial
# Hans Fangohr, University of Southampton, UK

def hello():
    """Print "Hello World" and return None."""
    print("Hello World")

# Main program starts here
hello()

# Definimos el valor de variables variable:
x=4
print(x)
y=10
print(y)
a,b=2,4
print(a)
print(b)
# variable int y float
a1=2
type(a1)
a2=1.52
type(a2)
# convirtiendo un float en int
int(a2)
# convirtiendo un int en float
float(a1)
# una variable con tipo boleano
a3=True
type(a3)
# Strings
"Argentina"
x=100
print(x+"dolares") #Sale error porque combinamos int con str
print(str(x) + " dolares")

# Condicional if
if 4==20/5:
    print("Acertaste")
# Otro ejemplo
if 4==20/5:
    print("Acertaste")
if 4!= 2*3:
    print("Acertaste2")

# Condicional if else:
    # veamos este caso:
x = 4
if x > 6:
    print("Caso 1")
if x <= 6:
    print("Caso 2")
    # Esto lo podemos solucionar con if else:
if x > 6: # Sentencia "Si"
    print("Caso 1")
else: # Sentencia "sino"
    print("Caso 2")

# Con elif introducimos varios "if" a la sentencia
def comparar_cuatro(y):
    if y > 4: # Sentencia "Si"
        return "Mayor"
    elif y < 0:# Otra sentencia "Si"
        return "Negativo"
    elif y < 4:# Otra sentencia "Si"
        return "Menor"
    else: # si las anteriores son falsas "entonces"
        return "Igual"
print(comparar_cuatro(-1))

# Funciones
def simple ():# definimos la función, en este caso sin párametros
    print ("Mi primera función")
simple() # Llamamos la función
def suma_diez(a): # función con un párametro 
    return(a+10) 
suma_diez(10)

# Ahora una función con más parametros:
def suma_diez(a): # función con un párametro 
    resultado = a+10 
    return(resultado)
suma_diez(4)

# Función dentro de una función:
# Supongamos que tu salario es una función de las horas trabajadas
# Te pagan a 20 USD cada hora trabajada 
def salario (n_trabajadas): # Función 1
    return (n_trabajadas * 20)
# Además te pagan un bono de 50 USD:
def con_bono (n_trabajadas): # Función 2
    return (salario(n_trabajadas)+50)
salario (8), con_bono(8) # Llamo ambas funciones identificando ambos salarios

# Combinemos funciones y condicionales:
# Problema: Si Carlos ha ahorrado al menos $100 al fin de semana. Entonces se le darán 10 adicionales
# En caso contrario ella no recibiría dinero adicional:
def sumar_10 (d):
    if d >= 100:
        d = d + 10
        return (d)
    else:
        return ("Carlos no has ahorrado lo suficiente")
sumar_10(40)

# Listas: Conjunto de elementos. Una lista se crea con []:
paises = ["colombia", "venezuela"]
paises[1]
paises[0] = "méxico"
paises
del paises[0]
paises
paises.append("Ecuador") # Un método se usa con el punto (.)
paises
len(paises) # Una función se usa con paréntesis ()

# Tuplas: conjunto de elementos inmutables, se crea con ()
x = (20, 21, 22) # creación de una tupla
x[0] # llamemos el primer elemento de la tupla
y = (30, 31, 32) # creamos otra tupla
lista = (x,y) # Creamos una lista compuesta por las dos tuplas anteriores
# Las funciones pueden dar como resultado tuplas:
def cuadrado (z):
    A = z ** 2
    P = 4 * z
    print ("Area y Perimetro")
    return A,P
cuadrado (2)

# Diccionarios: Son otra forma de almacenar datos, donde cada variable tiene un valor.
# Los diccionarios se crean con {variable:valor,...}
dict = {"x1" : "perro", "x2" : "gato", "x3" : "conejo"}
dict["x1"] # llamamos el elemento por su nombre
dict["x4"] = "pajaro" # podemos agregar un nuevo elemento al diccionario
dict["x3"] = "mono" # podemos cambiar elementos del diccionario
# PLT podemos crear un diccionario vacío y empezar a llenarlo
equipo = {}
equipo["equipo 1"] = "Carlos" 
equipo["equipo 2"] = "Luis"
equipo["equipo 3"] = "Miguel"
equipo["equipo 4"] = "Carolina"

# Iteraciones FOR: Sirve para aplicar un código de manera repetitiva, en cada elemento de un conjunto
par = [0,2,4,6,8,10]
for n in par:
    print(n)

rango = (1,2,3)
suma = 0
for n in rango:
    suma = suma + n
    print("n=", n, "suma=", suma)

# Iteraciones WHILE: Son iteraciones realizadas bajo ciertas condiciones
x = 0
while x <= 20:
    print(x)
    x = x + 2 # Condición para detener la iteración, mediante un incremento de 2
# Un incremento también puede escribirse de otra forma:
x = 0
while x <= 20:
    print(x)
    x += 2

# Función RANGE: Sirve para crear listas aleatorias de números: range(inicio, final, pasos)
list(range(0, 10, 1))
list(range(10)) # los parametros inicio y pasos, son opcionales, tiene por defecto los valores de 0 y 1 respectivamente

# Iteraciones y Range:
for n in range(10):
    print(2**n)

list(range (20))
for n in range(20):
    if n % 2 == 0:
        print (n)
    else:
        print("impar")

list(range(2))
list(range(5))
for i in range(2):
    for j in range(5):
        print("i: %d j: %d" %(i,j))


# Condicionales, Funciones y Bucles:
def contar(numeros):
    total = 0
    for x in numeros:
        if x < 20:
            total += 1
    return total
lista_1 = [2,4,6,10,14,18,20,28,30,42]
contar(lista_1)

# Iterar sobre diccionarios:
precios ={"chocolate":5, "helado":4, "caramelo":1}
cantidad = {"chocolate":10, "helado":8, "caramelo":0}
# Cuánto se gasto carlos en el supermercado?
dinero_gastado = 0
for p in precios:
    dinero_gastado = dinero_gastado + (precios[p]*cantidad[p])


# Taller S02: Numpy

# Punto 1. Defina un array de 6x6x6.
import numpy as np
X = np.random.randint(10, size=(6,6,6))
print (X)

# Punto 3. Demuestre que una matiz A (6x6) por por una identidad es igual a A
# Definamos la matriz A y la Identidad
A = np.random.randint(10, size = (6,6))
I = np.eye(6)
print (A)
print (I)
C = A.dot(I)
print(C)
I == C


# Taller S02: Pandas:
import pandas as pd
    
# Punto 1. 
s = pd.Series(['100', '200', 'python', '300.12', '400'])
print(s)

# Punto 2.1
exam_data = {'name': ['Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'], 'score': [12.5, 9, 16.5, 10, 9, 20, 14.5, 12, 8, 19], 'attempts': [1, 3, 2, 3, 2, 3, 1, 1, 2, 1], 'qualify': ['yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes'], 'labels': ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']}
df_t2=pd.DataFrame(exam_data) # Aquí creación del Df en pandas
print(df_t2)
df_t2.set_index('labels', inplace=True) # Aquí creamos el index como "labels"
print(df_t2.head(3)) # visualización de DF. Pto 1.

# Punto 2.2: Filtre solo las columnas "name" y "score"
df_t2[["name", "score"]]

# Punto 2.3: Seleccione las filas donde "attemps">2
print(df_t2[df_t2["attempts"]>2])

# Punto 3.1: Seleccione las filas con 15>="score"<=20
print(df_t2[(df_t2["score"] >= 15) & (df_t2["score"]<=20)])    

# Punto 3.2: Cambie el score en d a 11.5
df_t2.loc["d","score"] = 11.5
print(df_t2)

# Punto 3.3: Agregue una fila k con datos a su voluntad
df_t2.loc["k"] = ["Lehyton",18,1,"yes"]

# Punto 3.4: Elimine la fila k
df_t2.drop("k")
print(df_t2)

# Punto 3.5: Ordene el DF por "name" e orden ascendente
df_t2.sort_values(by=["name"], ascending=[True])

# Punto 3.6: Reemplace en "qualify" yes and no por True or False
df_t2['qualify'] = df_t2['qualify'].map({'yes': True, 'no': False})
print(df_t2)


# Taller S03: Taller 1

# Punto 1: 
def max_de_dos (a,b):
    if a == b:
        print ("Los números son iguales")
    else: 
        return (max(a, b))

max_de_dos(5,5)

# Punto 2:
numbers = [ 386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345, 399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217, 815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717, 958,743, 527 ]
listaFin=[]
for i in numbers:
  if (i%2==0):
      listaFin.append(i)
  if (i==237): 
      break
print(listaFin)

#  Punto 3.1: del 1-20 con un while
x = 1
while x <= 20:
    print ("x=",x)
    x=x+1

#  Punto 3.2: del 1-20 con un for
rango5 = list(range(21))
for n in rango5:
    print(n)

# Punto 4. Pregunta: Pregunta: No entiendo
from random import randint
b = randint(1, 21)

adivinaste = False 
conteo = 0
while not adivinaste:
    numero_usuario = int(input("Introduce el número buscado: "))   
    if a<b:
            print("Te quedaste corto, el número correcto era", b, "tus intentos son =", "conteo")
            adivinaste = False
            conteo=conteo+1          
    elif a>b:
            print("Te pasaste, el número correcto era", b, "tus intentos son =", "conteo")
            adivinaste = False 
            conteo=conteo+1 
    else:   
            print("Adivinaste!!")
            adivinaste = True 
             

# Pregunta 5.
def es_cadena_vacia(cadena):
    vacia = True
    if len(cadena) == 0:
        vacia = True
        print (vacia, "La cadena está vacía")
    else:
        vacia = False
        print (vacia, "La cadena no está vacia")
es_cadena_vacia("")
es_cadena_vacia("Quiero aprender Python")

# Pregunta 6.
def es_numero(entrada):
    respuesta = True
    if type(entrada) == int:
        respuesta = True
        return (respuesta)
    elif type(entrada) == float:
        respuesta = True
        return (respuesta)
    else: # "entrada no es un número"
        respuesta = False
        return (respuesta)
es_numero("b")

# Pregunta 7. Pregunta: No sé por qué no me da.
class Hotel():
    nombre = ''
    ubicacion = ''
    puntaje = ''
    precio = ''
    def print_information (self, nombre, ubicacion, puntaje, precio):  
        if type(nombre)!=str or len(nombre)==0: 
            print("Error: Recuerde escribir el nombre del Hotel") 
        elif type(ubicacion)!=str or len(ubicacion)==0: 
            print("Error: Recuerde escribir la ubicación del Hotel") 
        elif type(puntaje)!=int or type(puntaje)!=float: 
            print("Error: el puntaje debe ser un número") 
        elif type(precio)!=int or type(precio)!=float or precio==0: 
            print("Error: el precio debe ser un número distinto de cero") 
        else: 
            print("nombre:", self.nombre) 
            print("ubicacion:", self.ubicacion) 
            print("puntaje:", self.puntaje) 
            print("precio:", self.precio)

TresEstrellas = Hotel()
TresEstrellas.nombre= 'Alcatraz'
TresEstrellas.ubicacion= 'Colombia'
TresEstrellas.puntaje= 5
TresEstrellas.precio= 40000
TresEstrellas.print_information(TresEstrellas.nombre,TresEstrellas.ubicacion, TresEstrellas.puntaje, TresEstrellas.precio)

type(TresEstrellas.puntaje)



