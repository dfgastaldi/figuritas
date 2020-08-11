# Paenza; Pagina 12 - Contratapa 06/07/2014
# Datos:
#   Album con 640 figuritas.
# Cada figurita se imprime en cantidades iguales y se distribuye aleatoriamente.
# Cada paquete trae cinco figuritas.
# Cada paquete sale $20
# 1. Creamos las funciones

# a) Simular la generacion de un paquete con 5 figuritas, sabiendo que el album es de 640. Notemos que, como en la vida real, pueden haber figuritas repetidas en un paquete.
sample(     #Utilizamos la función sample para obtener números pseudo-aleatorios
  1:640,    #El primer argumento son los valores posibles. En este caso, desde 1 hasta las 640 figuritas posibles
  5,        #El segundo argumento es la cantidad de valores que extraerá sobre los posibles. En este caso, los 5 que trae el sobre
  T         #Este argumento permite que haya repetidas en un mismo sobre
  )

# b) Implementar una funcion genPaquete(figusTotal, figusPaquete) que dado el tamaño del album (figusTotal) y la cantidad de figuritas por paquete (figusPaquete) genere un paquete de figuritas al azar. Notemos que, como en la vida real, pueden haber figuritas repetidas en un paquete.

# Paso 1: Creamos la función
genPaquete <- function(figusTotal, figusPaquete) {
  sample(            #Utilizamos la función sample para obtener números pseudo-aleatorios
    1:figusTotal,    #El primer argumento son los valores posibles. En este caso, desde 1 hasta las 640 figuritas posibles
    figusPaquete,    #El segundo argumento es la cantidad de valores que extraerá sobre los posibles. En este caso, figusPaquete
    T                #Este argumento permite que haya repetidas en un mismo sobre
  )
}

# Paso 2: Testeamos la función con valores de prueba
genPaquete(figusTotal = 650, figusPaquete = 6)
genPaquete(figusTotal = 800, figusPaquete = 4)
genPaquete(figusTotal = 10000, figusPaquete = 10)

# c) Implementar una funcion cuantosPaquetes(figusTotal, figusPaquete) que dado el tamaño del album (figusTotal) y la cantidad de figuritas por paquete (figusPaquete) simule el llenado del album y devuelva cuántos paquetes se debieron adquirir para completarlo.

# Paso 1: Creamos la función
cuantosPaquetes <- function(figusTotal, figusPaquete){
  album <- numeric(figusTotal) # Crea un vector con tantos ceros como figutitas tenga el album
  n=0                          # Seteamos en cero la variable que va a contar la cantidad de sobres
  while(min(album)==0) {       # Si min(album)==0 quiere decir que todavía nos faltan figutitas
    aux <- sample(1:figusTotal,figusPaquete,T)
                               # Arma el sobre de figutitas de manera pseudo-aleatoria
    for(i in 1:length(aux)){   # Ciclo sobre las figuritas del sobre
      album[aux[i]] <- album[aux[i]] +1
                               # Suma 1 a cada figurita del album que vino en el sobre
    }
    n=n+1                      # Suma 1 a la cantidad de sobres
   }
  print(n)
}

# Paso 2: Testeamos la función con valores de prueba
cuantosPaquetes(640,5)
cuantosPaquetes(900,3)
cuantosPaquetes(10000,10)


# 2. Estimamos las distribuciones y esperanzas de las variables en estudio.
# Definimos 
# N como la cantidad de paquetes necesarios hasta completar el album, 
# C la cantidad de plata necesaria para completar el album, 
# T el tiempo necesario hasta completar el album, y 
# X el tiempo (en días) entre dos compras consecutivas de paquetes, que se sabe tiene distribución exponencial de parámetro 1. 
# A partir de Nrep=100 simulaciones, utilizando figusTotal=640, figusPaquete=5:
# a) Estimar y graficar las funciones de probabilidad de N y C y la función de densidad de T.
Nrep <- 100
results <- numeric(Nrep)
for (i in 1:Nrep) {
  album <- numeric(figusTotal) # Crea un vector con tantos ceros como figutitas tenga el album
  n=0                          # Seteamos en cero la variable que va a contar la cantidad de sobres
  while(min(album)==0) {       # Si min(album)==0 quiere decir que todavía nos faltan figutitas
    aux <- sample(1:figusTotal,figusPaquete,T)
    # Arma el sobre de figutitas de manera pseudo-aleatoria
    for(j in 1:length(aux)){   # Ciclo sobre las figuritas del sobre
      album[aux[j]] <- album[aux[j]] +1
      # Suma 1 a cada figurita del album que vino en el sobre
    }
    n=n+1                      # Suma 1 a la cantidad de sobres
  }
  results[i] <- n
}

hist(results, 
     breaks = 20, # Cantidad de barras
     freq = T     # T = Frecuencia, F = Densidad
     )

Precio_sobre = 15
Costos = results * Precio_sobre

hist(Costos, 
     breaks = 20, # Cantidad de barras
     freq = T     # T = Frecuencia, F = Densidad
     )

# Creamos una serie exponencial
T <-
rexp(      # Utilizamos la función de R que nos permite generar una secuencia de valores con distribucion exponencial
  n,    # El primer argumento es la cantidad de elementos de la serie, que va a ser igual a la cantidad de sobres
  0.5   # El segundo argumento refiere a "estimated rate of events for the distribution; this is usually 1/expected service life or wait time". 
)

hist(T, 
     breaks = 20, # Cantidad de barras
     freq = F     # T = Frecuencia, F = Densidad
)



# b) Estimar la esperanza de N, C y T
mean(results)
mean(Costos)
mean(T)
