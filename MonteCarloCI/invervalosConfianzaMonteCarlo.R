#####################################################
#                                                   #
# Cálculo de Intervalos de Confianza de Monte Carlo #
#                                                   #
#####################################################

m <- 1000 # Número de simulaciones
n <- 100  # Tamaño de la muestra

# Genera una matriz de nxm con valores N(896,174)
# La media de la muestra y la desviación estándar muestral para esta muestra aleatoria son xn = 896 y sn = 174
# y la distribución de las puntuaciones es aproximadamente normal

x <- matrix(rnorm((m*n),896,174),nrow=m)

# Media de cada una de las filas de X
means <- apply(x,1,mean)

 # Desviación típica de cada una de las filas de x
sdevs <- sqrt(apply(x,1,var))

# Mediana de cada una de las filas de x
medians <- apply(x,1,median)

# Intervalo de cada una de las filas de x
ranges <- apply(apply(x,1,range),2,diff) 

# Crea una ventana de 2x2
par(mfrow=c(2,2))

# Muestra un histograma de las medias de Monte Carlo
hist(means,xlab="Media", ylab="Frecuencia", main="Media de Monte Carlo",
     angle=45,density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6) 

# Muestra un histograma de la desviación típica de Monte Carlo
hist(sdevs,xlab="Desviación típica",ylab="Frecuencia",angle=45,density=12,main=
       "Desviación típica de Monte Carlo",cex.axis=1.5,cex.lab=1.6,cex.main=1.6)

# Muestra un histograma de las medianas de Monte Carlo
hist(medians,xlab="Mediana",ylab="Frecuencia",main="Medianas de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)

# Muestra un histograma de los intervalos de Monte Carlo
hist(ranges,xlab="Intervalos",ylab="Frecuencia",main="Intervalos de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)

# Ahora tenemos m = 1.000 medias, medianas, desviaciones típicas, y los intervalos de la distribución N (896, 174)
# ¿Cómo encontramos intervalos de confianza para la población homóloga?

# 1. Primero tenemos que ordenar los valores estimados del más pequeño al más grande.

# Ordena las medias de menor a mayor
means <- sort(means)

# Ordena las desviaciones típicas de menor a mayor
sdevs <- sort(sdevs)

# Ordena las medianas de menor a mayor
medians <- sort(medians)

# Ordena los intervalos de menor a mayor
ranges <- sort(ranges) 

# 2. Dado que lo que queremos encontrar es un límite superior e inferior 
# de tal manera que aproximadamente el 95% de las estimaciones estén dentro 
# de estos límites. 
# Una forma de obtener un IC del 95% es encontrar el percentil 2,5 
# y el percentil 97,5 de la distribución de Monte Carlo. 
# En el ejemplo, ya que hay m = 1000 valores estimados, 
# ¿qué valores de la lista ordenada corresponden a los 
# percentiles 2,5 y 97,5? 
# Se debe tener en consideración que podemos elegir cualquier par de valores
# dentro de los cuales el 95% de las observaciones mienten y lo llaman un intervalo de confianza
# del 95% para el parámetro. Sin embargo, la convención es construir el intervalo utilizando el "medio" 95% de la distribución, de acuerdo con los percentiles.

# Definimos el coeficiente de confianza (95%)
CC <- .95
# Calculamos la tail probability (.025)
tail <- (1-CC)/2

# Calculamos el elemento por debajo del percentil 2.5
numero_menor1 <- trunc(tail*m+.5)
# Calculamos el elemento por encima del percentil 2.5
numero_menor2 <- ceiling(tail*m+.5)
# Calculamos el peso de numero_menor1
lp <- tail*m+.5-numero_menor1

# Calculamos el elemento por debajo del percentil 97.5
numero_mayor1 <- trunc((1-tail)*m+.5)
# Calculamos el elemento por encima del percentil 97.5
numero_mayor2 <- ceiling((1-tail)*m+.5)

# Calculamos el peso de numero_mayor1
up <- 1-lp

# Intervalo de Confianza 95% límite menor para la media 
menor <- (1-lp)*means[numero_menor1]+lp*means[numero_menor2]
# Intervalo de Confianza 95% límite mayor para la media 
mayor <- (1-up)*means[numero_mayor1]+up*means[numero_mayor2]

# Imprime los límites menor y mayor redondeados a 4 decimales
cat("Intervalo de Confianza de Monte Carlo para la media = (", 
    round(menor,4), round(mayor,4),") \n")

# Intervalo de Confianza 95% límite menor para la desviación típica 
menor <- (1-lp)*sdevs[numero_menor1]+lp*sdevs[numero_menor2]
# Intervalo de Confianza 95% límite mayor para la desviación típica 
mayor <- (1-up)*sdevs[numero_mayor1]+up*sdevs[numero_mayor2]

# Imprime los límites menor y mayor redondeados a 4 decimales
cat("Intervalo de Confianza de Monte Carlo para la desviación típica = (", 
    round(menor,4), round(mayor,4),") \n")

# Intervalo de Confianza 95% límite menor para la mediana 
menor <- (1-lp)*medians[numero_menor1]+lp*medians[numero_menor2]
# Intervalo de Confianza 95% límite mayor para la mediana 
mayor <- (1-up)*medians[numero_mayor1]+up*medians[numero_mayor2]

# Imprime los límites menor y mayor redondeados a 4 decimales
cat("Intervalo de Confianza de Monte Carlo para la mediana = (", 
    round(menor,4), round(mayor,4),") \n")

# Intervalo de Confianza 95% límite menor para el intervalo 
menor <- (1-lp)*ranges[numero_menor1]+lp*ranges[numero_menor2]
# Intervalo de Confianza 95% límite mayor para el intervalo 
mayor <- (1-up)*ranges[numero_mayor1]+up*ranges[numero_mayor2]

# Imprime los límites menor y mayor redondeados a 4 decimales
cat("Intervalo de Confianza de Monte Carlo para el intervalo = (", 
    round(menor,4), round(mayor,4),") \n")

