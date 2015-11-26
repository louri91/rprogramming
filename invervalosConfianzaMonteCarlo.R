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

