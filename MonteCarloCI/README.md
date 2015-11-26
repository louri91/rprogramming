## Cálculo de Intervalos de Confianza de Monte Carlo
### Número de simulaciones
```R
m <- 1000
```
### Tamaño de la muestra
```R
n <- 100  
```

### Genera una matriz de nxm con valores N(896,174)
#### La media de la muestra y la desviación estándar muestral para esta muestra aleatoria son xn = 896 y sn = 174 y la distribución de las puntuaciones es aproximadamente normal
```R
x <- matrix(rnorm((m*n),896,174),nrow=m)
```

### Media de cada una de las filas de X
```R
means <- apply(x,1,mean)
```

### Desviación típica de cada una de las filas de x
```R
sdevs <- sqrt(apply(x,1,var))
```

### Mediana de cada una de las filas de x
```R
medians <- apply(x,1,median)
```

### Intervalo de cada una de las filas de x
```R
ranges <- apply(apply(x,1,range),2,diff)
```

### Crea una ventana de 2x2
```R
par(mfrow=c(2,2))
```

### Muestra un histograma de las medias de Monte Carlo
```R
hist(means,xlab="Media", ylab="Frecuencia", main="Media de Monte Carlo",
     angle=45,density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

### Muestra un histograma de la desviación típica de Monte Carlo
```R
hist(sdevs,xlab="Desviación típica",ylab="Frecuencia",angle=45,density=12,main="Desviación típica de Monte Carlo",cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

### Muestra un histograma de las medianas de Monte Carlo
```R
hist(medians,xlab="Mediana",ylab="Frecuencia",main="Medianas de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

### Muestra un histograma de los intervalos de Monte Carlo
```R
hist(ranges,xlab="Intervalos",ylab="Frecuencia",main="Intervalos de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```


## Ahora tenemos m = 1000 medias, medianas, desviaciones típicas, y los intervalos de la distribución N (896, 174)
## ¿Cómo encontramos intervalos de confianza para la población homóloga?

### 1. Primero tenemos que ordenar los valores estimados del más pequeño al más grande.

#### Ordena las medias de menor a mayor
```R
means <- sort(means)
```

#### Ordena las desviaciones típicas de menor a mayor
```R
sdevs <- sort(sdevs)
```

#### Ordena las medianas de menor a mayor
```R
medians <- sort(medians)
```

#### Ordena los intervalos de menor a mayor
```R
ranges <- sort(ranges)
```
