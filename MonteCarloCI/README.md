Cálculo de Intervalos de Confianza de Monte Carlo
======

Ejemplo 1
---------
Número de simulaciones
```R
m <- 1000
```
Tamaño de la muestra
```R
n <- 100  
```

Genera una matriz de nxm con valores N(896,174)
La media de la muestra y la desviación estándar muestral para esta muestra aleatoria son xn = 896 y sn = 174 y la distribución de las puntuaciones es aproximadamente normal
```R
x <- matrix(rnorm((m*n),896,174),nrow=m)
```

Media de cada una de las filas de X
```R
means <- apply(x,1,mean)
```

Desviación típica de cada una de las filas de x
```R
sdevs <- sqrt(apply(x,1,var))
```

Mediana de cada una de las filas de x
```R
medians <- apply(x,1,median)
```

Intervalo de cada una de las filas de x
```R
ranges <- apply(apply(x,1,range),2,diff)
```

Crea una ventana de 2x2
```R
par(mfrow=c(2,2))
```

Muestra un histograma de las medias de Monte Carlo
```R
hist(means,xlab="Media", ylab="Frecuencia", main="Media de Monte Carlo",
     angle=45,density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

Muestra un histograma de la desviación típica de Monte Carlo
```R
hist(sdevs,xlab="Desviación típica",ylab="Frecuencia",angle=45,
density=12,main="Desviación típica de Monte Carlo",
cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

Muestra un histograma de las medianas de Monte Carlo
```R
hist(medians,xlab="Mediana",ylab="Frecuencia",main="Medianas de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```

Muestra un histograma de los intervalos de Monte Carlo
```R
hist(ranges,xlab="Intervalos",ylab="Frecuencia",main="Intervalos de Monte Carlo",angle=45,
     density=12,cex.axis=1.5,cex.lab=1.6,cex.main=1.6)
```


Ahora tenemos m = 1000 medias, medianas, desviaciones típicas, y los intervalos de la distribución N (896, 174)
¿Cómo encontramos intervalos de confianza para la población homóloga?

1.Primero tenemos que ordenar los valores estimados del más pequeño al más grande.

Ordena las medias de menor a mayor
```R
means <- sort(means)
```

Ordena las desviaciones típicas de menor a mayor
```R
sdevs <- sort(sdevs)
```

Ordena las medianas de menor a mayor
```R
medians <- sort(medians)
```

Ordena los intervalos de menor a mayor
```R
ranges <- sort(ranges)
```

2.Dado que lo que queremos encontrar es un límite superior e inferior de tal manera que aproximadamente el 95% de las estimaciones estén dentro de estos límites.
Una forma de obtener un IC del 95% es encontrar el percentil 2,5 y el percentil 97,5 de la distribución de Monte Carlo.
En el ejemplo, ya que hay m = 1000 valores estimados, ¿qué valores de la lista ordenada corresponden a los percentiles 2,5 y 97,5?
Se debe tener en consideración que podemos elegir cualquier par de valores dentro de los cuales el 95% de las observaciones mienten y lo llaman un intervalo de confianza del 95% para el parámetro. Sin embargo, la convención es construir el intervalo utilizando el "medio" 95% de la distribución, de acuerdo con los percentiles.

Definimos el coeficiente de confianza (95%)
```R
CC <- .95
```

Calculamos la tail probability (.025)
```R
tail <- (1-CC)/2
```

Calculamos el elemento por debajo del percentil 2.5
```R
numero_menor1 <- trunc(tail*m+.5)
```
Calculamos el elemento por encima del percentil 2.5
```R
numero_menor2 <- ceiling(tail*m+.5)
```
Calculamos el peso de numero_menor1
```R
lp <- tail*m+.5-numero_menor1
```

Calculamos el elemento por debajo del percentil 97.5
```R
numero_mayor1 <- trunc((1-tail)*m+.5)
```
Calculamos el elemento por encima del percentil 97.5
```R
numero_mayor2 <- ceiling((1-tail)*m+.5)
```

Calculamos el peso de numero_mayor1
```R
up <- 1-lp
```

Intervalo de Confianza 95% límite menor para la media
```R
menor <- (1-lp)*means[numero_menor1]+lp*means[numero_menor2]
```
Intervalo de Confianza 95% límite mayor para la media
```R
mayor <- (1-up)*means[numero_mayor1]+up*means[numero_mayor2]
```

Imprime los límites menor y mayor redondeados a 4 decimales
```R
cat("Intervalo de Confianza de Monte Carlo para la media = (",
    round(menor,4), round(mayor,4),") \n")
```

Intervalo de Confianza 95% límite menor para la desviación típica
```R
menor <- (1-lp)*sdevs[numero_menor1]+lp*sdevs[numero_menor2]
```
Intervalo de Confianza 95% límite mayor para la desviación típica
```R
mayor <- (1-up)*sdevs[numero_mayor1]+up*sdevs[numero_mayor2]
```

Imprime los límites menor y mayor redondeados a 4 decimales
```R
cat("Intervalo de Confianza de Monte Carlo para la desviación típica = (",
    round(menor,4), round(mayor,4),") \n")
```

Intervalo de Confianza 95% límite menor para la mediana
```R
menor <- (1-lp)*medians[numero_menor1]+lp*medians[numero_menor2]
```
Intervalo de Confianza 95% límite mayor para la mediana
```R
mayor <- (1-up)*medians[numero_mayor1]+up*medians[numero_mayor2]
```

Imprime los límites menor y mayor redondeados a 4 decimales
```R
cat("Intervalo de Confianza de Monte Carlo para la mediana = (",
    round(menor,4), round(mayor,4),") \n")
```

Intervalo de Confianza 95% límite menor para el intervalo
```R
menor <- (1-lp)*ranges[numero_menor1]+lp*ranges[numero_menor2]
```
Intervalo de Confianza 95% límite mayor para el intervalo
```R
mayor <- (1-up)*ranges[numero_mayor1]+up*ranges[numero_mayor2]
```

Imprime los límites menor y mayor redondeados a 4 decimales
```R
cat("Intervalo de Confianza de Monte Carlo para el intervalo = (",
    round(menor,4), round(mayor,4),") \n")
```
Resultados:

|         | Límite inferior | Límite superior  |
| ------------- |--------:| -----:|
| Media | **861.0241** | **929.5535** |
| Desviación Típica | **150.2904** | **196.1969** |
| Mediana | **854.5643** | **940.0827** |
| Intervalo | **698.7373** | **1101.841** |

Conclusiones:
Si comparamos los resultados obtenidos con los histogramas del principio, vemos que los resultados tienen coherencia

![alt text](https://github.com/louri91/rprogramming/raw/master/MonteCarloCI/img/Rplot.png "Histogramas")


Ejemplo 2
----------
Suponemos un administrador de una web, preocupado por tener ancho de banda suficiente para una web concreta. Está interesado en la distribución del máximo número de visitantes a la web en cualquier momento. De los datos que tiene, el administrador se ha dado cuenta que la distribución del número de visitantes de una web concreta puede ser modelada como una distribución gamma, con parámetros (shape)alpha = 3 y (rate)betha = 5. Considera encontrar un intervalo de confianza aproximado al 95% para el número máximo de visitantes de la web en un determinado momento utilizando los métodos de Monte Carlo. ¿Dependerá el intervalo del tamaño de la muestra utilizada?
Para poder contestar a la pregunta, los intervalos de confianza de Monte Carlo están basados en una muestra de 1000 ejemplos de tamaño n, para n=10,50,100,...,500,1000, resumida en la siguiente tabla

| n | Media(Max) | Intervalo de Confianza de Monte Carlo 95% | Anchura del intervalo  |
| ------------- |--------| -----| -----|
| 10 | 30.82 | (17.76,52.08) | 34.32 |
| 50 | 41.52 | (29.19,60.81) | 31.62 |
| 100 | 45.45 | (33.56,62.82) | 29.26 |
| 150 | 48.17 | (36.49,67.50) | 31.01 |
| 200 | 50.09 | (38.84,67.13) | 28.29 |
| 250 | 51.05 | (39.02,68.44) | 29.42 |
| 300 | 52.35 | (40.21,69.78) | 29.57 |
| 350 | 52.73 | (41.52,71.19) | 29.67 |
| 400 | 54.35 | (42.64,72.32) | 29.68 |
| 450 | 54.67 | (42.77,73.03) | 30.26 |
| 500 | 55.48 | (43.45,72.24) | 28.79 |
| 1000 | 59.91 | (48.75,79.78) | 31.03 |
