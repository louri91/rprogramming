################################################################
#                                                              #
# Ejemplo 2. Cálculo de Intervalos de Confianza de Monte Carlo #
#                                                             #
##############################################################

n <- c(10,seq(50,500,50),1000)              # Vector of sample sizes.
for (i in 1:12){                            # Beginning of for loop.
  m <- 1000                                 # Number of simulations.
  x <- matrix(rgamma((m*n[i]),3,.2),nrow=m) # Generate an mxn matrix of gamma(3,5) values.
  maxs <- sort(apply(x,1,max))              # Computes the max in each row of x.
  lower <- (1-lp)*maxs[numero_menor1]+lp*maxs[numero_menor2] # Lower 95% CI limit for max.
  upper <- (1-up)*maxs[numero_mayor1]+up*maxs[numero_mayor2]   # Upper 95% CI limit for max.
  cat("Media(Máximo) = ",mean(maxs),"     ")     # Prints the mean of the maxima.
  cat("Intervalo de Confianza de Montecarlo para el Máximo = (",              # Print the limits of the CI,
      round(lower,4),round(upper,4),") \n")        #   rounded to 4 decimal places.
}

