est1 <- function(...){
  estimacion <- mean(...)*2
  return(estimacion)
}

est2 <- function(datos){
  maximo <- 0
  for(i in datos){
    maximo <- max(maximo, i)
  }
  return(maximo)
}

#muestra1 <- scan()
#1.17 1.75 0.28 2.56 2.36 0.36 1.82 0.24 1.17 1.86

muestr1 <- c(1.17, 1.75, 0.28, 2.56, 2.36, 0.36, 1.82, 0.24, 1.17, 1.86)

est1_muestra1 <- est1(muestra1)

est2_muestra2 <- est2(muestra1)

muestra2 <- c(0.66, 0.07, 0.62, 0.65, 1.33, 0.40, 1.17, 1.11, 2.01, 2.98)

est1_muestra2 <- est1(muestra2)

est2_muestra2 <- est2(muestra2)

Nrep <- 10000

estimaciones_funcion <- function(n, x){
  estimaciones <- rep(NA, Nrep)
  for(i in 1:Nrep){
    datos_n <- runif(n, min = 0, max = 10)
    if(x == 1){
      estimaciones[i] <- est1(datos_n)
    }
    else{
      estimaciones[i] <- est2(datos_n)
    }
  }
  return(estimaciones)
}

estimacion1_5 <- estimaciones_funcion(5, 1)
estimacion1_30 <- estimaciones_funcion(30, 1)
estimacion1_100 <- estimaciones_funcion(100, 1)


par(mfrow = c(1,3))
hist(estimacion1_5, probability = T, xlim = c(0,20), ylim  = c(0, 0.6))
hist(estimacion1_30, probability = T, xlim = c(0,20), ylim = c(0, 0.6))
hist(estimacion1_100, probability = T, xlim = c(0, 20), ylim = c(0, 0.6))

boxplot(estimacion1_5, horizontal = T)
boxplot(estimacion1_30, horizontal = T)
boxplot(estimacion1_100, horizontal = T)

#Tiene distribucion aproximadamente normal
#Cuando n aumenta, la varianza disminuye, ya que var(tetha sombrero) = var(2*Xn) = 4*var(Xn) = 4*var(X)/n = 1/3n y eso es decreciente en n
#Tetha sombrero se concentran alrededor de E(X), es decir de 10, y esto se debe a LGN
#El estimador que usaria para estimar E(Tetha sombrero), usaria la media muestral, es decir 1/n*(sumatoria(desde: i = 1, hasta: N, de: Tetha sombrero sub i))

estimacion2_5 <- estimaciones_funcion(5, 2)
estimacion2_30 <- estimaciones_funcion(30, 2)
estimacion2_100 <- estimaciones_funcion(100, 2)

par(mfrow = c(1,3))
hist(estimacion2_5, probability = T, xlim = c(0,10), ylim  = c(0, 8))
hist(estimacion2_30, probability = T, xlim = c(0,10), ylim = c(0, 8))
hist(estimacion2_100, probability = T, xlim = c(0, 10), ylim = c(0, 8))

boxplot(estimacion2_5, horizontal = T)
boxplot(estimacion2_30, horizontal = T)
boxplot(estimacion2_100, horizontal = T)

par(mfrow = c(2, 3))
