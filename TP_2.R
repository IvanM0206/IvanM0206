#Ejercicio 1 con X~E(1/12)
# E(X) = 1/(1/12) = 12 y Var(X) = 1/(1/12**2) = 144

exp_N_infty = rexp(10000, rate = 1/12)
mean(exp_N_infty)
var(exp_N_infty)
hist(exp_N_infty, plot = T, probability = T)

hist(exp_N_infty, plot = T, freq = T, xlim = c(0, 70), ylim = c(0, 6000), xaxt = "n")
axis(1, at = seq(0, 80, 10))

#Se aproxima a una exponencial de parametro 1/12
#Tiene asimetria a derecha, ya que la probabilidad de que sea mayor a la esperanza (12) es mucho menor a que sea menor a la esperanza

curve(1/12*exp(-(1/12)*x), add = T, col = "blue")

grilla_X = seq(0, 140, 0.1)
curve(dexp(x, 1/12), add = T, col = "red")


#Ejercicio 2, 10.000 realizaciones del promedio con n = 5


calculo_de_promedios <- function(n){
  promedios_n = c()
  for(i in 1:10000){
    promedios_n <- append(promedios_n, mean(rexp(n, rate = 1/12)))
  } 
  return(promedios_n)
}


promedios_exp5 = calculo_de_promedios(5)

hist(promedios_exp5, plot = T, probability = T, xlim = c(0, 32), xaxt = "n")
axis(1, at = seq(0, 32, 4))
#Tiene asimetria a la derecha

curve(dnorm(x, mean = 12, sd= sqrt(144/5)), add = T, col = "red", lwd = 2)

#No es una buena aproximacion y eso se debe a que tomo un n bajo

promedios_exp30 = calculo_de_promedios(30)
promedios_exp100 = calculo_de_promedios(100)

hist(promedios_exp30, plot = T, probability = T, xlim = c(0,32), xaxt = "n")
axis(1, at = seq(0, 32, 4))

hist(promedios_exp100, plot = T, probability = T, xlim = c(0,32), xaxt = "n")
axis(1, at = seq(0, 32, 4))

curve(dnorm(x, mean = 12, sd= sqrt(144/30)), add = T, col = "blue", lwd = 2)
curve(dnorm(x, mean = 12, sd= sqrt(144/100)), add = T, col = "green", lwd = 2)

par(mfrow = c(1,3))




#Ejercicio 3 con X~U(10,14)
#E(X) = (14+10)/2 = 12 Var(X) = ((14-10)**2)/12 = 4/3

unif_N_infty = runif(10000, min = 10, max = 14)
mean(unif_N_infty)
var(unif_N_infty)
hist(unif_N_infty, plot = T, probability = T)

#Se aproxima a una uniforme entre 10 y 14

curve(dunif(x, min = 10, max = 14), add = T, col = "blue")


#Ejercicio 2, 10.000 realizaciones del promedio con n = 5

calculo_de_promedios_unif <- function(n){
  promedios_n = c()
  for(i in 1:10000){
    promedios_n <- append(promedios_n, mean(runif(n, min = 10, max = 14)))
  } 
  return(promedios_n)
}


promedios_unif5 = calculo_de_promedios_unif(5)

hist(promedios_unif5, plot = T, probability = T, xlim = c(10, 14))

curve(dnorm(x, mean = 12, sd= sqrt(4/15)), add = T, col = "red", lwd = 2)

#No es una buena aproximacion y eso se debe a que tomo un n bajo

promedios_unif30 = calculo_de_promedios_unif(30)
promedios_unif100 = calculo_de_promedios_unif(100)

hist(promedios_unif30, plot = T, probability = T, xlim = c(10, 14))

hist(promedios_unif100, plot = T, probability = T, xlim = c(10,14))

curve(dnorm(x, mean = 12, sd= sqrt(4/90)), add = T, col = "blue", lwd = 2)
curve(dnorm(x, mean = 12, sd= sqrt(4/300)), add = T, col = "green", lwd = 2)


#Ejercicio 4 con X~N(12,4/3)
#E(X) = (14+10)/2 = 12 Var(X) = 4/3

norm_N_infty = rnorm(10000, mean = 12, sd = sqrt(4/3))
mean(norm_N_infty)
var(norm_N_infty)
hist(norm_N_infty, plot = T, probability = T)

#Se aproxima a una normal de media 12 y varianza 4/3

curve(dnorm(x, mean = 12, sd = sqrt(4/3)), add = T, col = "blue")


#Ejercicio 2, 10.000 realizaciones del promedio con n = 5

calculo_de_promedios_norm <- function(n){
  promedios_n = c()
  for(i in 1:10000){
    promedios_n <- append(promedios_n, mean(rnorm(n, mean = 12, sd = sqrt(4/3))))
  } 
  return(promedios_n)
}


promedios_norm5 = calculo_de_promedios_norm(5)

hist(promedios_norm5, plot = T, probability = T, xlim = c(11,13))

curve(dnorm(x, mean = 12, sd= sqrt(4/15)), add = T, col = "red", lwd = 2)

#No es una buena aproximacion y eso se debe a que tomo un n bajo

promedios_norm30 = calculo_de_promedios_norm(30)
promedios_norm100 = calculo_de_promedios_norm(100)

hist(promedios_norm30, plot = T, probability = T, xlim = c(11, 13))

hist(promedios_norm100, plot = T, probability = T, xlim = c(11, 13))

curve(dnorm(x, mean = 12, sd= sqrt(4/90)), add = T, col = "blue", lwd = 2)
curve(dnorm(x, mean = 12, sd= sqrt(4/300)), add = T, col = "green", lwd = 2)
