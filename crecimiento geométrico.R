##Creamos los vectores para graficar el crecimiento de las mariposas

#Tamaño de la mariposa cada mes

N<- c(2,4,8,16,32)

#Tiempo en el cual se hizo la observación

month<- 6:10

plot(month, N, type="b", 
     xlab="Meses de muestreo", 
     ylab="Tamaño. N")

##Analizamos la proporción de incremento

tasa <- N[2:5]/N[1:4]
tasa

##Proyectemos el tamaño de la población

N0<- 2
t<- 0:10
lamda<- 2

##Nuestra formula Nt=lamda^t(N0)

N10<- lamda^t*(N0)
N10


##El crecimiento esta influido por el tamaño inicial

N0 <- c(10, 20, 30) 
lambda <- 2
time <- 0:4

Nt.s <- sapply(N0, function(n) n * lambda^time)
Nt.s


matplot(time, Nt.s, type="b", pch = 1:3) 

##Como podemos ver el incremento de la población es diferente en cada una
##son dependientes del tamaño inicial de la población

#Veamos la diferencia en el crecimiento en cada una

Nt.in<- Nt.s[2:5,]-Nt.s[1:4,]
Nt.in

#Que pasa si ponemos y en escala logaritmica?

par(mfcol=c(1,2))
matplot(time, Nt.s, type="b", 
        pch = 1:3)
matplot(time, Nt.s, type="b", 
        log = "y", pch = 1:3)

##La tasa de incremento afecta el crecimiento

N0 <- 100 
time <- 0:6
lambdas <- c(0.5, 1, 1.5)

N.all <- sapply(lambdas, function(x) N0 * x^time)
matplot(time, N.all, type="b", xlab = "Years", ylab = "N", pch = 1:3) 
abline(h = N0, lty = 3) 
text(0.5, 250, expression(lambda > 1), cex = 1.2) > text(0.5, 20, expression(lambda < 1), cex = 1.2)


##Pero en la vida real, los cambios entre años no es siempre igual

#veamos un caso real

library(primer)
data(sparrows)

plot(sparrows$Year,sparrows$Count, type="b")


#obtenemos el promedio de la tasa de incremento

t <- 5 
SS6 <- sparrows[1:(t+1), ]

#Calculamos λ para cada generacióndesde t a t+1, y calculamos
#las medias geométricas y aritmeticas.

SSgr <- SS6$Count[2:(t + 1)]/SS6$Count[1:t] 
lam.A <- sum(SSgr)/t 
lam.G <- prod(SSgr)^(1/t)

#graficamos las proyecciones
N0 <- SS6$Count[1] 
plot(0:t, SS6$Count, type="b", ylab = "Tamaño de la población proyectada") 
lines(0:t, N0 * lam.A^(0:t), lty = 2, col="red") 
lines(0:t, N0 * lam.G^(0:t), lty = 1, col="blue") 
legend(0, 70, c("Media aritmética", "Media Geométrica"), 
       title = "Proyecciones basadas en:", 
       lty = 2:1, bty = "n", xjust = 0, col=c("red", "blue"))



p1<- N0 * lam.A^(0:t)
pg1<-N0 * lam.G^(t)

##Ahora veamos que tan bueno es nuestro modelo para predecir 
##la evolución de la población

##Proyectar la población a 10 y 20 años y comparar con 
##los datos que tenemos
##incrementar 5 y 10 años más en el calculo de lambda 
##promedio y proyectar la población
str(sparrows)

#######################
#Crecimiento exponencial


r <- c(-0.03, -0.02, 0, 0.02, 0.03) 
N0 <- 2; t <- 1:100 
cont.mat <- sapply(r, function(ri) N0 * exp(ri * t))
layout(matrix(1:2, nrow = 1)) 

matplot(t, cont.mat, type = "l", ylab = "N", col = 1:5) 
legend("topleft", paste(rev(r)), lty = 5:1, col = c(1:5), 
       bty = "n", title = "r")


matplot(t, cont.mat, type = "l", ylab = "N", log = "y", col = 1:5)

#Podríamos hacer lo mismo con los datos de sparrows.
#Calcular r y proyectar la población


#Calculamos r

r<- log(lam.G)
t<- 1:36
crec.exp<- N0 * exp(r * t)
plot(t, crec.exp, type="l")
lines(t, sparrows$Count, col="red")













