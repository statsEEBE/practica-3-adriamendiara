#Ensayo de Bernoulli

x <- c(0,1)
f <- c(0.68, 0.32)

plot(x, f, type="h", ylim=c(0,1), col="red")
points(x,f,pch=16, col="red")

n <- 43
muestra <- sample(x, n, f, replace=TRUE) #muestra de 43 personas,con las probailidades de que digan si o no

table(muestra)
mean(muestra)

bar <- barplot(table(muestra)/n, ylim=c(0,1))
lines(bar, f, type="h", ylim=c(0,1), col="red")
points(bar, f,pch=16, col="red")


muestra <- sample(x, n, f, replace=TRUE)
sum(muestra)


#Cálculo de la probabilidad de que 13 digan sí
Y <- function(i){sum(sample(x, n, f, replace=TRUE))}
Y(1)
Y(2)

set.seed(123) #generador de los mismos datos aleatorios

m <- 400000 #numero de repeticiones de la encuesta
encuestas <- sapply(1:m, Y) #todos los resultados de las m encuestas

fr <- table(encuestas)/m
fr["13"]
barplot(fr)


#Probabilidad de que 13 personas de 43 tengan 2 teles.
dbinom(13,43,0.32)

br <- barplot(fr)
lines(br, dbinom(2:29, 43, 0.32), type="h", ylim=c(0,1), col="red")
points(br, dbinom(2:29, 43, 0.32), pch=16, col="red")



#Apartado b, probabilidad de que 16 o menos tengan 2 televisores?

dbinom(16, 44, 0.32)
plot(0:43, dbinom(0:43,44,0.32), type="h", col="red")

#como es menos de calculamos la probabilidad acumulada
pbinom(16,44,0.32)


#Apartado c
n <- 24
x <- c(0,1)
f <- c(0.32,0.68)

set.seed(123)
m <- 400000
Xstar <- 
encuestas <- sapply(1:m,Xstar)
mean(encuestas)
n*0.68
var(encuestas)
n*0.68*0.32

qbinom(0.25,24, 0.68)
plot(0:24, dbinom(0:24,24,0.68), type="h", col="red")
