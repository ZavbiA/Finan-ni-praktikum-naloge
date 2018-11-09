library(actuar)

#1.NALOGA

#(a)

vzorec <- scan("vzorec3.txt")
histogram1 <- hist(vzorec,
     xlab="Visina odskodnine", main="Histogram odskodnin", col="lightblue")
#(b)
#Imam Weibullovo porazdelitev.

parametri <- mde(vzorec,
                 measure="CvM", start=list(shape=1, scale=1), pweibull)
shape <- parametri$estimate[1]
scale <- parametri$estimate[2]

#(c)
histogram2 <- hist(vzorec, main="Histogram odskodnin", col="lightblue",
     xlab="Visina odskodnine", ylab="Density", probability=TRUE)
curve(dweibull(x, shape, scale), add=TRUE, from=0, to=5, col="blue", lwd = 2)

skupni_graf <- plot(ecdf(vzorec), main="Porazdelitvena funkcija odskodnin",
                    xlab="Visina odskodnine", ylab="Porazdelitvena funkcija")
curve(pweibull(x, shape, scale), add=TRUE, from=0, to=5, col="blue", lwd = 2)

#(d)
n=25 #velikost
p=0.5 #verjetnost

upanje_N <- n*p #Upanje binomske porazdelitve.
upanje_Y <- scale*gamma(1+(1/shape)) #Upanje Weibullove p.(vzorca)
upanje_S <- upanje_N*upanje_Y

var_N <- upanje_N*(1-p)
upanje_N2 <- upanje_N*n
var_Y <- (scale^2)*(gamma(1+(2/shape))-(gamma(1+(1/shape)))^2)
disperzija_S <- var_Y*upanje_N+upanje_N2*var_N

#2.NALOGA

#(a)
h <- 0.25 #dolzina koraka
n <- 100 #izberem na novo
diskretna <- discretize(pweibull(x,shape,scale), step=h, from=0, to=h*n,
                        method="rounding")

#(b)
graf_porazdelitve <- plot(stepfun(seq(0, (n-1)*h, h), diffinv(diskretna)),
                          main="Gama porazdelitev", do.points = FALSE,
                          ylab="Porazdelitvena funkcija")
curve(pweibull(x,shape,scale), add=TRUE, col="orange", lwd=2)

#(c)
panjer <- aggregateDist(method='recursive', model.freq="binomial", model.sev=diskretna, x.scale=h, size=25, prob=0.5, maxit=1000)

#(d)
upanje_P <- mean(panjer)
varianca_P <- as.numeric(knots(panjer)^2 %% diff(panjer)) - upanje_P^2

#3.NALOGA

#(a)


#(b)


#(c)


#(d)

