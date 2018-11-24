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
legend("topright", legend = c("gama porazdelitev"),
       lwd=2, col = c("blue"), cex = 1)

skupni_graf <- plot(ecdf(vzorec), main="Porazdelitvena funkcija odskodnin", lwd=2,
                    xlab="Visina odskodnine", ylab="Porazdelitvena funkcija")
curve(pweibull(x, shape, scale), add=TRUE, from=0, to=5, col="blue", lwd = 2)
legend("right", legend = c("empiricna porazdelitev", "gama porazdelitev"),
       lwd=2, bty="n", col = c("black", "blue"), pch=c(19,NA))

#(d)
n=25 #velikost
p=0.5 #verjetnost

upanje_N <- n*p #Upanje binomske porazdelitve.
upanje_Y <- as.numeric(scale*gamma(1+(1/shape))) #Upanje Weibullove p.(vzorca)
upanje_S <- upanje_N*upanje_Y

var_N <- n*p*(1-p)
var_Y <- as.numeric((scale^2)*(gamma(1+(2/shape))-(gamma(1+(1/shape)))^2))
upanje_Y2 <- var_Y + (upanje_Y)^2
disperzija_S <- (var_Y*upanje_N) + ((upanje_Y)^2*var_N)

#2.NALOGA

#(a)
h <- 0.25 #dolzina koraka
n <- 40 #izberem na novo
diskretna <- discretize(pweibull(x,shape,scale), step=h, from=0, to=h*n,
                        method="rounding")

#(b)
graf_porazdelitve <- plot(stepfun(seq(0, (n-1)*h, h), diffinv(diskretna)),
                          main="Gama porazdelitev", do.points = FALSE,
                          ylab="Porazdelitvena funkcija", col="orange", lwd=2)
curve(pweibull(x,shape,scale), add=TRUE, lwd=1)

#(c)
panjer1 <- aggregateDist(method="recursive", model.freq="binomial",
                        model.sev=diskretna, x.scale=h, size=25,
                        prob=0.5)

#(d)
upanje_P <- mean(panjer1)
varianca_P <- sum(diff(panjer1)*knots(panjer1)^2)-upanje_P^2

#3.NALOGA

#(a)
binomska <- rbinom(10000, size=25, prob=0.5)
v <- rep(NA, 10000)

simulacija <- function(){
  for(i in 1:10000){
    v[i] <- sum(rweibull(binomska[i],shape,scale))
  }
  return(v)
}

#(b)
ocena_upanja <- mean(simulacija())
ocena_disperzije <- var(simulacija())
# Primerjava z (2.d): Upanje pri nalogi (2.d) je bilo za malenkost večje,
# vendar pa sta vrednosti skoraj enaki, razlikujeta se šele na 2.decimalki.
# Tudi varianca se ne razlikuje veliko, vendar pa za nekoliko več kot upanje.
# Očitno je simulacija dobra za oceno upanja in variance.

#(c)
plot(panjer1, col="black", main="Aggregate Claim Amount Distribution", cex=0.5)
plot(ecdf(simulacija()), col="green", main="Aggregate Claim Amount Distribution",
     xlab="", ylab="", add=TRUE)
legend("bottomright", legend = c("Panjerjev algoritem", "Monte Carlo simulacija"),
       lwd=2, bty="n", col = c("black", "green"))

# Obe porazdelitveni funkciji se lepo ujemata.
