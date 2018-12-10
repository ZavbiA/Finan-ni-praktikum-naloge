#3.VAJE
library(combinat)
library(Rlab)

#NALOGA 1
#(a)
SO <- 50
u <- 1.05
d <- 0.95
U <- 5
R <- 0.03
T <- 3

S0 <- c(50, 50, 50, 50, 50)
S1 <- c(52.5, 52.5, 47.5, 47.5, 52.5)
S2 <- c(49.88, 55.12, 49.88, 45.12, 49.88)
S3 <- c(52.37, 57.88, 47.38, 47.38, 52.37)
S4 <- c(49.75, 60.78, 45.01, 49.75, 54.99)
S5 <- c(52.24, 63.81, 42.76, 52.24, 57.74)

X <- c(0,0,0,0,0)
Y <- c(0,0,0,0,0)

tabela <- data.frame(S0, S1, S2, S3, S4, S5, X, Y)
for (i in 1:5){
  tabela$X[i] = max(0, max(tabela[i,(T+1):(U+1)]) - max(tabela[i, 1:T]))}

for (i in 1:5){
  tabela$Y[i] = max(0, min(tabela[i,(T+1):(U+1)]) - min(tabela[i, 1:T]))}

#(b)

izplacilo <- function(vrsta, T, type){
  if (type == "call") {
    return(max(max(vrsta[(T+1):length(vrsta)]) - max(vrsta[1:T]), 0))}
  else {
    return(max(min(vrsta[(T+1):length(vrsta)]) - min(vrsta[1:T]), 0))}
}

#Pri vseh testih funkcija vraca pravilne rezultate.

#NALOGA2
#(a)
q=(1+R-d)/(u-d)

binomski <- function(SO, u, d, U, R, T, type){
  q=(1+R-d)/(u-d) #Treba imeti znotraj funkcije, ker novi parametri.
  mozni <- hcube(rep(2,U), translation = -1) #Drevo stanj: 1=gor, 0=dol
  mozni2 <- d**(1-mozni) * u**(mozni)
  n <- rowSums(mozni) #Vektor, ki za vrstico sesteje, kolikokrat gre gor.
  koncne_verjetnosti <- q^n *(1-q)^(U-n) #Vektor verjetnosti koncnih stanj
  
  mozni2 <- t(apply(mozni2, 1, cumprod))
  vrednosti <- cbind(SO, SO*mozni2)
  
  izplacilo <- apply(vrednosti, 1, function(x) izplacilo(x,T,type))
  izkupicek <- sum(izplacilo*koncne_verjetnosti)
  
  return (izkupicek/(1+R)^U)}

#Preverim:
binomski(SO, u, d, U, R, T, type="call")
binomski(SO, u, d, U, R, T, type="put")

#(b)

monte <- function(SO, u, d, U, R, T, type, N){
  q=(1+R-d)/(u-d)
  s <- matrix(rbinom(U*N, 1, q), N, U)
  stanja <- d**(1-s) * u**(s)
  
  n <- rowSums(s) #Vektor, ki za vrstico sesteje, kolikokrat gre gor.
  koncne_verjetnosti <- q^n *(1-q)^(U-n)
  
  stanja <- t(apply(stanja, 1, cumprod))
  vrednosti <- cbind(SO, SO*stanja)
  
  izplacilo <- apply(vrednosti, 1, function(x) izplacilo(x,T,type))
  izkupicek = sum(izplacilo)/length(izplacilo)
  return (izkupicek/(1+R)^U)}

#Preverim:
monte(50,1.05,0.9,10,0.03,5,"call",100)
monte(70,1.05,1,7,0,5,"put",2000)
monte(90, 1.15, 0.8 , 10, 0.01, 3, "call",50000)

#NALOGA3
#(a)
N1 <- c()
N2 <- c()
N3 <- c()
M <- 100

for (i in c(1:M)){
  N1 <- c(N1, monte(60,1.05,0.95,15,0.01,8,"put",10))
  N2 <- c(N2, monte(60,1.05,0.95,15,0.01,8,"put",100))
  N3 <- c(N3, monte(60,1.05,0.95,15,0.01,8,"put",1000))}

cena_binomske <- binomski(60,1.05,0.95,15,0.01,8,"put")
#Cena premije, dobljena z binomskim modelom.

min <- floor(min(c(N1,N2,N3))) 
max <- ceiling(max(c(N1,N2,N3)))

#(b)

#histogram N1
pov.N1 <- mean(N1) #Povprecje N1
odklon.N1 <- sqrt(var(N1)) #Standardni odklon N1
x1_odklon_desno <- cena_binomske + odklon.N1
x1_odklon_levo <- cena_binomske - odklon.N1

histogram1 <- hist(N1, main = "Monte Carlo: N=10", col = "yellow",
                  xlab = "Premija", xlim = c(0, 12))
abline(v = pov.N1, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0=0, x1 = x1_odklon_desno, col="green", length=0.1 )
arrows(x0 = cena_binomske, y0=0, x1 = x1_odklon_levo, col="green", length=0.1 )

legend("topright", legend = c("Monte Carlo", "analiza modela"),
       col = c("green", "red"), bty="n", cex=0.8, lty=c("solid","dashed"))

#histogram N2
pov.N2 <- mean(N2) #povprecje N2
odklon.N2 <- sqrt(var(N2)) #standardni odklon N2
x2_odklon_desno <- cena_binomske + odklon.N2
x2_odklon_levo <- cena_binomske - odklon.N2


histogram2 <-hist(N2, main = "Monte Carlo: N=100", col = "yellow",
                  xlab = "Premija", xlim = c(0, 12))
abline(v= pov.N2, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0=0, x1 = x2_odklon_desno, col="green", length=0.1 )
arrows(x0 = cena_binomske, y0=0, x1 = x2_odklon_levo, col="green", length=0.1 )

legend("topright", legend = c("Monte Carlo", "analiza modela"),
       col = c("green", "red"), cex=0.8, bty="n", lty=c("solid","dashed"))

#histogram N3
pov.N3 <- mean(N3) #povprecje N3
odklon.N3 <- sqrt(var(N3)) #standardni odklon N3
x3_odklon_desno <- cena_binomske + odklon.N3
x3_odklon_levo <- cena_binomske - odklon.N3


histogram3 <-hist(N3,main = "Monte Carlo: N=1000", col ="yellow",
                  xlab = "Premija", xlim = c(0, 12))
abline(v= pov.N3, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0=0, x1 = x3_odklon_desno, col="green", length=0.1 )
arrows(x0 = cena_binomske, y0=0, x1 = x3_odklon_levo, col="green", length=0.1 )
legend("topright", legend = c("Monte Carlo", "Analiza modela"),
       col = c("green", "red"), cex=0.8, bty="n", lty=c("solid","dashed"))
