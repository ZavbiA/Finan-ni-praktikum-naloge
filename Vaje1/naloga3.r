#3.naloga

#(a) Izracun terminskih obrestnih mer

vse_terminske <- c()

for (i in 1:nrow(skupna_tabela)){
  vse_terminske[i] <- (1/(12-6))*((1+12*skupna_tabela[c(i),c(8)])/(1+6*skupna_tabela[c(i),c(6)])-1)}

View(vse_terminske)

#(b) Naredim tabelo primerjav

terminska <- c("NA", "NA","NA", "NA","NA", "NA")
for (i in 1:(nrow(skupna_tabela)-6)){
  terminska[i+6] <- vse_terminske[i]
}

Euribor6m <- c(skupna_tabela[,c(6)])
Euribor12m <- c(skupna_tabela[,c(8)])
Napoved6m <- c(skupna_tabela[,c(0)],terminska)

Euribor6m <- as.table(Euribor6m)
Euribor12m <- as.table(Euribor12m)

tabela_napovedi <- cbind(Euribor6m, Euribor12m, Napoved6m)
View(tabela_napovedi)
