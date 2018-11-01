library(rvest) 
library(gsubfn) 
library(readr) 
library(dplyr) 
library(tibble)
library(reshape2)
library(gsubfn)
library(tidyr)
require(graphics)
library(ggplot2)

#uvoz tabele 2014

tabela2014 <- read.csv("Vaje1/podatki_2014.csv")

tabela2014[237:256] <- NULL
tabela2014[217:235] <- NULL
tabela2014[194:215] <- NULL
tabela2014[172:192] <- NULL
tabela2014[151:170] <- NULL
tabela2014[128:149] <- NULL
tabela2014[107:126] <- NULL
tabela2014[86:105] <- NULL
tabela2014[66:84] <- NULL
tabela2014[45:64] <- NULL
tabela2014[25:43] <- NULL
tabela2014[3:23] <- NULL

#uvoz tabele 2015

tabela2015 <- read.csv("Vaje1/podatki_2015.csv")

tabela2015[237:257] <- NULL
tabela2015[216:235] <- NULL
tabela2015[194:214] <- NULL
tabela2015[172:192] <- NULL
tabela2015[151:170] <- NULL
tabela2015[128:149] <- NULL
tabela2015[106:126] <- NULL
tabela2015[86:104] <- NULL
tabela2015[66:84] <- NULL
tabela2015[44:64] <- NULL
tabela2015[24:42] <- NULL
tabela2015[3:22] <- NULL

#uvoz tabele 2016

tabela2016 <- read.csv("Vaje1/podatki_2016.csv")

tabela2016[239:258] <- NULL
tabela2016[217:237] <- NULL
tabela2016[196:215] <- NULL
tabela2016[174:194] <- NULL
tabela2016[151:172] <- NULL
tabela2016[130:149] <- NULL
tabela2016[108:128] <- NULL
tabela2016[86:106] <- NULL
tabela2016[65:84] <- NULL
tabela2016[44:63] <- NULL
tabela2016[23:42] <- NULL
tabela2016[3:21] <- NULL

#Zdruzim v skupno tabelo.

skupna_tabela <- t(cbind(tabela2014[,-1],tabela2015[,-1],tabela2016[,-1]))
colnames(skupna_tabela) <- c("1w","2w","1m","2m","3m","6m","9m","12m")

#Narisem graf.

obresti_6m <- ts(skupna_tabela[,"6m"], start=c(2014,1), frequency=12)
obresti_12m <- ts(skupna_tabela[,"12m"], start=c(2014,1), frequency=12)
ts.plot(obresti_6m, obresti_12m, gpars = list(xlab="Time", ylab="%"), col=c("orange","purple"), lwd=2)
title("Euribor")
legend("topright", legend = c("6m", "12m"), lwd=2, bty="n", col = c("orange", "purple"), cex = 1)

#2.NALOGA

#Najprej naredim tabelo za graf po datumih.
casovne_enote <- c(0.23, 0.45, 1, 2, 3, 6, 9, 12)
datumi <- as.data.frame((skupna_tabela[5,]))
colnames(datumi) <- c("maj2014")
datumi["maj2015"] <- as.data.frame((skupna_tabela[17,]))
datumi["avgust2016"] <- as.data.frame((skupna_tabela[32,]))
datumi["casovne_enote"] <- casovne_enote

#Nato narisem graf.
ggplot() +
  geom_point(data=datumi, aes(x=datumi$casovne_enote,y=datumi$maj2014), colour="purple") +
  geom_line(data=datumi, aes(x=datumi$casovne_enote,y=datumi$maj2014), colour="purple") +
  geom_text(aes(x=11, y=0.67), label="2.5.2014", colour="purple") +
  geom_point(data=datumi, aes(x=datumi$casovne_enote,y=datumi$maj2015), colour="orange") +
  geom_line(data=datumi, aes(x=datumi$casovne_enote,y=datumi$maj2015), colour= "orange") +
  geom_text(aes(x=11, y=0.22), label="4.5.2015", colour="orange") +
  geom_point(data=datumi, aes(x=datumi$casovne_enote,y=datumi$avgust2016), colour="darkgreen") +
  geom_line(data=datumi, aes(x=datumi$casovne_enote,y=datumi$avgust2016), colour="darkgreen") +
  geom_text(aes(x=11, y=-0.17), label="1.8.2016", colour="darkgreen") +
  labs(x="Dospetje [mesec]", y="%") + ggtitle("Casovna struktura Euribor") +
  theme(plot.title = element_text(hjust = 0.5))

# 2.5.2014 obrestne mere najbolj konstantno naraščajo.
# Zanimiva je primerjava z drugima dvema datumoma,
# pri katerih gre za drugačne trende.
# 4.5.2015 je nekoliko večji skok pri obrestnih merah za krajše obdobje,
# naprej pa so veliko bolj konstantne.
# 1.8.2016 pa je ravno obratno - obrestne mere za krajše obdobje
# so si bolj podobne, nato pa hitreje naraščajo.

#3.NALOGA

#(a) Izracun terminskih obrestnih mer

library(gsubfn)

vse_terminske <- c()

for (i in 1:nrow(skupna_tabela)){
  vse_terminske[i] <- (1/6)*(((1+12*skupna_tabela[c(i),c(8)])/(1+6*skupna_tabela[c(i),c(6)]))-1)}


#(b) Naredim tabelo primerjav.

terminska <- c(NA,NA,NA,NA,NA,NA)
for (i in 1:(nrow(skupna_tabela)-6)){
  terminska[i+6] <- vse_terminske[i]
}

Euribor6m <- c(skupna_tabela[,c(6)])
Euribor12m <- c(skupna_tabela[,c(8)])
Napoved6m <- c(skupna_tabela[,c(0)],terminska)

Euribor6m <- as.table(Euribor6m)
Euribor12m <- as.table(Euribor12m)

tabela_napovedi <- cbind(Euribor6m, Euribor12m, Napoved6m)
tabela_napovedi <- as.data.frame(tabela_napovedi) #To je rešitev (b) naloge.

tabela_napovedi$leto[1:12] <- "2014" #dodam stolpec z letom (za legendo)
tabela_napovedi$leto[13:24] <- "2015"
tabela_napovedi$leto[25:36] <- "2016"

#Razdelim tabelo na 3 tabele po letih (za d nalogo).
primerjava2014 <- tabela_napovedi[1:12,]
primerjava2015 <- tabela_napovedi[13:24,]
primerjava2016 <- tabela_napovedi[25:36,]

#(c) Narisem razsevni grafikon.

ggplot(tabela_napovedi, aes(x=Napoved6m, y=Euribor6m, colour=leto)) +
  geom_point(size=2) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2014-2016") +
  geom_smooth(method="lm", colour="black", size=0.5, se=FALSE) +
  xlim(-0.22,1.5) + ylim(-0.22,1.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         shape = guide_legend(order=3)) +
  theme(legend.position = c(0.15,0.7))


#(d) Uporabim tabele po letih in narisem locene grafe.

#2014
ggplot() +
  geom_point(data=primerjava2014, colour="red", size=2,
             aes(x=primerjava2014$Napoved6m, y=primerjava2014$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2014") +
  geom_smooth(data=primerjava2014, method="lm", colour="red", size=1, se=FALSE,
              aes(x=primerjava2014$Napoved6m, y=primerjava2014$Euribor6m)) +
  xlim(0,0.32) + ylim(0,0.32) +
  theme(plot.title = element_text(hjust = 0.5))

#2015
ggplot() +
  geom_point(data=primerjava2015, colour="yellow", size=2,
             aes(x=primerjava2015$Napoved6m, y=primerjava2015$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2015") +
  geom_smooth(data=primerjava2015, method="lm", colour="yellow", size=1, se=FALSE,
              aes(x=primerjava2015$Napoved6m, y=primerjava2015$Euribor6m)) +
  xlim(-0.05,0.24) + ylim(-0.05,0.24) +
  theme(plot.title = element_text(hjust = 0.5))

#2016
ggplot() +
  geom_point(data=primerjava2016, colour="green", size=2,
             aes(x=primerjava2016$Napoved6m, y=primerjava2016$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2016") +
  geom_smooth(data=primerjava2016, method="lm", colour="green", size=1, se=FALSE,
              aes(x=primerjava2016$Napoved6m, y=primerjava2016$Euribor6m)) +
  xlim(-0.225,1.45) + ylim(-0.225,1.45) +
  theme(plot.title = element_text(hjust = 0.5))

#(e)
# Za potrditev hipoteze bi se podatki morali bolj prilegati simetrali
# lihih kvadrantov, vendar se ji v našem primeru ne najbolje.
# Tudi po regresijski premici se vidi, da napovedane obrestne mere zelo odstopajo
# od realnih, saj se premica zelo razlikuje od simetrale.
# Še najbolj se približa leto 2014, v letu 2016 pa so odstopanja zelo velika.
# Empirični podatki ne potrjujejo hipoteze pričakovanj trga, morda le delno.
