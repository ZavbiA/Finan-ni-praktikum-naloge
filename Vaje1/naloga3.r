#3.naloga

#Najprej je treba zagnati 1.nalogo.

#(a) Izracun terminskih obrestnih mer

vse_terminske <- c()

for (i in 1:nrow(skupna_tabela)){
  vse_terminske[i] <- (1/6)*(((1+12*skupna_tabela[c(i),c(8)])/(1+6*skupna_tabela[c(i),c(6)]))-1)}


#(b) Naredim tabelo primerjav

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
tabela_napovedi <- as.data.frame(tabela_napovedi)

#Razdelim tabelo na 3 tabele po letih.
primerjava2014 <- tabela_napovedi[1:12,]
primerjava2015 <- tabela_napovedi[13:24,]
primerjava2016 <- tabela_napovedi[25:36,]

#(c) Narišem razsevni grafikon.
ggplot() +
  geom_point(data=primerjava2016, colour="green", size=2,
             aes(x=primerjava2016$Napoved6m, y=primerjava2016$Euribor6m)) +
  geom_point(data=primerjava2015, colour="yellow", size=2,
             aes(x=primerjava2015$Napoved6m, y=primerjava2015$Euribor6m)) +
  geom_point(data=primerjava2014, colour="red", size=2,
             aes(x=primerjava2014$Napoved6m, y=primerjava2014$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2014-2016") +
  geom_smooth(data=tabela_napovedi, method="lm", colour="black", size=0.5, se=FALSE,
              aes(x=tabela_napovedi$Napoved6m, y=tabela_napovedi$Euribor6m)) +
  xlim(-0.22,1.5) + ylim(-0.22,1.5) +
  legend("center", legend = c("2014","2015","2016"), col=c("red","yellow","green"), pch=19)

  #geom_text(aes(x=0, y=1.5), label="2014", size=4.5) +
  #geom_text(aes(x=0, y=1.35), label="2015", size=4.5) +
  #geom_text(aes(x=0, y=1.2), label="2016", size=4.5) +
  #annotate(geom = "text", x=0, y=1, label="2014", size=4.5)
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  # guides(fill = guide_legend(title = "Uvrstitev")) +
  # scale_fill_manual(values=c("darkgoldenrod1", "darkgray", "lightsalmon4"))
  #scale_colour_manual(values=c("red","green","blue"))
  #legend("center", legend = c("2014","2015","2016"), col=c("red","yellow","green"))
  #guides(color=guide_legend(), size=guide_legend())
  #theme(legend.position = "bottom")
  #guides(col=guide_legend(nrow=3))

#(d) Uporabim tabele po letih in narišem ločene grafe.

#2014
ggplot() +
  geom_point(data=primerjava2014, colour="red", size=2,
             aes(x=primerjava2014$Napoved6m, y=primerjava2014$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2014") +
  geom_smooth(data=primerjava2014, method="lm", colour="red", size=1, se=FALSE,
              aes(x=primerjava2014$Napoved6m, y=primerjava2014$Euribor6m)) +
  xlim(0,0.32) + ylim(0,0.32)

#2015
ggplot() +
  geom_point(data=primerjava2015, colour="yellow", size=2,
             aes(x=primerjava2015$Napoved6m, y=primerjava2015$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2015") +
  geom_smooth(data=primerjava2015, method="lm", colour="yellow", size=1, se=FALSE,
              aes(x=primerjava2015$Napoved6m, y=primerjava2015$Euribor6m)) +
  xlim(-0.05,0.24) + ylim(-0.05,0.24)

#2016
ggplot() +
  geom_point(data=primerjava2016, colour="green", size=2,
             aes(x=primerjava2016$Napoved6m, y=primerjava2016$Euribor6m)) +
  geom_abline(intercept = 0, slope = 1, lty="dashed") +
  labs(x="Napoved", y="Opazovano") + ggtitle("6m Euribor 2016") +
  geom_smooth(data=primerjava2016, method="lm", colour="green", size=1, se=FALSE,
              aes(x=primerjava2016$Napoved6m, y=primerjava2016$Euribor6m)) +
  xlim(-0.225,1.45) + ylim(-0.225,1.45)

#(e)
#Podatki bi se morali bolj prilegati simetrali lihih kvadrantov,
#vendar se ji v našem primeru ne najbolje.
#Še najbolj se približa leto 2014, v letu 2016 pa je zelo zgrešeno.
# Empirični podatki ne potrjujejo hipoteze pričakovanj trga.