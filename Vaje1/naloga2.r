#2.naloga
#Daj vse 3.naloge v skupno datoteko.
#Najprej je treba zagnati 1.nalogo.
require(graphics)
library(ggplot2)

#Najprej naredim tabelo za graf po datumih.
casovne_enote <- c(0.23, 0.45, 1, 2, 3, 6, 9, 12)
datumi <- as.data.frame((skupna_tabela[5,]))
colnames(datumi) <- c("maj2014")
datumi["maj2015"] <- as.data.frame((skupna_tabela[17,]))
datumi["avgust2016"] <- as.data.frame((skupna_tabela[32,]))
datumi["casovne_enote"] <- casovne_enote

#Nato narišem graf.
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
  labs(x="Dospetje [mesec]", y="%") + ggtitle("Casovna struktura Euribor")

#2.5.2014 obrestne mere najbolj konstantno naraščajo.
#Zanimiva je primerjava z drugima dvema datumoma, pri katerih gre za drugačne trende.
#4.5.2015 je nekoliko večji skok pri obrestnih merah za krajše obdobje, naprej pa so veliko bolj konstantne.
#1.8.2016 pa je ravno obratno - obrestne mere za krajše obdobje so si bolj podobne, nato pa hitreje naraščajo.
