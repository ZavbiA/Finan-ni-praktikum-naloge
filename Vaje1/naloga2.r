#2.naloga

#Najprej je treba zagnati 1.nalogo.

maj2014 <- ts(skupna_tabela[c(5),], start=c(0,1), end = c(12,1), frequency = 1)
april2015 <- ts(skupna_tabela[c(16),], start=c(0,1), end = c(12,1), frequency = 1)
avgust2016 <- ts(skupna_tabela[c(32),], start=c(0,1), end = c(12,1), frequency = 1)
ts.plot(maj2014, april2015, avgust2016, gpars = list(xlab="Dospetje [mesec]", ylab="%"), col=c("orange","purple", "green"), type="o")
title("Casovna struktura Euribor")

#popravi časovne enote (imamo tedne in mesece, start/end/frequency)