#2.naloga

#Najprej je treba zagnati 1.nalogo.
require(graphics)

maj2014 <- ts(skupna_tabela[c(5),], start=0, end = 12, frequency = )
april2015 <- ts(skupna_tabela[c(16),], start=0, end = 12, frequency = )
avgust2016 <- ts(skupna_tabela[c(32),], start=0, end = 12, frequency = )
ts.plot(maj2014, april2015, avgust2016, gpars = list(xlab="Dospetje [mesec]", ylab="%"),
        col=c("orange","purple", "green"), type="o")
legend("topright", legend = c("2.5.2014", "1.4.2015", "1.8.2016"), bty="n", text.col = c("orange", "purple", "green"))
title("Casovna struktura Euribor")

#popravi časovne enote (imamo tedne in mesece, start/end/frequency)