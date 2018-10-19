library(rvest) 
library(gsubfn) 
library(readr) 
library(dplyr) 
library(tibble)
library(reshape2)
library(gsubfn)
library(tidyr)

#uvoz tabele 2014

tabela2014 <- read_csv("podatki_2014.csv",
                       locale = locale(encoding = "UTF-8"))

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

tabela2015 <- read_csv("podatki_2015.csv",
                       locale = locale(encoding = "UTF-8"))

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

tabela2016 <- read_csv("podatki_2016.csv",
                       locale = locale(encoding = "UTF-8"))

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

#Združim v skupno tabelo.

skupna_tabela <- t(cbind(tabela2014,tabela2015,tabela2016))
skupna_tabela <- skupna_tabela[-c(1,14,27), ]
colnames(skupna_tabela) <- c("1w","2w","1m","2m","3m","6m","9m","12m")

#Narišem graf.

obresti_6m <- ts(skupna_tabela[,"6m"], start=c(2014,1), frequency=12)
obresti_12m <- ts(skupna_tabela[,"12m"], start=c(2014,1), frequency=12)
ts.plot(obresti_6m, obresti_12m, gpars = list(xlab="Time", ylab="%"), col=c("orange","purple"))
title("Euribor")
legend("topright", legend=c("6m", "12m"), fill = c("orange","purple"))
