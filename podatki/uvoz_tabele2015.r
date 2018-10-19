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

tabela2015 <- t(tabela2015)
