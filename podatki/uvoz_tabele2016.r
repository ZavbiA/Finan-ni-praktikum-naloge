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

tabela2016 <- t(tabela2016)
