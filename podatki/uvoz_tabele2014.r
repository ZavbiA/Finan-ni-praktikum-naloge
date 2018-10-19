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

tabela2014 <- t(tabela2014)
